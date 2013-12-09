#!/usr/bin/env python

# Copyright (c) Ximin Luo
# Copyright (c) Twisted Matrix Laboratories.
# See LICENSE for details.


"""
Simple read-only IMAP4 REPL.
"""

import argparse
import shlex
import subprocess
import sys

from twisted.internet import protocol
from twisted.internet import ssl
from twisted.internet import defer
from twisted.internet import stdio
from twisted.mail import imap4
from twisted.protocols import basic
from twisted.python import util
from twisted.python import log



class TrivialPrompter(basic.LineReceiver):
    from os import linesep as delimiter

    promptDeferred = None

    def prompt(self, msg):
        assert self.promptDeferred is None
        self.display(msg)
        self.promptDeferred = defer.Deferred()
        return self.promptDeferred

    def display(self, msg):
        self.transport.write(msg)

    def lineReceived(self, line):
        if self.promptDeferred is None:
            return
        d, self.promptDeferred = self.promptDeferred, None
        d.callback(line)



class SimpleIMAP4Client(imap4.IMAP4Client):
    """
    A client with callbacks for greeting messages from an IMAP server.
    """
    greetDeferred = None

    def serverGreeting(self, caps):
        self.serverCapabilities = caps
        if self.greetDeferred is not None:
            d, self.greetDeferred = self.greetDeferred, None
            d.callback(self)



class SimpleIMAP4ClientFactory(protocol.ClientFactory):
    usedUp = False

    protocol = SimpleIMAP4Client


    def __init__(self, username, onConn):
        self.ctx = ssl.ClientContextFactory()

        self.username = username
        self.onConn = onConn


    def buildProtocol(self, addr):
        """
        Initiate the protocol instance. Since we are building a simple IMAP
        client, we don't bother checking what capabilities the server has. We
        just add all the authenticators twisted.mail has.  Note: Gmail no
        longer uses any of the methods below, it's been using XOAUTH since
        2010.
        """
        assert not self.usedUp
        self.usedUp = True

        p = self.protocol(self.ctx)
        p.factory = self
        p.greetDeferred = self.onConn

        p.registerAuthenticator(imap4.PLAINAuthenticator(self.username))
        p.registerAuthenticator(imap4.LOGINAuthenticator(self.username))
        p.registerAuthenticator(
                imap4.CramMD5ClientAuthenticator(self.username))

        return p


    def clientConnectionFailed(self, connector, reason):
        d, self.onConn = self.onConn, None
        d.errback(reason)



def cbServerGreeting(proto, username, password, mbox):
    """
    Initial callback - invoked after the server sends us its greet message.
    """
    # Hook up stdio
    tp = TrivialPrompter()
    stdio.StandardIO(tp)

    # And make it easily accessible
    proto.prompt = tp.prompt
    proto.display = tp.display

    # Try to authenticate securely
    return proto.authenticate(password
        ).addCallback(cbAuthentication, proto, mbox
        ).addErrback(ebAuthentication, proto, username, password
        )


def ebConnection(reason):
    """
    Fallback error-handler. If anything goes wrong, log it and quit.
    """
    log.startLogging(sys.stdout)
    log.err(reason)
    return reason


def cbAuthentication(result, proto, mbox):
    """
    Callback after authentication has succeeded.

    Lists a bunch of mailboxes.
    """
    if mbox:
        return proto.examine(mbox
            ).addCallback(cbExamineMbox, proto
            )
    else:
        return proto.list("", "*"
            ).addCallback(cbMailboxList, proto
            )

def ebAuthentication(failure, proto, username, password):
    """
    Errback invoked when authentication fails.

    If it failed because no SASL mechanisms match, offer the user the choice
    of logging in insecurely.

    If you are trying to connect to your Gmail account, you will be here!
    """
    failure.trap(imap4.NoSupportedAuthentication)
    return defer.fail(Exception("No secure authentication available; abort"))


def cbMailboxList(result, proto):
    """
    Callback invoked when a list of mailboxes has been retrieved.
    """
    result = [e[2] for e in result]
    s = '\n'.join(['%d. %s' % (n + 1, m) for (n, m) in enumerate(result)])
    if not s:
        return defer.fail(Exception("No mailboxes exist on server!"))
    return proto.prompt(s + "\nWhich mailbox? [Quit] "
        ).addCallback(cbPickMailbox, proto, result
        )


def cbPickMailbox(result, proto, mboxes):
    """
    When the user selects a mailbox, "examine" it.
    """
    if result and result != 'q':
        mbox = mboxes[int(result or '1') - 1]
        return proto.examine(mbox
            ).addCallback(cbExamineMbox, proto
            )
    else:
        proto.display("Goodbye\n")
        return proto.logout()


def cbExamineMbox(result, proto):
    """
    Callback invoked when examine command completes.

    Search for recent messages.
    """
    return proto.search(imap4.Query(sorted=1, recent=1)
        ).addCallback(cbRecent, proto
        )


def cbRecent(result, proto):
    """
    Callback invoked when search command completes.

    Retrieve the subject header of every message in the result.
    """
    if result:
        s = imap4.MessageSet(zip(result, result))
        return proto.fetchSpecific(s,
                                   headerType='HEADER.FIELDS',
                                   headerArgs=['FROM', 'SUBJECT', 'DATE'],
            ).addCallback(cbFetch, proto
            )
    else:
        proto.display("No recent messages; Goodbye\n")
        return proto.logout()


def cbFetch(result, proto):
    """
    Display headers, then prompt for message(s) to fetch.
    """
    if result:
        keys = result.keys()
        keys.sort()
        for k in keys:
            proto.display("## %s\n%s" % (k, result[k][0][2]))

        return proto.prompt('Which message(s)? ("1" or "1,2" or "1:3") [Quit] '
            ).addCallback(cbPickMessage, proto
            )
    else:
        proto.display("Empty header result; return to recent.\n")
        return cbExamineMbox(None, proto)


def cbPickMessage(result, proto):
    """
    Fetch a message, then display it.
    """
    if result and result != 'q':
        return proto.fetchMessage(result).addCallback(cbFetchMessage, proto)
    else:
        proto.display("Goodbye\n")
        return proto.logout()


def cbFetchMessage(result, proto):
    """
    Display a message, then loop back to recent messages.
    """
    if result:
        keys = result.keys()
        keys.sort()
        for k in keys:
            proto.display("## %s\n%s\n" % (k, result[k]['RFC822']))

        return cbExamineMbox(None, proto)
    else:
        proto.display("Empty body result; return to recent.\n")
        return cbExamineMbox(None, proto)


def cbClose(result):
    """
    Close the connection when we finish everything.
    """
    from twisted.internet import reactor
    reactor.stop()


def main():
    parser = argparse.ArgumentParser(
        description="""Check your INBOX via IMAP-SSL.

Passwords are obtained via secret-tool(1). You can put this tool, which only
*reads* your email, behind a more restricted form of access, such as an
authenticated web interface, so that you don't need to give an insecure email
client (such as on your smartphone) your account password, and write-access
along with it.
""")
    parser.add_argument('user', metavar='NAME',
        help="user to authenticate as")
    parser.add_argument('host', metavar='HOST',
        help="host to connect to")
    parser.add_argument('port', metavar='PORT',
        help="port to connect to",
        nargs='?', default=993, type=int)
    parser.add_argument('-m', '--mbox', metavar='MBOX',
        help="IMAP mbox (i.e. folder/directory) to check, default %(default)s. "
        "set empty to list all mailboxes and then prompt for one to use.",
        default="INBOX")
    parser.add_argument('-s', '--secret', metavar='ARGS',
        help="query for `secret-tool(1) lookup`, parsed with shlex in POSIX "
        "mode. If empty, we use \"hostname imap://$host username $user\", "
        "which is compatible with passwords stored via mozilla-gnome-keyring.")

    args = parser.parse_args()
    secret = args.secret or "hostname imap://%s username %s" % (args.host, args.user)
    password = subprocess.check_output(["secret-tool", "lookup"] + shlex.split(secret))

    onConn = defer.Deferred()
    onConn = onConn.addCallbacks(cbServerGreeting, ebConnection, callbackArgs=(args.user, password, args.mbox))
    onConn = onConn.addBoth(cbClose)

    factory = SimpleIMAP4ClientFactory(args.user, onConn)

    from twisted.internet import reactor
    reactor.connectSSL(args.host, args.port, factory, ssl.ClientContextFactory())
    reactor.run()


if __name__ == '__main__':
    main()
