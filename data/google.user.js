// ==UserScript==
// @name          Google Search Tweaks
// @namespace     http://infinity0.ath.cx/
// @description   Various tweaks to google search
// @version       0.1.2010.07.25.1
// @include       http://www.google.*/*
// @include       https://www.google.*/*
// @include       https://encrypted.google.*/*
// ==/UserScript==

GT_CLEANUP_INTERVAL = 500; // interval in milliseconds
GT_CLEANUP_MAX_ITER = 40;  // maximum number of iterations to run before giving up
document._GT_cleanup_done = 0;
document._GT_cleanup_iter = 0;

document._GT_cleanup_href = function(s, n) {
	var i = s.indexOf(n + '=');
	if (i < 0) { return null; }

	var u = s.substring(i + n.length + 1);
	u = u.substring(0, u.indexOf('&'));

	return (u.substring(0, 4) == "http")? unescape(u): null;
}

document._GT_cleanup = function() {

	var a = document.getElementsByTagName('a');
	if (a.length == 0) {
		// google does some weird shit with iframes. the second time you use the search box,
		// it will load (a page with an empty body and javascript head) into an iframe. the
		// userscript runs in this iframe, so we must clean the parent document instead
		a = top.document.getElementsByTagName('a');
		//alert("cleanup running from child iframe");
	}

	for (var i=0; i<a.length; ++i) {
		var e = a[i];

		// cleanup URL rewriter
		if (e.hasAttribute('onmousedown')) {
			e.setAttribute('onmousedown', null);
			++document._GT_cleanup_done;
		}

		// cleanup already-rewritten URLs
		var s = e.getAttribute('href');
		if (s.indexOf('/url?') >= 0) {
			var u = document._GT_cleanup_href(s, 'url');
			if (u == null) { u = document._GT_cleanup_href(s, 'q'); }
			if (u == null) { continue; }

			e.setAttribute('href', u);
			++document._GT_cleanup_done;
		}
	}

	++document._GT_cleanup_iter;

	// clear interval if cleanup was successful
	if (document._GT_cleanup_done > 0 || document._GT_cleanup_iter >= GT_CLEANUP_MAX_ITER) {
		window.clearInterval(document._GT_cleanup_intv);
		//alert("cleanup stopped");
	}/* else {
		var head = document.wrappedJSObject.childNodes[1].firstChild;
		var body = head.nextSibling;
		debug = "cancel cleanup? " + a.length + " " + document._GT_cleanup_done + "\n" + document.wrappedJSObject.location;
		debug += "\n" + head + " " + head.childNodes.length + "\n" + head.innerHTML.substring(0,1024);
		debug += "\n" + body + " " + body.childNodes.length + "\n" + body.innerHTML.substring(0,1024);
		if (confirm(debug)) {
			window.clearInterval(document._GT_cleanup_intv);
		}
	}*/

}

//alert("cleanup started...");
document._GT_cleanup_intv = window.setInterval(document._GT_cleanup, GT_CLEANUP_INTERVAL);

