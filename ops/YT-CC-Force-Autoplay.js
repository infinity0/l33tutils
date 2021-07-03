// ==UserScript==
// @name         YouTube Chromecast Force Auto-play
// @namespace    https://github.com/infinity0
// @version      0.1
// @description  Chromecast auto-play functionality is often broken. This script tries to brute-force it to work.
// @author       infinity0
// @match        https://www.youtube.com/watch?v=*
// @icon         https://www.google.com/s2/favicons?domain=youtube.com
// @grant        none
// @run-at       document-end
// ==/UserScript==

(function() {
    'use strict';

    let elem = document.getElementsByClassName("ytp-large-play-button")[0];
    new MutationObserver(function(mutations) {
        let display = window.getComputedStyle(elem).display;
        console.log("mutated ytp-large-play-button: display: " + display);
        if (display != "none") {
            console.log("clicking ytp-large-play-button");
            elem.click();
        }
    }).observe(elem, {
        attributes: true,
        characterData: true,
        childList: true,
        subtree: true,
    });
})();
