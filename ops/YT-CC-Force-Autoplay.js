// ==UserScript==
// @name         YouTube Chromecast Force Autoplay
// @namespace    https://github.com/infinity0
// @version      0.1
// @description  Chromecast autoplay functionality is often broken. This script brute-forces it to work.
// @author       infinity0
// @match        https://www.youtube.com/*
// @icon         https://www.google.com/s2/favicons?domain=youtube.com
// @grant        none
// ==/UserScript==

(function() {
    'use strict';

    let obsConfig = {
        attributes: true,
        childList: true,
        subtree: true,
    };
    let _log = function(x) {
        console.log("yt-cc-f-ap: " + x);
    }
    let trackedPairs = [];
    new MutationObserver(function(_) {
        let elems = Array.from(document.getElementsByClassName("ytp-large-play-button"));
        let oldPairs = trackedPairs.filter(p => !elems.includes(p.elem));
        trackedPairs = trackedPairs.filter(p => elems.includes(p.elem));
        let newElems = elems.filter(e => trackedPairs.find(p => p.elem == e) === undefined);
        for (const p of oldPairs) {
            _log("obs: release old");
            p.obs.disconnect();
        }
        for (const elem of newElems) {
            _log("obs: observe new");
            let obs = new MutationObserver(function(mutations) {
                // note, d.getElByClassName(".html5-main-video")[0].paused is always true
                // even when not paused on chromecast, because the in-browser player is technically paused
                // so, instead we get the paused status by looking at the small play button, which toggles
                let playToggle = document.getElementsByClassName("ytp-play-button");
                if (!playToggle) return;
                let isPaused = playToggle[0].getAttribute("aria-label").indexOf("Play") >= 0;
                _log("observed small-play-button; video-paused: " + isPaused);
                if (isPaused) {
                    _log("clicking large-play-button");
                    elem.click();
                }
            }).observe(elem, obsConfig);
            trackedPairs.push({elem: elem, obs: obs});
        }
    }).observe(document.querySelector("body"), obsConfig);
})();
