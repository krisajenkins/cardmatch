/* @flow weak */

window.addEventListener(
    'load',
    function () {
        var app = Elm.fullscreen(Elm.Main, {
            startTime : Date.now()
        });
    },
    false
);
