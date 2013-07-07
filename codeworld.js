(function () {
/*
 * Keys that should be recognized from the JavaScript key property, if
 * and when major browsers ever implement it.
 */
var specialKeys = [
    "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
    "Up", "Down", "Left", "Right", "PageUp", "PageDown", "Home", "End", "Insert",
    "Del", "NumLock"
    ];

/*
 * Keys that should be recognized from the JavaScript keyCode property.
 * These are actually, by some miracle, consistent across browsers.
 */
var specialKeyCodes = [
    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
    38, 40, 37, 39, 33, 34, 36, 35, 45, 46, 144
    ]

/*
 * Since Firefox doesn't deliver key codes with the keypress event, we
 * remember the last code from a keydown event, in the hopes that it
 * can be matched up, and we can deliver the event for releasing a
 * char-type key.
 */
var lastKeyCode;

/*
 * Cached map from key codes to characters.  This is so that when the
 * key is released, we can deliver the right event with the right
 * character even though several browsers don't fill in the charCode
 * property.
 */
var cachedCodes = {};

/*
 * Look up a special key from a key event, and return the identifier
 * string, or null if there is none.
 */
window.getSpecialKey = function(e) {
    lastKeyCode = e.keyCode;

    if (e.key)
    {
        var i = specialKeys.indexOf(e.key);

        if (i == -1) return "None";
        else         return e.key;
    }

    if (e.keyCode)
    {
        var i = specialKeyCodes.indexOf(e.keyCode);

        if (i == -1) return "None";
        else         return specialKeys[i];
    }

    return "None";
};

window.getPressedKey = function(e) {
    if (e.charCode == 0) return "None";

    cachedCodes[lastKeyCode.toString()] = String.fromCharCode(e.charCode);
   	return String.fromCharCode(e.charCode);
};

window.getReleasedKey = function(e) {
    var kname = getSpecialKey(e);
    if (kname != "None") return kname;

    if (e.charCode && e.charCode != 0) return String.fromCharCode(e.charCode);
    if (e.keyCode && cachedCodes.hasOwnProperty(e.keyCode.toString()))
    {
        return cachedCodes[e.keyCode.toString()];
    }
};

window.stopEvent = function(e) {
    if (e.preventDefault)  e.preventDefault();
    if (e.stopPropogation) e.stopPropogation();
    if (e.cancelBubble)    e.cancelBubble();
};

window.mouseToElementX = function(e) {
    return e.clientX - canvas.getBoundingClientRect().left - 250;
};

window.mouseToElementY = function(e) {
    return 250 - e.clientY + canvas.getBoundingClientRect().top;
};

window.mkImage = function(url) {
    var img = new Image();
    img.src = url;
    return img;
};

})();
