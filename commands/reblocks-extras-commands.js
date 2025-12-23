function reblocksAddClass(params) {
    var target = jQuery('#' + params.domId);
    target.addClass(params.cssClass);
}

function reblocksRemoveClass(params) {
    var target = jQuery('#' + params.domId);
    target.removeClass(params.cssClass);
}

function reblocksRemoveClass(params) {
    var target = jQuery('#' + params.domId);
    target.toggleClass(params.cssClass);
}

jQuery(function () {
    window.commandHandlers['addClass'] = reblocksAddClass;
    window.commandHandlers['removeClass'] = reblocksRemoveClass;
    window.commandHandlers['toggleClass'] = reblocksToggleClass;
});
