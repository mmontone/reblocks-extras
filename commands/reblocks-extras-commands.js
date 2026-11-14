function reblocksAddClass(params) {
    var target = jQuery('#' + params.domId);
    if (params.path) {
        target = target.find(params.path);
    }
    target.addClass(params.cssClass);
}

function reblocksRemoveClass(params) {
    var target = jQuery('#' + params.domId);
    if (params.path) {
        target = target.find(params.path);
    }
    target.removeClass(params.cssClass);
}

function reblocksToggleClass(params) {
    var target = jQuery('#' + params.domId);
    if (params.path) {
        target = target.find(params.path);
    }
    target.toggleClass(params.cssClass);
}

function reblocksPrompt(params) {
    const res = window.prompt(params.message);
    if (res == null || res == undefined) return;
    initiateActionWithArgs(params.actionCode, null, {
        'response': res
    });
}

function reblocksConfirm(params) {
    const res = window.confirm(params.message);
    initiateActionWithArgs(params.actionCode, null, {
        'response': res
    });
}

function reblocksAlert(params) {
    const res = window.alert(params.message);
    initiateActionWithArgs(params.actionCode, null, {
        'response': res
    });
}

jQuery(function () {
    window.commandHandlers['addClass'] = reblocksAddClass;
    window.commandHandlers['removeClass'] = reblocksRemoveClass;
    window.commandHandlers['toggleClass'] = reblocksToggleClass;
    window.commandHandlers['prompt'] = reblocksPrompt;
    window.commandHandlers['confirm'] = reblocksConfirm;
    window.commandHandlers['alert'] = reblocksAlert;
});
