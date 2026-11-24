function insertAtIndex($parent, $element, index) {
    const $children = $parent.children();

    if (index <= 0 || $children.length === 0) {
        $parent.prepend($element);
    } else if (index >= $children.length) {
        $parent.append($element);
    } else {
        $children.eq(index).before($element);
    }
}

function reblocksInsertChild(params) {
    var target = jQuery('#' + params.domId);
    if (typeof params.path === 'string') {
        target = target.find(params.path);
    }

    insertAtIndex(target, jQuery(params.html), params.index);
}

function reblocksRemoveChild(params) {
    var target = jQuery('#' + params.domId);
    if (typeof params.path === 'string') {
        target = target.find(params.path);
    }

    target.children().eq(params.index).remove();
}

function reblocksAddClass(params) {
    var target = jQuery('#' + params.domId);
    if (typeof params.path === 'string') {
        target = target.find(params.path);
    }
    target.addClass(params.cssClass);
}

function reblocksRemoveClass(params) {
    var target = jQuery('#' + params.domId);
    if (typeof params.path === 'string') {
        target = target.find(params.path);
    }
    target.removeClass(params.cssClass);
}

function reblocksToggleClass(params) {
    var target = jQuery('#' + params.domId);
    if (typeof params.path === 'string') {
        target = target.find(params.path);
    }
    target.toggleClass(params.cssClass);
}

function reblocksSetAttribute(params) {
    var target = jQuery('#' + params.domId);
    if (typeof params.path === 'string') {
        target = target.find(params.path);
    }
    target.attr(params.attribute, params.value);
}

function reblocksPrompt(params) {
    const res = window.prompt(params.message);
    if (res == null || res == undefined) return;
    initiateAction(params.actionCode, {
        args: {
            'response': res
        }
    });
}

function reblocksConfirm(params) {
    const res = window.confirm(params.message);
    initiateAction(params.actionCode, {
        args: {
            'response': res
        }
    });
}

function reblocksAlert(params) {
    const res = window.alert(params.message);
    initiateAction(params.actionCode, {
        args: {
            'response': res
        }
    });
}

window.addEventListener('DOMContentLoaded', function () {
    window.commandHandlers['addClass'] = reblocksAddClass;
    window.commandHandlers['removeClass'] = reblocksRemoveClass;
    window.commandHandlers['toggleClass'] = reblocksToggleClass;
    window.commandHandlers['setAttribute'] = reblocksSetAttribute;
    window.commandHandlers['insertChild'] = reblocksInsertChild;
    window.commandHandlers['removeChild'] = reblocksRemoveChild;
    window.commandHandlers['prompt'] = reblocksPrompt;
    window.commandHandlers['confirm'] = reblocksConfirm;
    window.commandHandlers['alert'] = reblocksAlert;
});
