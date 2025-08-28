function reblocksNotify(params) {
    if (params['target']) {
        jQuery(params['target']).notify(
            params['message'],
            params['options']
        );
    }
    else {
        jQuery.notify(
            params['message'],
            params['options']
        )
    }
}

window.commandHandlers['notify'] = reblocksNotify;
