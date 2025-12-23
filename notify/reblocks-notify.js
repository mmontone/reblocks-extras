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

jQuery(function () {
    window.commandHandlers['notify'] = reblocksNotify;
});
