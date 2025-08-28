function reblocksNotify(params) {
    if (params['target']) {
        jQuery(params['target']).notify({
            params['message'],
            params['style'] ,
            params['options']
        });
    }
    else {
        jQuery.notify({
            params['message'],
            params['style'] ,
            params['options']
        })
    }    
}

window.commandHandlers['notify'] = reblocksNotify;
