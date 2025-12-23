jQuery(function () {
    window.commandHandlers['updateHistory'] =
        function(params) {
            switch(params.operation) {
                case 'pushState':
                    let historyState = {
                        //location: document.location,
                        state: JSON.parse(params.state),
                        popActionCode: params.popActionCode
                    };
                    history.pushState(historyState, '', params.url);
                    break;
                case 'back':
                    history.back();
                    break;
                case 'forward':
                    history.forward();
                    break;
            }
        };
});

window.addEventListener("popstate", (event) => {
    let historyState = event.state;
    if (historyState?.popActionCode) {
        initiateAction(historyState.popActionCode, {
            args: {
                location: document.location,
                state: JSON.stringify(historyState.state)
            }
        });
    }
});
