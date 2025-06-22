var reblocksWidgetSelectionStarted = false;
var selectionAction;

function inspectWidget(id, widget) {
    jQuery.get('/inspector/inspect/' + id);
    console.log('Inspecting: ', widget);
}

function startReblocksInspector() {
    reblocksWidgetSelectionStarted = true;
    selectionAction = inspectWidget;
}

function startReblocksWidgetUpdater() {
    reblocksWidgetSelectionStarted = true;
    selectionAction = updateWidget;
}

function stopReblocksWidgetSelection() {
    reblocksWidgetSelectionStarted = false;
    jQuery('.widget').css('border', 'inherit');
}

function resetReblocksSession() {
    console.log('Resetting Reblocks session');
    jQuery.get('/inspector/resetsession').always(function () {
        location.reload();
    });
}

function startWidgetSelection(action) {
    reblocksInspectorEnabled = true;
    selectionAction = action;
}

function updateWidget(id, widget) {
    jQuery.get('/inspector/update/' + id);
    console.log('Updating: ', widget);
}

jQuery(function () {

    // Add an inspect button

    let tools = jQuery('<div class="reblocks-tools" style="position: fixed; right: 0; top: 0;"></div>');
    let inspectButton = jQuery('<button type="button" title="Inspect widget">🔍</button>');
    inspectButton.click(startReblocksInspector);

    let updateWidgetButton = jQuery('<button type="button" title="Update widget">🔂</button>');
    updateWidgetButton.click(startReblocksWidgetUpdater);

    let resetSessionButton = jQuery('<button type="button" title="Reset session">🔁</button>');
    resetSessionButton.click(resetReblocksSession);

    tools.append(inspectButton);
    tools.append(updateWidgetButton);
    tools.append(resetSessionButton);

    jQuery('body').append(tools);

    jQuery('body').on('click', '.widget',
                      function(ev) {
                          if (reblocksWidgetSelectionStarted) {
                              stopReblocksWidgetSelection();
                              let widget = jQuery(ev.currentTarget);
                              selectionAction(widget.attr('id'), widget);
                              selectionAction = null;
                              return false;
                          }
                      });

    jQuery('body').on('mouseenter', '.widget',
                      function (ev) {
                          if (reblocksWidgetSelectionStarted) {
                              jQuery(ev.currentTarget).css('border', '1px solid green');
                              ev.preventDefault();
                          }
                      });

    jQuery('body').on('mouseleave', '.widget',
                      function (ev) {
                          if (reblocksWidgetSelectionStarted) {
                              jQuery(ev.currentTarget).css('border', 'inherit');
                              ev.preventDefault();
                          }
                      });
});
