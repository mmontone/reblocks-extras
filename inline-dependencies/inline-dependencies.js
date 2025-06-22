function insert_script(script_name, script_source) {
    if (!window.loadedDependencies.includes(script_name)) {
        var html_doc = document.getElementsByTagName('head').item(0);
        var js = document.createElement('script');
        js.setAttribute('language', 'javascript');
        js.setAttribute('type', 'text/javascript');
        js.textContent = script_source;
        html_doc.appendChild(js);
        window.loadedDependencies.push(script_name);
    }
    return false;
}
window.loadedStyles = [];
function insert_style(name, source) {
    if (!window.loadedStyles.includes(name)) {
        var html_doc = document.getElementsByTagName('head').item(0);
        var css = document.createElement('style');
        css.textContent = source;
        html_doc.appendChild(css);
        window.loadedStyles.push(name);
    }
    return false;
}
