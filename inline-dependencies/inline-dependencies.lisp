(defpackage :reblocks/inline-dependencies
  (:use :cl)
  (:import-from #:reblocks/app #:defapp)
  (:import-from #:reblocks/html #:with-html)
  (:export
   #:inline-dependencies
   #:inline-dependency))

(in-package :reblocks/inline-dependencies)

(defclass inline-dependency (reblocks/dependencies:dependency)
  ((name :initarg :name
         :reader dependency-name
         :initform (error "Provide a name for the dependency")
         :type string)
   (source :initarg :source
           :reader dependency-source
           :initform (error "Provide dependency source")
           :type string))
  (:default-initargs
   :type (error "Provide dependency type.")))

(defmethod print-object ((dependency inline-dependency) stream)
  (print-unreadable-object (dependency stream :type t)
    (format stream "~a" (dependency-name dependency))))

(defmethod reblocks/dependencies:get-url ((dependency inline-dependency))
  ;; Use name as "url"
  (dependency-name dependency))

(defmethod reblocks/dependencies:render-in-head ((dependency inline-dependency))
  (case (reblocks/dependencies:get-type dependency)
    ;; Javascript
    (:js
     (with-html ()
       (:script :type "text/javascript"
                (dependency-source dependency))))
    ;; CSS
    (:css
     (with-html ()
       (:style
        (dependency-source dependency))))))

(defmethod reblocks/dependencies:render-in-ajax-response ((dependency inline-dependency))
  (case (reblocks/dependencies:get-type dependency)
    (:js
     (let ((script (ps:ps* `(insert_script ,(dependency-name dependency) ,(dependency-source dependency)))))
       (log:debug "Rendering js dependency in ajax response" dependency)
       (reblocks/response:send-script script :before-load)))
    (:css
     (let ((script (ps:ps* `(insert_style ,(dependency-name dependency) ,(dependency-source dependency)))))
       (log:debug "Rendering css dependency in ajax response" dependency)
       (reblocks/response:send-script script :before-load)))))

;; define an application that loads code necessary for inline dependencies

(defapp inline-dependencies
  :autostart nil)

(defmethod reblocks/dependencies:get-dependencies ((app inline-dependencies))
  (list* (make-instance 'inline-dependency
                       :type :js
                       :name "inline-dependencies.js"
                       :source
                       "function insert_script(script_name, script_source) {
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
}")
         (call-next-method)))
