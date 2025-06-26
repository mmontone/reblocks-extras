(defpackage :reblocks/examples-browser
  (:use :cl)
  (:import-from #:log)
  (:import-from #:find-port
                #:find-port)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/widget
                #:widget #:defwidget #:render #:update)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/routes
                #:page)
  (:export #:add-example
           #:define-example
           #:open-browser
           #:stop-browser))

(in-package :reblocks/examples-browser)

(defvar *examples* '())

(defstruct example
  name builder description category)

(defmacro define-example (name options &body body)
  `(add-example ,name (lambda () ,@body) ,@options))

(defun make-js-value-action (action &optional (js-source "this.value"))
  "Returns JS code which can be inserted into `onchange` attribute and will
execute given Lisp function when HTML input changes, passing its string value as parameter.

It accepts any function as input and produces a string with JavaScript code.
JS-SOURCE should be a javascript string that is evaluated client side
and passed as argument to ACTION.
The javascript value is accessible in &key value in the callback.

Example:
   (:input :type \"text\"
           :onchange
               (make-js-value-action
                   (lambda (&key value &allow-other-keys)
                       (setf (input-value widget) value))))
"
  (let* ((action-code (reblocks/actions:make-action action)))
    (format nil "return initiateAction(\"~a\", {\"args\": {\"value\": ~a}})"
            action-code js-source)))

(defun add-example (name widget-builder &key description category)
  (check-type name string)
  (check-type widget-builder trivial-types:function-designator)
  (serapeum:push-end (make-example :name name
                                   :builder widget-builder
                                   :description description
                                   :category category)
                     *examples*))

(defun find-example (name &optional examples-browser)
  (or (find name (if examples-browser
                     (examples examples-browser)
                     *examples*)
            :key #'example-name
            :test #'string=)
      (error "Example not found: ~s" name)))

(defwidget examples-browser ()
  ((examples :initarg :examples)
   (selected-example
    :initarg :selected-example
    :accessor selected-example
    :initform nil
    :type (or null string))))

(defmethod examples ((browser examples-browser))
  (if (slot-boundp browser 'examples)
      (slot-value browser 'examples)
      *examples*))

(defmethod render ((browser examples-browser))
  (with-html ()
    (:html
     (:head
      (:title "Reblocks Examples Browser")
      (:link :href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" :rel "stylesheet" :integrity "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC" :crossorigin "anonymous"))
     (:body
      (:div :class "container"
            (:div :class "row"
                  (:div :class "col"
                        (:select
                            :onchange (make-js-value-action (lambda (&key value &allow-other-keys)
                                                              (setf (selected-example browser) value)
                                                              (update browser)))
                          (:option "-- Select example --")
                          (dolist (example (examples browser))
                            (:option :value (example-name example)
                                     (example-name example)))))
                  (:div :class "col"
                        (if (not (selected-example browser))
                            (:h3 "Select an example")
                            (let* ((example (find-example (selected-example browser) browser))
                                   (widget (funcall (example-builder example))))
                              (render widget))))))
      (:script :src "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js" :integrity "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM" :crossorigin"anonymous")
      ))))

(defapp examples-browser-app
  :prefix "/"
  :autostart nil
  :routes ((page ("/" :name "examples-browser")
             (make-instance 'examples-browser))))

(defvar *server*)

(defun open-browser (&key (port (find-port)))
  (setf *server*
        (reblocks/server:start :port port
                               :apps 'examples-browser-app))

  (let ((url (format nil "http://localhost:~A" port)))
    (log:info "Opening a web browser to look at ~A" url)
    (trivial-open-browser:open-browser url)
    *server*))

(defun stop-browser ()
  (reblocks/server:stop
   (reblocks/server:get-interface *server*)
   (reblocks/server:get-port *server*)))

(eval-when (:load-toplevel :execute)
  (add-example "Hello World"
               (lambda ()
                 (REBLOCKS/WIDGETS/STRING-WIDGET:MAKE-STRING-WIDGET "Hello world!"))))
