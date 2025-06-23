(defpackage :reblocks/extras/utils
  (:use :cl)
  (:export #:defwidget*
           #:make-js-value-action
           #:test-session-code
           #:make-js-callback
           #:make-ps-callback
           #:let**
           #:make-js-action*
           #:css-classes))

(in-package :reblocks/extras/utils)

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

(defun test-session-code (func)
  (let* ((reblocks/session::*session* (make-hash-table))
         (reblocks/app::*current-app* (make-instance 'reblocks/app::app-routes :namespace ""))
         (reblocks/page::*current-page* (make-instance 'reblocks/page::page)))
    (funcall func)))

(defun make-js-callback (callback &rest args)
  "Returns a javascript function to call CALLBACK."
  (let ((reblocks/actions::*js-default-action* "initiateAction(\"~A\"~@[, ~A~])"))
    (apply #'reblocks/actions:make-js-action callback args)))

(defun make-ps-callback (callback &rest args)
  "Returns a javascript function to call CALLBACK."
  (let ((action-code (reblocks/actions:make-action callback)))
    (list* 'initiate-action action-code args)))

(defmacro let** (bindings &body body)
  "Parallel let"
  `(let ,(mapcar #'car bindings)
     ,@(loop for binding in bindings
             collect `(setf ,@binding))
     ,@body))

(defmacro make-js-action* (&body body)
  `(make-js-action (lambda (&key &allow-other-keys)
                     ,@body)))

(defun css-classes (&rest classes)
  (str:join #\space (remove nil classes)))


(defparameter *defwidget-options-expanders*
  `((:render . ,(lambda (renderer name)
                  (destructuring-bind ((arg) &body body) (cdr renderer)
                    `(defmethod reblocks/widget:render ((,arg ,name))
                       ,@body))))
    (:css-classes . ,(lambda (css-classes-spec name)
                       (destructuring-bind (css-classes &key (inherit t)) (cdr css-classes-spec)
                         `(defmethod reblocks/widget:get-css-classes ((widget ,name))
                            (declare (ignore widget))
                            ,(if inherit
                                 `(append ',css-classes (call-next-method))
                                 `',css-classes)))))
    (:html-tag . ,(lambda (html-tag name)
                    `(defmethod reblocks/widget:get-html-tag ((widget ,name))
                       (declare (ignore widget))
                       ,(cadr html-tag))))
    (:dependencies . ,(lambda (dependencies name)
                        `(defmethod reblocks/dependencies:get-dependencies ((widget ,name))
                           (list* ,@(mapcar (lambda (dependency-spec)
                                              (destructuring-bind (dependency-class &rest args) dependency-spec
                                                `(make-instance ',dependency-class ,@args)))
                                            (cdr dependencies))
                                  (call-next-method)))))))

(defmacro defwidget* (name direct-superclasses direct-slots &rest options)
  "Widget definition macro on top of REBLOCKS/WIDGET:DEFWIDGET that supports extra options.

Options:
(:renderer (widget) &body body)
Specify the widget renderer method, inline.

(:html-tag <html-tag-keyword>)
HTML tag to use by the widget, via REBLOCKS/WIDGET:GET-HTML-TAG .

(:css-classes css-classes-list &key (inherit t))
CSS classes for the widget, via REBLOCKS/WIDGET:GET-CSS-CLASSES .

(:dependencies &rest (dependency-class &rest initargs))
List of widget dependencies."
  (flet ((custom-option-p (option)
           (member (car option) (mapcar #'car *defwidget-options-expanders*))))
    (if (not (some #'custom-option-p options))
        `(defwidget ,name ,direct-superclasses
           ,direct-slots
           ,@options)
        (let ((custom-options (remove-if #'custom-option-p options)))
          `(progn
             (defwidget ,name ,direct-superclasses
               ,direct-slots
               ,@custom-options)
             ,@(loop for option in options
                     when (custom-option-p option)
                       collect (let ((expander (cdr (assoc (car option) *defwidget-options-expanders*))))
                                 (funcall expander option name))))))))
