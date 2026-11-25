;;; reblocks-extras-utils --- Utilities for Reblocks web framework.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Utilities for Reblocks web framework.
;;
;;; Code:

(defpackage :reblocks/extras/utils
  (:use :cl)
  (:export #:defwidget*
           #:make-js-value-action
           #:test-session-code
           #:with-test-session
           #:make-js-callback
           #:make-ps-callback
           #:let**
           #:make-js-action*
           #:css-classes
           #:ui))

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

(defmacro with-test-session (&body body)
  `(test-session-code (lambda () ,@body)))

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
  `(reblocks/actions:make-js-action (lambda (&key &allow-other-keys)
                                      ,@body)))

(defun css-classes (&rest classes)
  (str:join #\space (remove nil (alexandria:flatten classes))))


(defparameter *defwidget-options-expanders*
  `((:render . ,(lambda (renderer name)
                  (destructuring-bind ((arg) &body body) renderer
                    `(defmethod reblocks/widget:render ((,arg ,name))
                       ,@body))))
    (:css-classes . ,(lambda (css-classes-spec name)
                       (destructuring-bind (css-classes &key (inherit t)) css-classes-spec
                         `(defmethod reblocks/widget:get-css-classes ((widget ,name))
                            (declare (ignore widget))
                            ,(if inherit
                                 `(append ',css-classes (call-next-method))
                                 `',css-classes)))))
    (:css-classes-method . ,(lambda (css-classes-method name)
                              (destructuring-bind ((widget) &body body) css-classes-method
                                `(defmethod reblocks/widget:get-css-classes ((,widget ,name))
                                   ,@body))))
    (:html-tag . ,(lambda (html-tag name)
                    (if (> (length html-tag) 1)
                        ;; method form ((widget) &body body)
                        (destructuring-bind ((widget) &body body) html-tag
                          `(defmethod reblocks/widget:get-html-tag ((,widget ,name))
                             ,@body))
                        ;; otherwise, direct tag specification
                        `(defmethod reblocks/widget:get-html-tag ((widget ,name))
                           (declare (ignore widget))
                           ',(car html-tag)))))
    (:dependencies . ,(lambda (dependencies name)
                        `(defmethod reblocks/dependencies:get-dependencies ((widget ,name))
                           (list* ,@(mapcar (lambda (dependency-spec)
                                              (destructuring-bind (dependency-class &rest args) dependency-spec
                                                `(make-instance ',dependency-class ,@args)))
                                            dependencies)
                                  (call-next-method)))))))

(defmacro defwidget* (name direct-superclasses direct-slots &rest options)
  "Widget definition macro on top of REBLOCKS/WIDGET:DEFWIDGET that supports extra options.

Options:
(:render (widget) &body body)
Specify the widget render method, inline.

(:html-tag html-tag)
HTML tag to use by the widget, via REBLOCKS/WIDGET:GET-HTML-TAG .

(:html-tag (widget) &body body)
HTML tag method. Should return an HTML tag, or (html-tag . attributes) for the widget.

(:css-classes css-classes-list &key (inherit t))
CSS classes for the widget, via REBLOCKS/WIDGET:GET-CSS-CLASSES .

(:css-classes-method (widget) &body body)
CSS classes method. Should return a list of CSS classes for the widget.

(:dependencies &rest (dependency-class &rest initargs))
List of widget dependencies."
  (flet ((custom-option-p (option)
           (member (car option) (mapcar #'car *defwidget-options-expanders*))))
    (if (not (some #'custom-option-p options))
        `(reblocks/widget:defwidget ,name ,direct-superclasses
           ,direct-slots
           ,@options)
        (let ((custom-options (remove-if #'custom-option-p options)))
          `(progn
             (reblocks/widget:defwidget ,name ,direct-superclasses
               ,direct-slots
               ,@custom-options)
             ,@(loop for option in options
                     when (custom-option-p option)
                       collect (let ((expander (cdr (assoc (car option) *defwidget-options-expanders*))))
                                 (funcall expander (cdr option) name))))))))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:html-tag :ul)))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:html-tag (wdg) :ul)))

;; html-tag can return html-tag plus attributes
#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:html-tag (:input . (:type "number")))))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:html-tag (wdg) (cons :input (list :type (input-type wdg))))))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:css-classes ("foo" "bar"))))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:css-classes ("foo" "bar") :inherit nil)))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:css-classes-method (wdg)
    (list "foo" "bar" (class-of-widget wdg)))))

#+example
(macroexpand
 '(defwidget* my-widget ()
   ()
   (:dependencies (dependency2 :config "value"))))

(defmacro ui (&body body)
  "Render HTML with updatable areas.

Example:

(let ((elems (list 1 2 3)))
  (ui
    (:div
     (area my-list
           (:ul :css-classes (list \"list\")
                (dolist (elem elems)
                  (:li elem))))
     (:button :onclick (make-js-action (lambda (&rest _args)
                                         (push (random-string) elems)
                                         (update (area my-list))))
              \"Add\"))))"
  (let ((areas (gensym "areas-")))
    `(let ((,areas (make-hash-table)))
       (macrolet ((area (name &optional spinneret-form)
                    (if (null spinneret-form)
                        ;; if there's no spinneret-form, then this is
                        ;; an area reading form
                        `(gethash ',name ,',areas)
                        ;; otherwise, create the area widget
                        ;; this is a bit of a hack, take the already expanded
                        ;; spinneret form (with-tag (:tag &rest attrs) &body body)
                        ;; and transform it
                        (destructuring-bind (with-tag (html-tag &rest attrs) &rest area-body) spinneret-form
                          (let ((widget-attrs (alexandria:remove-from-plist attrs :class)))
                            (declare (ignore with-tag))
                            `(render (setf (gethash ',name ,',areas)
                                           (reblocks/extras/widgets:make-inline-widget
                                            ,(if widget-attrs
                                                 (cons html-tag widget-attrs)
                                                 html-tag)
                                            widget-attrs
                                            (lambda (,name)
                                              (declare (ignorable ,name))
                                              ,@area-body)))))))))
         (reblocks/html:with-html ()
           ,@body)))))

#+test
(let ((elems (list 1 2 3)))
  (ui
    (:div
     (area my-list
           (:ul :css-classes (list "list")
                (dolist (elem elems)
                  (:li elem))))
     (:button :onclick (make-js-action (lambda (&rest _args)
                                         (push (random-string) elems)
                                         (update (area my-list))))
              "Add"))))
