;;; reblocks-tom-select --- tom-select based widget for Reblocks.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; tom-select based widget for Reblocks.
;; See: https://tom-select.js.org
;;
;; Usage:
;;
;; Add dependencies via GET-DEPENDENCIES to your application.

;;; Code:

(defpackage :reblocks/tom-select
  (:use :cl)
  (:import-from #:reblocks/widget
                #:widget
                #:defwidget
                #:render)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widgets/dom
                #:dom-id)
  (:export #:get-dependencies
           #:tom-select))

(in-package :reblocks/tom-select)

(defun get-dependencies (&key bootstrap-5)
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-tom-select "tom-select/tom-select.complete.js"))
                  :type :js)
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (if bootstrap-5
                            (probe-file (asdf:system-relative-pathname :reblocks-tom-select "tom-select/tom-select.bootstrap5.css"))
                            (probe-file (asdf:system-relative-pathname :reblocks-tom-select "tom-select/tom-select.css")))
                  :type :css)))

;; See: https://tom-select.js.org/docs/

(defwidget tom-select ()
  ((options :initarg :options
            :accessor select-options
            :initform nil
            :documentation "List of options of the select widget.
An association list, or a function. If a function, then it is used as server side handler.")
   (items :initarg :items
          :accessor select-items
          :initform nil
          :documentation "List of initial selected values.")
   (settings :initarg :settings
             :initform nil
             :documentation "An association list with more tom-select widget settings. See settings in: https://tom-select.js.org/docs/")))

(defmethod reblocks/widget:get-html-tag ((widget tom-select))
  :select)

(defun register-items-handler (id handler)
  (when (not (hash-table-p (reblocks/session:get-value :reblocks/tom-select/handlers)))
    (setf (reblocks/session:get-value :reblocks/tom-select/handlers)
          (make-hash-table :test 'equalp)))

  (let ((handlers-table (reblocks/session:get-value :reblocks/tom-select/handlers)))
    (setf (gethash id handlers-table) handler)))

(defmethod initialize-instance :after ((widget tom-select) &rest initargs)
  (declare (ignore initargs))
  (with-slots (items) widget
    (when (typep items 'trivial-types:function-designator)
      ;; save the widget in the session in order to respond to items requests from client
      (register-items-handler (dom-id widget) items))))

(defmethod render ((widget tom-select))
  (with-slots (options items settings) widget
    (with-html ()
      ;; render options html
      (dolist (option options)
        (:option :value (car option) (cdr option))))
    ;; add javascript
    (reblocks/page-dependencies:push-dependency
     (make-instance 'reblocks/inline-dependencies:inline-dependency
                    :name (format nil "tom-select#~a" (dom-id widget))
                    :type :js
                    :source
                    (ps:ps
                      (ps:chain (j-Query document)
                                (ready
                                 (lambda ()
                                   (ps:new (-Tom-Select (ps:lisp (format nil "#~a" (reblocks/widgets/dom:dom-id widget)))))))))))))
