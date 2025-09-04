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
  (:import-from #:trivial-types
                #:function-designator)
  (:import-from #:parenscript
                #:ps #:chain #:lisp #:new #:create)
  (:import-from #:reblocks/app
                #:defapp)
  (:export #:get-dependencies
           #:tom-select
           #:tom-select-server))

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
                  :type :css)
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-tom-select "tom-select/reblocks-tom-select.js")))))

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

(defun register-options-handler (id handler)
  (when (not (hash-table-p (reblocks/session:get-value :reblocks/tom-select/handlers)))
    (setf (reblocks/session:get-value :reblocks/tom-select/handlers)
          (make-hash-table :test 'equalp)))

  (let ((handlers-table (reblocks/session:get-value :reblocks/tom-select/handlers)))
    (setf (gethash id handlers-table) handler)))

(defun find-options-handler (id)
  (gethash id
           (reblocks/session:get-value :reblocks/tom-select/handlers)))

(defmethod initialize-instance :after ((widget tom-select) &rest initargs)
  (declare (ignore initargs))
  (with-slots (options) widget
    (when (typep options 'function-designator)
      ;; save the widget in the session in order to respond to options requests from client
      (register-options-handler (dom-id widget) options))))

(defun options-handler-url (widget)
  (quri:render-uri
   (quri:make-uri :path "/tom-select/options"
                  :query (list (cons "id" (dom-id widget))))))

(defmethod render ((widget tom-select))
  (with-slots (options items settings) widget
    (with-html ()
      ;; render options html
      (when (listp options)
        (dolist (option options)
          (:option :value (car option) (cdr option)))))
    ;; add javascript
    (reblocks/page-dependencies:push-dependency
     (make-instance 'reblocks/inline-dependencies:inline-dependency
                    :name (format nil "tom-select#~a" (dom-id widget))
                    :type :js
                    :source
                    (cond
                      ((typep options 'function-designator)
                       (ps
                         (chain (j-Query document)
                                (ready
                                 (lambda ()
                                   (new (-Tom-Select
                                         (lisp (format nil "#~a" (dom-id widget)))
                                         (create
                                          :value-field "value"
                                          :label-field "label"
                                          :search-field "label"
                                          :items items
                                          :load (lambda (query callback)
                                                  (chain
                                                   (fetch (+ (lisp (options-handler-url widget)) "&query=" query))
                                                   (then (lambda (res) (chain res (json))))
                                                   (then (lambda (json)
                                                           (callback json)))
                                                   (catch (lambda ()
                                                            (callback)))))))))))))
                      (t
                       (ps
                         (chain (j-Query document)
                                (ready
                                 (lambda ()
                                   (new (-Tom-Select
                                         (lisp (format nil "#~a" (dom-id widget)))
                                         (create
                                          :items (lisp items)
                                          :options (lisp options))))))))))))))

(defapp tom-select-server
  :prefix "/tom-select/"
  :autostart nil
  :routes ((40ants-routes/defroutes:post ("/options" :name "Tom select remote options handler")
             (let* ((id (reblocks/request:get-parameter "id"))
                    (query (reblocks/request:get-parameter "query"))
                    (options-handler
                      (gethash id
                               (reblocks/session:get-value :reblocks/tom-select/handlers)))
                    (options (funcall options-handler query)))
               (lack/response:make-response
                200
                (list (cons "Content-Type" "application/json"))
                (json:encode-json-to-string options))))))
