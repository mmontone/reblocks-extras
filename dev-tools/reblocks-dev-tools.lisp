;;; reblocks-dev-tools --- Development tools for Reblocks.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Development tools for Reblocks.
;;
;; Usage:
;;
;; Add REBLOCKS-DEV-TOOLS application and dependencies via GET-DEPENDENCIES to your application.
;;
;;; Code:

(defpackage :reblocks/dev-tools
  (:use #:cl)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/widget
                #:update)
  (:export #:clear-inspector-widgets
           #:get-dependencies
           #:reblocks-dev-tools
           #:*inspected-widget*))

(in-package :reblocks/dev-tools)

(defvar *widgets* (make-hash-table :test 'equalp :weakness :value))
(defvar *inspected-widget* nil
  "The widget being inspected.")

;; we need to start recording widgets when they are created
(defmethod initialize-instance :after ((widget reblocks/widget:widget) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (reblocks/widgets/dom:dom-id widget) *widgets*)
        widget))

(defun clear-inspector-widgets ()
  (setf *widgets* (make-hash-table :test 'equalp :weakness :value)))

(defun get-dependencies ()
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-dev-tools "dev-tools/reblocks-dev-tools.js"))
                  :type :js)))

(defapp reblocks-dev-tools
  :prefix "/dev-tools/"
  :routes ((40ants-routes/defroutes:get ("/inspect/<dom-id>")
             (when-let ((widget (gethash dom-id *widgets*)))
               (setf *inspected-widget* widget)
               ;; SWANK:INSPECT-IN-EMACS doesn't work if I don't bind these
               (let ((swank::*buffer-package* (find-package :cl))
                     (swank::*buffer-readtable* *readtable*))
                 (swank:inspect-in-emacs widget)
                 "Ok")))
           (40ants-routes/defroutes:get ("/update/<dom-id>")
             (when-let ((widget (gethash dom-id *widgets*)))
               (update widget))
             "Ok")
           (40ants-routes/defroutes:get ("/resetsession")
             (reblocks/debug:reset-latest-session)
             "Ok")
           (reblocks/routes:page ("/routes" :name "routes"
                                            :title "Routes")
             (with-html ()
               (:h3 "Routes")
               (:ul
                (dolist (route (40ants-routes/routes:children-routes
                                (reblocks/app:app-routes (reblocks/app:get-current))))
                  (:li (princ-to-string route))))))))
