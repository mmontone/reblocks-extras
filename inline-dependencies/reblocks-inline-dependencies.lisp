;;; reblocks-inline-dependencies --- Inline dependencies for Reblocks web framework.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Inline dependencies for Reblocks web framework.
;;
;; Usage: TODO
;;
;;; Code:

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
  (list* (make-instance 'reblocks/dependencies:local-dependency
                        :type :js
                        :path (asdf:system-relative-pathname :reblocks-inline-dependencies "inline-dependencies/inline-dependencies.js"))
         (call-next-method)))
