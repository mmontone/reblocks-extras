;;; reblocks-extras-widgets --- Extra widgets for Reblocks.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Extra widgets for Reblocks.
;;
;; Usage:
;;
;; TODO
;;
;;; Code:

(defpackage :reblocks/extras/widgets
  (:use :cl #:reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/actions
                #:make-js-action)
  (:export
   #:inline-widget
   #:make-inline-widget
   #:widget-html-tag
   #:widget-css-classes
   #:renderer))

(in-package :reblocks/extras/widgets)

(defwidget inline-widget ()
  ((css-classes :initarg :css-classes
                :initform nil
                :accessor widget-css-classes)
   (html-tag :initarg :html-tag
             :initform :div
             :accessor widget-html-tag)
   (renderer :initarg :renderer
             :accessor renderer)))

(defmethod reblocks/widget:get-css-classes ((widget inline-widget))
  (widget-css-classes widget))

(defmethod reblocks/widget:get-html-tag ((widget inline-widget))
  (widget-html-tag widget))

(defmethod render ((widget inline-widget))
  (let ((result (funcall (renderer widget) widget)))
    (if (typep result 'reblocks/widget:widget)
        (render result)
        result)))

(defun make-inline-widget (html-tag-and-attributes css-classes renderer)
  (make-instance 'inline-widget
                 :html-tag html-tag-and-attributes
                 :css-classes css-classes
                 :renderer renderer))
