;;; reblocks-extras-commands --- Extra commands for Reblocks framework.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Extra commands for Reblocks framework.
;;
;; Usage: add to application dependencies via GET-DEPENDENCIES function.
;;
;;; Code:

(defpackage :reblocks/extras/commands
  (:use :cl)
  (:export #:add-css-class
           #:remove-css-class
           #:toggle-css-class
           #:set-attribute
           #:prompt
           #:confirm
           #:alert
           #:inline-prompt
           #:inline-confirm
           #:inline-alert
           #:get-dependencies))

(in-package :reblocks/extras/commands)

(defun get-dependencies ()
  "Use this method in your app to add to your dependencies."
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-extras-commands "commands/reblocks-extras-commands.js"))
                  :type :js)))

(defun add-css-class (widget css-class &optional path)
  "Add CSS-CLASS to WIDGET.
If PATH is present, then use jQuery.find method to stablish the target element first."
  (reblocks/commands:add-command
   :add-class
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :css-class css-class
   :path path))

(defun remove-css-class (widget css-class &optional path)
  "Remove CSS-CLASS from WIDGET.
If PATH is present, then use jQuery.find method to stablish the target element first."
  (reblocks/commands:add-command
   :remove-class
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :css-class css-class
   :path path))

(defun toggle-css-class (widget css-class &optional path)
  "Toggle CSS-CLASS in WIDGET.
If PATH is present, then use jQuery.find method to stablish the target element first."
  (reblocks/commands:add-command
   :toggle-class
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :css-class css-class
   :path path))

(defun set-attribute (widget attribute value &optional path)
  "Set ATTRIBUTE of WIDGET to VALUE.
If PATH is present, then use jQuery.find method to stablish the target element first."
  (reblocks/commands:add-command
   :set-attribute
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :attribute attribute
   :value value
   :path path))

(defun inline-prompt (message action)
  (let ((action-code (reblocks/actions:make-action action)))
    (format nil "reblocksPrompt({message:'~a', actionCode: '~a'); return false;"
            message action-code)))

(defun inline-confirm (message action)
  (let ((action-code (reblocks/actions:make-action action)))
    (format nil "reblocksConfirm({message:'~a', actionCode: '~a'}); return false;"
            message action-code)))

(defun inline-alert (message action)
  (let ((action-code (reblocks/actions:make-action action)))
    (format nil "reblocksAlert({message:'~a', actionCode: '~a'}); return false;"
            message action-code)))

(defun prompt (message action)
  "Browser prompt with MESSAGE, callback with ACTION."
  (reblocks/commands:add-command
   :prompt
   :message message
   :action-code (reblocks/actions:make-action action)))

(defun confirm (message action)
  "Browser confirm with MESSAGE, callback with ACTION."
  (reblocks/commands:add-command
   :confirm
   :message message
   :action-code (reblocks/actions:make-action action)))

(defun alert (message action)
  "Browser alert with MESSAGE, callback with ACTION."
  (reblocks/commands:add-command
   :alert
   :message message
   :action-code (reblocks/actions:make-action action)))
