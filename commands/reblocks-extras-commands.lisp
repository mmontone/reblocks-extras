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
;;; Code:

(defpackage :reblocks/extras/commands
  (:use :cl)
  (:export #:add-css-class
           #:remove-css-class
           #:toggle-css-class))

(in-package :reblocks/extras/commands)

(defun get-dependencies ()
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-extras-commands "commands/reblocks-extras-commands.js"))
                  :type :js)))

(defun add-css-class (widget css-class)
  (reblocks/commands:add-command
   :add-class
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :css-class css-class))

(defun remove-css-class (widget css-class)
  (reblocks/commands:add-command
   :remove-class
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :css-class css-class))

(defun toggle-css-class (widget css-class)
  (reblocks/commands:add-command
   :toggle-class
   :dom-id (reblocks/widgets/dom:dom-id widget)
   :css-class css-class))
