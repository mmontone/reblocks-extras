;;; reblocks-notify --- Notifications for Reblocks.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Notifications for Reblocks using notify.js library.
;;
;; Usage:
;;
;; Add dependencies via GET-DEPENDENCIES to your application.
;; Then invoke NOTIFY function.

;;; Code:

(defpackage :reblocks/notify
  (:use :cl)
  (:import-from #:reblocks/actions
                #:make-action)
  (:import-from #:reblocks/commands
                #:add-command)
  (:export #:notify
           #:get-dependencies))

(in-package :reblocks/notify)

(defun get-dependencies ()
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-notify "notify/notify.js"))
                  :type :js)
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-notify "notify/reblocks-notify.js"))
                  :type :js)))

(defun notify (message style &key target options)
  "Notify MESSAGE using STYLE.

Example:

    (notify \"Hello world\" \"success\")

"
  (add-command :notify
               :target target
               :message message
               :style style
               :options options))
