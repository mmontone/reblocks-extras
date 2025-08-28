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

(defun notify (message &optional (style "info") target &rest options)
  "Notify MESSAGE using STYLE.

Examples:

    (notify \"Hello world\" \"success\")
    (notify \"Hello world\" \"success\" \"#myElem\")
    (notify \"Hello world\" \"success\" \"#myElem\" :|position| \"topcenter\")
    (notify \"Hello world\" \"success\" nil :|position| \"topcenter\")

"
  (let ((notify-options (alexandria:plist-hash-table options)))
    (setf (gethash :|className| notify-options) style)
    (add-command :notify
                 :target (or target :null)
                 :message message
                 :options notify-options)))
