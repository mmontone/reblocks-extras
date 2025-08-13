;;; reblocks-browser-history --- Browser history api manipulation for Reblocks.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Browser history api manipulation for Reblocks.
;;
;; Usage:
;;
;; TODO
;;
;;; Code:

(defpackage #:reblocks/browser-history
  (:use #:cl)
  (:import-from #:reblocks/actions
                #:make-action)
  (:import-from #:reblocks/commands
                #:add-command)
  (:export #:browser-history-push-state
           #:browser-history-back
           #:browser-history-forward
           #:get-dependencies))

(in-package :reblocks/browser-history)

(defun browser-history-push-state (url &key (state "{}")
                                         pop-action)
  "Invoke history.pushState on the client.

POP-ACTION receives location and

See: https://developer.mozilla.org/en-US/docs/Web/API/History_API"
  (let ((action-code (when pop-action
                       (etypecase pop-action
                         (string pop-action)
                         (function (reblocks/actions:make-action pop-action))))))
    (add-command :update-history
                 :operation "pushState"
                 :state state
                 :url url
                 :pop-action-code action-code)))

(defun browser-history-forward ()
  "Invoke history.forward() on the client.

See: https://developer.mozilla.org/en-US/docs/Web/API/History_API"
  (add-command :update-history
               :operation "forward"))

(defun browser-history-back ()
  "Invoke history.back() on the client.

See: https://developer.mozilla.org/en-US/docs/Web/API/History_API"
  (add-command :update-history
               :operation "back"))

(defun get-dependencies ()
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :reblocks-browser-history "file-uploader/reblocks-browser-history.js"))
                  :type :js)))
