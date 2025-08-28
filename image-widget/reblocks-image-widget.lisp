;;; reblocks-image-widget --- Image widget for Reblocks web framework.

;; Copyright (C) 2025 Mariano Montone. All rights reserved.

;; This work is licensed under the terms of the MIT license.
;; For a copy, see <https://opensource.org/licenses/MIT>.

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Version: 0.1

;;; Commentary:
;;
;; Image widget for Reblocks web framework.
;;
;; Usage:
;;
;; Create an IMAGE-WIDGET widget, and pass it a pathname to an image in `':path`',
;; then render the widget.
;;
;;; Code:

(defpackage :reblocks/image-widget
  (:use #:cl
        #:reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/app
                #:defapp)
  (:export #:image-server
           #:image-widget
           #:image-url
           #:image-id))

(in-package :reblocks/image-widget)

(defvar *images* (make-hash-table :test 'equalp))
(defvar *image-base-url* nil)

(defapp image-server
  :prefix "/img/"
  :autostart nil
  :routes ((40ants-routes/defroutes:get ("/<image-id>" :name "Image handler")
             (multiple-value-bind (image-path found-p)
                 (gethash image-id *images*)
               (if (or (not found-p)
                       (not (probe-file image-path)))
                   (lack/response:make-response 404 nil "Image not found")
                   (lack/response:make-response 200 nil image-path))))))

(defwidget image-widget ()
  ((id :accessor image-id)
   (path :initarg :path
         :accessor image-path
         :initform (error "Provide the image path"))
   (mime-type :initarg :mime-type
              :accessor image-mime-type
              :initform nil)
   (alt :initarg :alt
        :accessor image-alt
        :initform nil)
   (width :initarg :width
          :accessor image-width
          :initform nil)
   (height :initarg :height
           :accessor image-height
           :initform nil)
   (style :initarg :style
          :accessor image-style
          :initform nil)
   (css-class :initarg :css-class
              :accessor css-class
              :initform nil)))

(defun sha256-file (path)
  "Compute the SHA-256 hash of the file at PATH and return it as a hex string."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((digest (ironclad:make-digest :sha256)))
      (loop for buffer = (make-array 4096 :element-type '(unsigned-byte 8))
            for bytes-read = (read-sequence buffer stream)
            while (> bytes-read 0)
            do (ironclad:update-digest digest buffer :end bytes-read))
      (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest)))))

(defmethod initialize-instance :after ((image image-widget) &rest initargs)
  (declare (ignore initargs))
  ;; check path of image
  (unless (probe-file (image-path image))
    (error "Image file does not exist: ~s" (image-path image)))
  ;; assign id
  (setf (image-id image)
        (sha256-file (image-path image)))
  ;; register path in image table to be served later
  (setf (gethash (image-id image) *images*) (image-path image)))

(defun image-url (image)
  (with-output-to-string (s)
    (when *image-base-url*
      (write-string *image-base-url* s))
    (write-string "/img/" s)
    (write-string (image-id image) s)))

(defmethod render ((image image-widget))
  (with-html ()
    (:img :src (image-url image)
          :width (image-width image)
          :height (image-height image)
          :alt (image-alt image)
          :style (image-style image)
          :class (css-class image))))

;; test

;; (reblocks/server:start :port 9090 :apps 'image-server)
;; (make-instance 'image-widget :path #p"/home/marian/src/tabler/web/test/tabler-avatars/jpg/f3382b5fa7e14fcab30d4279f203c83a.jpg")
;; (reblocks/server:stop "localhost" 9090)


;; (reblocks/preview:preview (make-instance 'image-widget :path #p"/home/marian/src/tabler/web/test/tabler-avatars/jpg/f3382b5fa7e14fcab30d4279f203c83a.jpg"))

;; (reblocks/preview:preview (make-instance 'image-widget :path #p"/home/marian/src/tabler/web/test/tabler-avatars/jpg/e6f79cdf2a45c0eb58b3f93407361989.jpg"))

;; (reblocks/preview:preview (make-instance 'image-widget :path #p"/home/marian/src/tabler/web/test/tabler-avatars/jpg/e6f79cdf2a45c0eb58b3f93407361989.jpg" :width 200 :height 200))
