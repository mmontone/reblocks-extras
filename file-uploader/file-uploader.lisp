(defpackage :reblocks/file-uploader
  (:use :cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:arrows
                #:->)
  (:export #:file-uploader
           #:file-uploader-server
           #:get-dependencies))

(in-package :reblocks/file-uploader)

(defvar *file-uploaders* (make-hash-table :test 'equalp))
(defvar *default-storage-directory* uiop/stream:*temporary-directory*)

(defun gen-id ()
  (-> (uuid:make-v4-uuid)
      (uuid:uuid-to-byte-array)
      (base64:usb8-array-to-base64-string :uri t)))

(defwidget file-uploader ()
  ((id :initform (gen-id)
       :accessor file-uploader-id)
   (filename :initarg :filename
             :accessor file-uploader-filename
             :type (or null string)
             :initform nil)
   (pathname :initarg :pathname
             :accessor file-uploader-pathname
             :type (or null pathname)
             :initform nil
             :documentation "Pathname of the uploaded file.")
   (mime-type :initarg :mime-type
              :type string
              :accessor file-uploader-mime-type
              :initform "application/octet-stream")
   (allowed-mime-types :initarg :allowed-mime-types
                       :initform t
                       :type (or boolean list)
                       :documentation "Either T (all mime types) or a list of allowed mime types.")
   (progress :initform 0
             :type integer
             :accessor file-uploader-progress)
   (status :initform :created
           :accessor file-uploader-status)
   (event-handlers :initform (make-hash-table)
                   :accessor event-handlers)))

(defmethod initialize-instance :after ((file-uploader file-uploader) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (file-uploader-id file-uploader) *file-uploaders*)
        file-uploader))

(defun file-uploader-storage-pathname (file-uploader)
  (if (file-uploader-pathname file-uploader)
      (file-uploader-pathname file-uploader)
      (merge-pathnames (file-uploader-id file-uploader)
                       *default-storage-directory*)))

(defmethod render ((file-uploader file-uploader))
  (with-html ()
    (:div
     (:input :type "file"
             :class "form-control"
             :id (file-uploader-id file-uploader)
             :name (file-uploader-id file-uploader)
             :value (when (file-uploader-filename file-uploader)
                      (file-uploader-filename file-uploader)))
     (:button :type "button"
              :class "btn"
              :onclick (ps:ps (file-uploader-upload (ps:lisp (file-uploader-id file-uploader))))
              "Upload"))
    (:div :id (format nil "~a-status" (file-uploader-id file-uploader)))
    (:input :type "number" :id (format nil "~a-progress" (file-uploader-id file-uploader)))))

(defun get-dependencies ()
  (list
   (make-instance 'reblocks/dependencies:local-dependency
                  :path (probe-file (asdf:system-relative-pathname :dbapp "web/reblocks-file-uploader/reblocks-file-uploader.js"))
                  :type :js)))

(defmethod reblocks/dependencies:get-dependencies ((app file-uploader-server))
  (append (get-dependencies)
          (call-next-method)))

(defapp file-uploader-server
  :prefix "/upload/"
  :autostart nil
  :routes ((40ants-routes/defroutes:post ("/file/<id>" :name "File upload")
             (let ((file-uploader (gethash id *file-uploaders*)))
               (destructuring-bind (bytes-stream file-name mime-type)
                   (reblocks/request:get-parameter id)
                 (alexandria:write-byte-vector-into-file
                  (alexandria:read-stream-content-into-byte-vector bytes-stream)
                  (file-uploader-storage-pathname file-uploader))
                 (lack/response:make-response 200 nil "OK"))))))
