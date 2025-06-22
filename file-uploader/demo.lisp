(defpackage :reblocks-file-uploader-demo
  (:use :cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:arrows
                #:->))

(in-package :reblocks-file-uploader-demo)

(defapp file-uploader-demo
  :prefix "/"
  :autostart t
  :routes ((reblocks/routes:page ("/" :name "Demo")
             (make-instance 'reblocks/file-uploader:file-uploader))))

(defmethod reblocks/dependencies:get-dependencies ((app file-uploader-demo))
  (append (reblocks/file-uploader:get-dependencies)
          (call-next-method)))

(defvar *port* (find-port:find-port))

(defun start (&key (port *port*))
  "(start)"
  (reblocks/server:start :port port
                         :apps '(file-uploader-demo
                                 reblocks/file-uploader:file-uploader-server)))

;; (start)
