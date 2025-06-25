(require :mutils)
(require :mutils-docs)

(defpackage :reblocks-extras/docs
  (:use :cl :arrows)
  (:export #:generate-readmes))

(in-package :reblocks-extras/docs)

;; (<module-directory> <module-file> <module-package>)
(defparameter *reblocks-modules*
  '(("paginated-list/" "reblocks-paginated-list.lisp" :reblocks/paginated-list)
    ("dev-tools/" "reblocks-dev-tools.lisp" :reblocks/dev-tools)
    ("file-uploader/" "reblocks-file-uploader.lisp" :reblocks/file-uploader)
    ("inline-dependencies/" "reblocks-inline-dependencies.lisp" :reblocks/inline-dependencies)
    ("utils/" "reblocks-extras-utils.lisp" :reblocks/extras/utils)))

(defun generate-readmes (&optional (modules-desc *reblocks-modules*))
  (dolist (module-desc modules-desc)
    (destructuring-bind (directory-name filename package-name) module-desc
      (let ((module (mutils:parse-lisp-module-file
                     (merge-pathnames filename (asdf:system-relative-pathname :reblocks-extras directory-name)))))
        (mutils-docs:generate-module-docs
         module
         (merge-pathnames "README.md" (asdf:system-relative-pathname :reblocks-extras directory-name))
         package-name)))))
