(asdf:defsystem "reblocks-file-uploader"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "file-uploader"
                :components
                ((:file "reblocks-file-uploader.lisp"))))
  :depends-on (:reblocks))
