(asdf:defsystem "reblocks-extras"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:file "dev-tools/reblocks-dev-tools.lisp")
               (:file "file-uploader/file-uploader.lisp"))
  :depends-on (:reblocks :reblocks-lass))
