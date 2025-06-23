(asdf:defsystem "reblocks-file-uploader"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "file-uploader"
                :components
                ((:file "file-uploader"))))
  :depends-on (:reblocks :arrows :uuid))
