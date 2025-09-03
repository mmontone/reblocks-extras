(asdf:defsystem "reblocks-tom-select"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "tom-select"
                :components
                ((:file "reblocks-tom-select"))))
  :depends-on (:reblocks :reblocks-inline-dependencies :trivial-types))
