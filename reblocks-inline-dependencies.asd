(asdf:defsystem "reblocks-inline-dependencies"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "inline-dependencies"
                :components
                ((:file "reblocks-inline-dependencies"))))
  :depends-on (:reblocks))
