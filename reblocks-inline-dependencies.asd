(asdf:defsystem "reblocks-inline-dependencies"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "inline-dependencies"
                :components
                ((:file "inline-dependencies"))))
  :depends-on (:reblocks))
