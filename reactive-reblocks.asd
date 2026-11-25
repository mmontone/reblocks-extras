(asdf:defsystem "reactive-reblocks"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "reactive"
                :components
                ((:file "reblocks-rx"))))
  :depends-on (:reblocks
               :mutils
               (:require :sgml-composer)
               (:require :reblocks-extras-commands)
               (:require :reblocks-extras-widgets)))
