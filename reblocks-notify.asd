(asdf:defsystem "reblocks-notify"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "Notifications for Reblocks via notify.js library."
  :components ((:module "notify"
                :components
                ((:file "reblocks-notify"))))
  :depends-on (:reblocks))
