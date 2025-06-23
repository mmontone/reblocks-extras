(asdf:defsystem "reblocks-paginated-list"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :components ((:module "paginated-list"
                :components
                ((:file "mupaginator")
                 (:file "reblocks-paginated-list"))))
  :depends-on (:reblocks :reblocks-lass :cl-who))
