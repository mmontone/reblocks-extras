(require :reblocks/tom-select)

(defpackage :reblocks/tom-select/test
  (:use :cl :reblocks/tom-select #:reblocks/html)
  (:import-from #:reblocks/widget
                #:widget
                #:defwidget
                #:render)
  (:import-from #:reblocks/app
                #:defapp))

(in-package :reblocks/tom-select/test)

(defapp reblocks-tom-select-test-app
  :autostart t
  :routes ((reblocks/routes:page ("/" :name "Tom-Select test")
             (make-instance 'tom-select-test))))

(defmethod reblocks/dependencies:get-dependencies ((app reblocks-tom-select-test-app))
  (append (reblocks/tom-select:get-dependencies)
          (call-next-method)))

(defwidget tom-select-test ()
  ())

(defmethod render ((widget tom-select-test))
  (with-html ()
    (:div
     (make-instance 'tom-select :options '(("foo" . "Foo")
                                           ("bar" . "Bar"))))))

(defun start (&key (port 9091))
  (reblocks/server:start :port port
                         :apps '(reblocks-tom-select-test-app)))

;; (start)
