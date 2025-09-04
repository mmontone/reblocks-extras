(require :peppol)

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
  :prefix "/"
  :subclasses (reblocks/tom-select:tom-select-server)
  :autostart t
  :routes ((reblocks/routes:page ("/" :name "Tom-Select test")
             (make-instance 'tom-select-test))))

(defwidget tom-select-test ()
  ())

(defun swap-cons (cons)
  (cons (cdr cons) (car cons)))

(defun match-countries (entered)
  (mapcar #'swap-cons
          (remove-if-not (lambda (country-name)
                           (str:containsp entered country-name :ignore-case t))
                         peppol/code-lists:|ISO 3166-1:Alpha2 Country codes|
                         :key #'car)))

;; (match-countries "Ar")

(defmethod render ((widget tom-select-test))
  (with-html ()
    (:div
     (make-instance 'tom-select :options '(("foo" . "Foo")
                                           ("bar" . "Bar"))))
    (:div
     (make-instance 'tom-select :options #'match-countries))))

(defun start (&key (port 9091))
  (reblocks/server:start :port port
                         :apps '(reblocks-tom-select-test-app)))

;; (start)
