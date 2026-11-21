(in-package :reblocks-rx)

(defvar *rxval1* (make-rxval 2))


(@ *rxval1*)

(onevent *rxval1* #'print)

(setf (@ *rxval1*) 25)


(eval (transform-sgml-forms '(:div (:foo "bar"))))

(eval
 (macroexpand
  `(with-rxvals ((foo 1) (bar 2))
     ,(transform-sgml-forms
       `(:div (:foo (+ (@ foo) (@ bar)))
              (:span () "Hello"))))))

(eval
 (macroexpand
  `(with-rxvals ((foo 1) (bar 2))
     ,(transform-sgml-forms
       `(when (@ foo)
          (:div (:foo (+ (@ foo) (@ bar)))
                (:span () "Hello")))))))

(eval
 (macroexpand
  `(with-rxvals ((foo 1) (bar 2))
     ,(transform-sgml-forms
       `(if (@ foo)
            (:div (:foo (+ (@ foo) (@ bar)))
                  (:span () "Hello"))
            (:div () "No")
            )))))

(transform-sgml-forms
 `(:div ()
        (format nil "Foo is: ~a" (@ foo))))

(eval
 (macroexpand
  `(with-rxvals ((foo 1) (bar 2))
     ,(transform-sgml-forms
       `(:div ()
              (format nil "Foo is: ~a" (@ foo)))))))

(defwidget sgml-test-1 (sgml-widget)
  ())

(defmethod render-sgml ((widget sgml-test-1))
  (sgml-composer:sgml
    (:div (:foo "foo")
          "Hello world")))

(with-output-to-string (reblocks/html:*stream*)
  (render (make-instance 'sgml-test-1)))

(defwidget sgml-test-2 (sgml-widget)
  ())

(defmethod render-sgml ((widget sgml-test-2))
  (sgml-composer:sgml
    (:div (:class "test2")
          (make-instance 'sgml-test-1)
          (reblocks/widgets/string-widget:make-string-widget "Hello again"))))

(with-output-to-string (reblocks/html:*stream*)
  (render (make-instance 'sgml-test-2)))

(defmethod spinneret:html ((element sgml-composer:element))
  (sgml-composer:write-sgml element :destination spinneret:*html*
                            :quote-char #\'))

(defun sgml-test-1 ()
  (sgml-composer:sgml
    (:div (:foo "fooo")
          "Hello")))

(spinneret:with-html
  (:div "Hello"))

(spinneret:with-html
  (:div (sgml-test-1)))

(spinneret:with-html
  (:div (make-instance 'sgml-test-2)))

(defwidget counter (sgml-widget)
  ((counter :initform (make-rxval 0)
            :accessor counter)))

(defmethod render-sgml ((widget counter))
  (with-slots (counter) widget
    (with-sgml ()
      (:div ()
            (:h2 (:style (if (oddp (@ counter))
                              "color:red;"
                              "color:blue;"))
                 "A counter")
            (:div ()
                  (if (oddp (@ counter))
                      (:span () "Counter is odd")
                      (:span () "Counter is even")))
            (:div () (format nil "Value: ~a" (@ counter)))
            (:div ()
                  (:input (:type "button"
                           :onclick (reblocks/actions:make-js-action
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       (incf (@ counter))))
                           :value "Increase"))
                  (:input (:type "button"
                           :onclick (reblocks/actions:make-js-action
                                     (lambda (&rest args)
                                       (declare (ignore args))
                                       (setf (@ counter) 0)))
                           :value "Reset")))))))

(defmethod reblocks/dependencies:get-dependencies ((app reblocks/preview:preview))
  (append (reblocks-rx:get-dependencies) (call-next-method)))

(reblocks/preview:preview (make-instance 'counter))

(require :reblocks-extras)

(reblocks/extras/utils:test-session-code (lambda () (with-output-to-string (reblocks/html:*stream*) (render (make-instance 'counter)))))

(require :cl-json)
(json:lisp-to-camel-case (symbol-name :background-color))

(defwidget multicounter (sgml-widget)
  ())

(defmethod render-sgml ((widget multicounter))
  (with-sgml ()
    (:div ()
          (loop for i from 1 to 10 collect
                                   (make-instance 'counter)))))

(reblocks/preview:preview (make-instance 'multicounter))
