;; load this file to start a demo of reactive reblocks

(require :reblocks-extras-utils)

(defpackage :reblocks-rx-demo
  (:use #:cl #:reblocks/html #:reblocks-rx
        #:reblocks/extras/utils)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget
                #:widget))

(in-package :reblocks-rx-demo)

(defmethod reblocks/dependencies:get-dependencies ((app reblocks/preview:preview))
  (append (reblocks-rx:get-dependencies) (call-next-method)))

;; reactive counter widget

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
            (when (zerop (mod (@ counter) 7))
              (:div () "Divisible by 7!"))
            (unless (zerop (mod (@ counter) 7))
              (:div () "Not Divisible by 7!"))
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

;; (reblocks/preview:preview (make-instance 'counter))

;; multicounter widget

(defwidget multicounter (sgml-widget)
  ())

(defmethod render-sgml ((widget multicounter))
  (with-sgml ()
    (:div ()
          (loop for i from 1 to 10 collect
                                   (make-instance 'counter)))))

;; (reblocks/preview:preview (make-instance 'multicounter))

(defwidget rx-attr-widget-test (rx-attr-widget)
  ((counter :accessor counter
            :type rxobject
            :initform (make-rxval 0)))
  (:default-initargs
   :attributes (lambda (widget)
                 (list :style (rxformula (format nil "background-color: rgb(~a, ~a, ~a)"
                                                 (mod (+ (@ (counter widget)) 20) 255)
                                                 (mod (+ (@ (counter widget)) 30) 255)
                                                 (mod (+ (@ (counter widget)) 40) 255)))))))

(defmethod render ((widget rx-attr-widget-test))
  (with-html ()
    (:input :type "button"
            :onclick (reblocks/actions:make-js-action
                      (lambda (&rest args)
                        (declare (ignore args))
                        (incf (@ (counter widget)) 10)))
            :value "Change color")))

;; (reblocks/preview:preview (make-instance 'rx-attr-widget-test))

;; reactive task list

(defwidget task-list (sgml-widget)
  ((tasks :initform (make-rxlist)
          :accessor tasks)))

(defmethod render-sgml ((widget task-list))
  (let (task-text)
    (with-sgml ()
      (:div ()
            (:ul ()
                 (dolist (task (@ (tasks widget)))
                   (:li ()
                        task
                        (:button (:class "button"
                                  :onclick (reblocks/actions:make-js-action
                                            (lambda (&rest args)
                                              (declare (ignore args))
                                              (rxlist-remove task (tasks widget)))))
                                 "Remove")
                        )))
            (:input (:type "text"
                     :onchange (reblocks/extras/utils:make-js-value-action
                                (lambda (&rest args &key value &allow-other-keys)
                                  (declare (ignore args))
                                  (setf task-text value)))))
            (:input (:type "button"
                     :onclick (reblocks/actions:make-js-action
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (when (not (find task-text (@ (tasks widget))))
                                   (add-last task-text (tasks widget)))))
                     :value "Add task"))))))

;; (reblocks/preview:preview (make-instance 'task-list))

(reblocks/preview:preview
 (reblocks/extras/widgets:make-inline-widget
  :div nil
  (lambda (w)
    (with-html ()
      (:h3 "Reactive attributes")
      (:div
       (make-instance 'rx-attr-widget-test))
      (:h3 "Tasks with reactive list")
      (:div
       (make-instance 'task-list))
      (:h3 "Reactive multicounter")
      (:div
       (make-instance 'multicounter))
      ))))
