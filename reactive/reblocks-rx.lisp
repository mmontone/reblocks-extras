(require :reblocks)
(require :trivial-garbage)

(defpackage :reblocks-rx
  (:use :cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget)
  (:export #:@))

(in-package :reblocks-rx)

(defun find-rxvars (form)
  (when (listp form)
    (if (eq (car form) '@)
        ;; rxvar access found
        (list (second form))
        ;; look into subforms
        (loop for subform in form
              appending (find-rxvars subform)))))

;; (find-rxvars '(@ hello))
;; (find-rxvars '(lambda () (+ (@ foo) (@ bar))))

(defun gen-id ()
  (symbol-name (gensym "rx")))

(defclass rxvar ()
  ((value :initarg :value :accessor @)
   (dependents :accessor dependents
               :initform nil)))

(defmethod (setf @) :after (value (rxvar rxvar))
  (dolist (dependent (dependents rxvar))
    ;;(let ((dependent (trivial-garbage:weak-pointer-value dependent)))
      (when dependent
        (funcall dependent value))
    ))

(defmethod print-object ((rxvar rxvar) stream)
  (format stream "@~s" (@ rxvar)))

(defun make-rxvar (value)
  (make-instance 'rxvar :value value))

(defun onchange (rxvar handler)
  (push handler (dependents rxvar)))

;; (defvar *rxvar1* (make-rxvar 2))

;; (@ *rxvar1*)

;; (onchange *rxvar1* #'print)

;; (setf (@ *rxvar1*) 25)

(defclass rx-element (sgml-composer::element)
  ((id :initarg :id
       :accessor element-id
       :initform (gen-id))
   (attribute-rxvars :initarg :attribute-rxvars
                      :accessor attribute-rxvars)))

(defwidget rx-widget ()
  ((children :initarg :children
             :accessor children)
   (css-classes :initarg :css-classes
                :initform nil
                :accessor widget-css-classes)
   (html-tag :initarg :html-tag
             :initform :div
             :accessor widget-html-tag))
  (:documentation "Reactive widget"))

(defmethod reblocks/widget:get-css-classes ((widget rx-widget))
  (widget-css-classes widget))

(defmethod reblocks/widget:get-html-tag ((widget rx-widget))
  (widget-html-tag widget))

(defmethod render ((widget rx-widget))
  (let ((result (funcall (renderer widget) widget)))
    (when (typep result 'reblocks/widget:widget)
      (render result))))

(defun make-rx-widget (html-tag attributes children &key attr-rxvars)
  (let ((widget (make-instance 'rx-widget
                               :html-tag (if attributes (cons html-tag attributes)
                                             html-tag)
                               :css-classes (getf attributes :class)
                               :children children)))
    ;; register rxvar handlers
    ;; update view using set-attribute command when a reactive variable changes
    (dolist (rxvar attr-rxvars)
      (destructuring-bind (attr value-reader rxvars) attr-rxvars
        (dolist (rxvar rxvars)
          (on :change rxvar
              (lambda (value)
                (declare (ignore value))
                (reblocks/commands:add-command
                 :set-attribute
                 :attr attr
                 :value (funcall value-reader)))))))))

(defclass rx-dolist (sgml-composer::element)
  ())

(defun make-rx-element (tag attributes children &rest args)
  (let ((children (if (atom children)
                      (list children)
                      children)))
    (apply #'make-instance 'rx-element
           :tag tag
           :attributes attributes
           :children (remove nil children)
           args)))

(defun transform-sgml-forms (form)
  (if (atom form)
      form
      (cond
        ((eql (first form) 'dolist))
        ((eql (first form) 'if))
        ((eql (first form) 'when))
        ((keywordp (first form))
         (destructuring-bind (tag attrs &body children) form
           ;; inspect attributes. extract references to rxvar values.
           ;; forms (@ place)
           (let ((attrs-rxvars (loop for attr in attrs by #'cddr
                                      for value in (cdr attrs) by #'cddr
                                      for attr-rxvars = (find-rxvars value)
                                      when attr-rxvars
                                        collect (cons attr attr-rxvars))))
             (cond
               (attrs-rxvars
                ;; if there are rxvars in attributes, create a reactive element
                `(make-rx-element ,tag (list ,@attrs)
                                  ,(cond
                                     ((= (length children) 0) "")
                                     ((= (length children) 1)
                                      (transform-sgml-forms (car children)))
                                     ((> (length children) 1)
                                      `(list ,@(mapcar #'transform-sgml-forms children))))
                                  :attrs-rxvars (list
                                                  ,@(loop for (attr . rxvars) in attrs-rxvars
                                                          collect `(list ,attr
                                                                         (lambda ()
                                                                           ,(getf attrs attr))
                                                                         (quote ,rxvars))))))
               ;; otherwise create a normal sgml element
               (t
                `(sgml-composer::make-element
                  ,tag
                  (list ,@attrs)
                  ,(cond
                     ((= (length children) 0) "")
                     ((= (length children) 1)
                      (transform-sgml-forms (car children)))
                     ((> (length children) 1)
                      `(list ,@(mapcar #'transform-sgml-forms children)))))))))))))

(eval (transform-sgml-forms '(:div (:foo "bar"))))

(eval (transform-sgml-forms '(:div (:foo (@ foo)))))
