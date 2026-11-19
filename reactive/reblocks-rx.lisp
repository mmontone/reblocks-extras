;; Reactive Reblocks provides Reblocks widgets that update with increased granularity;
;; instead of redrawing the whole widget, only specific parts of it are redrawn.
;; This can be useful for certain widgets that want to prevent losing the current
;; status of HTML elements. For example, form inputs can lose its current value
;; if the widget that contains it is redrawn; in that situation could be better
;; to be able to redraw only certain parts of the widget and prevent losing the
;; value and status (like current focus) of the inputs.

(require :reblocks)
(require :trivial-garbage)

(defpackage :reblocks-rx
  (:use :cl)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget
                #:widget)
  (:export #:@)
  (:documentation "Reactive Reblocks."))

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

(defclass rxobject ()
  ((dependents :accessor dependents
               :initform nil)))

(defun emit (rxval &rest args)
  (dolist (dependent (dependents rxval))
    ;;(let ((dependent (trivial-garbage:weak-pointer-value dependent)))
    (when dependent
      (apply dependent args))))

(defclass rxvalue (rxobject)
  ((value :initarg :value :accessor @)))

(defmethod (setf @) :after (value (rxval rxvalue))
  (emit rxval :changed value))

(defmethod print-object ((rxval rxvalue) stream)
  (format stream "@~s" (@ rxval)))

(defun make-rxval (&optional value)
  (make-instance 'rxvalue :value value))

(defun onchange (rxval handler)
  (push handler (dependents rxval)))

;; (defvar *rxval1* (make-rxval 2))

;; (@ *rxval1*)

;; (onchange *rxval1* #'print)

;; (setf (@ *rxval1*) 25)

(defmacro with-rxvals (vars &body body)
  `(let ,(loop for var in vars
               collect (list (if (listp var)
                                 (car var)
                                 var)
                             (if (listp var)
                                 `(make-rxval ,(second var))
                                 `(make-rxval))))
     ,@body))

(defclass rxlist (rxvalue)
  ()
  (:documentation "A reactive list. Signals different events when element is added, inserted, removed."))

(defun add (elem rxlist)
  (push elem (@ rxlist))
  (emit rxlist :added elem)
  rxlist)

(defun rxlist-remove (elem rxlist &key (test #'eql) key)
  (setf (@ rxlist)
        (remove elem (@ rxlist) :test test :key key))
  (emit rxlist :removed elem)
  rxlist)

(defclass rx-element (sgml-composer::element)
  ((id :initarg :id
       :accessor element-id
       :initform (gen-id))
   (attribute-rxvars :initarg :attribute-rxvars
                     :accessor attribute-rxvars)))

(defwidget rx-widget ()
  ((renderer :initarg :renderer
             :accessor renderer)
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

;; if rxlist emits a :changed event, then redraw the whole list
;; if rxlist emits a :added, :removed, :inserted event, then add, remove of insert an HTML node via Reblocks commands.
(defwidget rxlist-widget ()
  ((rxlist :initarg :rxlist
           :accessor rxlist)
   (item-renderer :initarg :item-renderer
                  :accessor item-renderer)))

(defwidget rxconditional-widget ()
  ((test :initarg :test
         :accessor test-of)
   (rxvals :initarg :rxvals
           :accessor rxvals)
   (true-renderer :initarg :true-renderer
                  :accessor true-renderer
                  :initform nil)
   (false-renderer :initarg :false-renderer
                   :accessor false-renderer
                   :initform nil)))

(defun make-rx-element (tag attributes children &rest args)
  (let ((children (if (atom children)
                      (list children)
                      children)))
    (apply #'make-instance 'rx-element
           :tag tag
           :attributes attributes
           :children (remove nil children)
           args)))

(defun make-rx-widget (tag attributes renderer &key attrs-rxvars)
  (let ((widget
          (make-instance 'rx-widget
                         :html-tag (cons tag attributes)
                         :renderer renderer)))
    ;; setup rxvars dependents
    (dolist (attr-rxvars attrs-rxvars)
      (destructuring-bind (attr value-lambda rxvars) attr-rxvars
        (dolist (rxvar rxvars)
          (onchange rxvar
                    (reblocks/commands:add-command
                     :set-attribute
                     :widget widget
                     :attribute attr
                     :value (funcall value-lambda))))))
    widget))

(defun transform-sgml-forms (form)
  (if (atom form)
      form
      (cond
        ((eql (first form) 'dolist))
        ((eql (first form) 'if))
        ((eql (first form) 'when))
        ((eql (first form) 'unless))
        ((keywordp (first form))
         (destructuring-bind (tag attrs &body body) form
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
                `(make-rx-widget ,tag (list ,@attrs)
                                 (lambda () (with-sgml () ,@body))
                                 :attrs-rxvars (list
                                                ,@(loop for (attr . rxvars) in attrs-rxvars
                                                        collect `(list ,attr
                                                                       (lambda ()
                                                                         ,(getf attrs attr))
                                                                       (list ,@rxvars))))))
               ;; otherwise create a normal sgml element
               (t
                `(sgml-composer::make-element
                  ,tag
                  (list ,@attrs)
                  ,(cond
                     ((= (length body) 0) "")
                     ((= (length body) 1)
                      (transform-sgml-forms (car body)))
                     ((> (length body) 1)
                      `(list ,@(mapcar #'transform-sgml-forms body)))))))))))))

(defmacro with-sgml (args &body body)
  `(progn ,(transform-sgml-forms body)))

(eval (transform-sgml-forms '(:div (:foo "bar"))))

(eval
 (macroexpand `(with-rxvals ((foo 1) (bar 2))
                 ,(transform-sgml-forms
                   `(:div (:foo (+ (@ foo) (@ bar)))
                          (:span () "Hello"))))))

(defwidget sgml-widget ()
  ()
  (:documentation "Widget renderer using sgml-composer"))

(defgeneric render-sgml (widget))

(defvar *sgml-output* nil)

(defmethod render ((widget sgml-widget))
  ;; capture html output
  ;; if sgml output is already bound, create an sgml raw element with the html
  ;; otherwise, output the sgml to the html output
  (let (sgml)
    (let ((*sgml-output* t))
      (setf sgml (render-sgml widget)))
    (check-type sgml sgml-composer:element)
    (if *sgml-output*
        ;; the sgml output is already bound, create an sgml raw element with the html
        (sgml-composer::make-element :raw nil
                                     (sgml-composer:write-sgml sgml))
        ;; sgml output not bound, write the html to reblocks stream
        (sgml-composer:write-sgml sgml :destination reblocks/html:*stream*))))

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

(defmethod sgml-composer:as-sgml ((widget widget))
  (if (typep widget 'sgml-widget)
      (render-sgml widget)
      (sgml-composer:make-element :raw nil
                                  (with-output-to-string (reblocks/html:*stream*)
                                    (render widget)))))

(defmethod render-sgml ((widget sgml-test-2))
  (sgml-composer:sgml
    (:div (:class "test2")
          (make-instance 'sgml-test-1)
          (reblocks/widgets/string-widget:make-string-widget "Hello again"))))

(with-output-to-string (reblocks/html:*stream*)
  (render (make-instance 'sgml-test-2)))

(defmethod spinneret:html ((element sgml-composer:element))
  (sgml-composer:write-sgml element :destination spinneret:*html*))

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
