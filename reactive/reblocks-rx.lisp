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
                #:defwidget)
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

(defun make-rxval (value)
  (make-instance 'rxvalue :value value))

(defun onchange (rxval handler)
  (push handler (dependents rxval)))

;; (defvar *rxval1* (make-rxval 2))

;; (@ *rxval1*)

;; (onchange *rxval1* #'print)

;; (setf (@ *rxval1*) 25)

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

(defun transform-sgml-forms (form)
  (if (atom form)
      form
      (cond
        ((eql (first form) 'dolist))
        ((eql (first form) 'if))
        ((eql (first form) 'when))
        ((eql (first form) 'unless))
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
                                      `(list ,(transform-sgml-forms (car children))))
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

(eval (transform-sgml-forms '(:div (:foo (+ (@ foo) (@ bar)))
                              (:span () "Hello"))))

(defwidget sgml-widget ()
  ()
  (:documentation "Widget renderer using sgml-composer"))

(defvar *sgml-output* nil)

(defmethod render :around ((widget sgml-widget))
  ;; capture html output
  ;; if sgml output is already bound, create an sgml raw element with the html
  ;; otherwise, output the sgml to the html output
  (let (sgml)
    (let ((*sgml-output* t))
      (setf sgml (call-next-method)))
    (check-type sgml sgml-composer:element)
    (if *sgml-output*
        ;; the sgml output is already bound, create an sgml raw element with the html
        (sgml-composer::make-element :raw nil
                                     (sgml-composer:write-sgml sgml))
        ;; sgml output not bound, write the html to reblocks stream
        (sgml-composer:write-sgml sgml :destination reblocks/html:*stream*))))

(defwidget sgml-test-1 (sgml-widget)
  ())

(defmethod render ((widget sgml-test-1))
  ;;(break "hello")
  (sgml-composer:sgml
    (:div (:foo "foo")
          "Hello world")))

(with-output-to-string (reblocks/html:*stream*)
  (render (make-instance 'sgml-test-1)))
