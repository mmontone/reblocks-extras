;; Reactive Reblocks provides Reblocks widgets that update with increased granularity;
;; instead of redrawing the whole widget, only specific parts of it are redrawn.
;; This can be useful for certain widgets that want to prevent losing the current
;; status of HTML elements. For example, form inputs can lose its current value
;; if the widget that contains it is redrawn; in that situation could be better
;; to be able to redraw only certain parts of the widget and prevent losing the
;; value and status (like current focus) of the inputs.

(require :reblocks)
(require :trivial-garbage)
(require :mutils)
(require :sgml-composer)
(require :reblocks-extras-commands)
(require :reblocks-extras-widgets)

(defpackage :reblocks-rx
  (:use :cl #:reblocks/html)
  (:import-from #:reblocks/widget
                #:render
                #:update
                #:defwidget
                #:widget)
  (:export #:@
           #:get-dependencies
           #:sgml-widget
           #:render-sgml
           #:with-rx-vals
           #:rxformula
           #:set-html-attribute)
  (:documentation "Reactive Reblocks."))

(in-package :reblocks-rx)

(defun get-dependencies ()
  (reblocks/extras/commands:get-dependencies))

(defun find-rxvars (form &key ignore-sgml-forms dont-go-into)
  "If IGNORE-SGML-FORMS is T, then don't look into forms with the form: (:html-tag attrs &body body)"
  (when (listp form)
    (when (and (or (not ignore-sgml-forms) (not (keywordp (car form))))
               (not (member (car form) dont-go-into)))
      (if (eq (car form) '@)
          ;; rxvar access found
          (list (second form))
          ;; look into subforms
          (loop for subform in form
                appending (find-rxvars
                           subform
                           :ignore-sgml-forms ignore-sgml-forms
                           :dont-go-into dont-go-into))))))

;; (find-rxvars '(@ hello))
;; (find-rxvars '(lambda () (+ (@ foo) (@ bar))))

;; (find-rxvars '(div () (@ hello)))
;; (find-rxvars '(:div () (@ hello)))
;; (find-rxvars '(:div () (@ hello)) :ignore-sgml-forms t)
;; (find-rxvars '((@ hello) (:div () (@ no))) :ignore-sgml-forms t)
;; (find-rxvars '((@ hello) (setf(@ foo) 22)) :ignore-sgml-forms t)
;; (find-rxvars '((@ hello) (setf (@ foo) 22)) :ignore-sgml-forms t :dont-go-into '(setf))

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

(defun onevent (rxval handler)
  (push handler (dependents rxval)))

(defmacro with-rxvals (vars &body body)
  `(let ,(loop for var in vars
               collect (list (if (listp var)
                                 (car var)
                                 var)
                             (if (listp var)
                                 `(make-rxval ,(second var))
                                 `(make-rxval))))
     ,@body))

(defclass rx-formula (rxobject)
  ((rxvars :initarg :rxvars
           :accessor rxvars)
   (formula-fn :initarg :formula-fn
               :accessor formula-fn)))

(defmethod initialize-instance :after ((rx-formula rx-formula) &rest initargs)
  (declare (ignore initargs))
  (dolist (rxvar (remove-duplicates (rxvars rx-formula)))
    (onevent rxvar (lambda (event &rest args)
                     (declare (ignore args))
                     (when (eql event :changed)
                       (emit rx-formula :changed
                             (funcall (formula-fn rx-formula))))))))

(defmethod @ ((rx-formula rx-formula))
    (funcall (formula-fn rx-formula)))

(defmacro rxformula (&body body)
  `(make-instance 'rx-formula
                  :formula-fn (lambda () ,@body)
                  :rxvars (list ,@(find-rxvars body))))

;; (macroexpand '(rxformula (+ (@ foo) (@ bar))))

(defclass rxlist (rxvalue)
  (())
  (:documentation "A reactive list. Signals different events when element is added, inserted, removed."))

(defun make-rxlist (&optional elems)
  (make-instance 'rxlist :value elems))

(defun add (elem rxlist)
  (push elem (@ rxlist))
  (emit rxlist :changed :insert elem 0)
  rxlist)

(defun add-last (elem rxlist)
  (setf (@ rxlist)
        (reblocks/utils/list:ninsert (@ rxlist) elem (length (@ rxlist))))
  (emit rxlist :changed :insert elem (1- (length (@ rxlist))))
  rxlist)

(defun rxlist-remove (elem rxlist &key (test #'eql) key)
  (setf (@ rxlist)
        (remove elem (@ rxlist) :test test :key key))
  (emit rxlist :changed :removed elem)
  rxlist)

(defwidget rx-widget ()
  ((renderer :initarg :renderer
             :accessor renderer)
   (css-classes :initarg :css-classes
                :initform nil
                :accessor widget-css-classes)
   (html-tag :initarg :html-tag
             :initform :span
             :accessor widget-html-tag))
  (:documentation "Reactive widget"))

(defmethod reblocks/widget:get-css-classes ((widget rx-widget))
  (widget-css-classes widget))

(defmethod reblocks/widget:get-html-tag ((widget rx-widget))
  (widget-html-tag widget))

(defmethod render ((widget rx-widget))
  (let ((result (funcall (renderer widget))))
    (if (typep result 'reblocks/widget:widget)
        (render result)
        result)))

(defun make-rx-widget (html-tag attributes renderer &key attrs-rxvars rxvars)
  "ATTR-RXVARS is a list of the form (attribute-name value-reader-lambda rxvars), with reactive variables found in the attribute value.
RXVARS is a list of reactive variables found in the body of the widget."
  (let ((widget (make-instance 'rx-widget
                               :html-tag (if attributes (cons html-tag attributes)
                                             html-tag)
                               :css-classes (getf attributes :class)
                               :renderer renderer)))
    ;; register rxvar handlers
    ;; update view using set-attribute command when a reactive variable changes
    (dolist (attr-rxvars attrs-rxvars)
      (destructuring-bind (attr value-reader rxvars) attr-rxvars
        (dolist (rxvar (remove-duplicates rxvars))
          (onevent rxvar
                   (lambda (event &rest args)
                     (declare (ignore args))
                     (when (eql event :changed)
                       (reblocks/extras/commands:set-attribute
                        widget
                        (string-downcase (symbol-name attr))
                        (funcall value-reader))))))))

    ;; when a reactive variable of the body of the widget changes, update the widget
    (dolist (rxvar (remove-duplicates rxvars))
      (onevent rxvar
               (lambda (event &rest args)
                 (declare (ignore args))
                 (when (eql event :changed)
                   (update widget)))))

    ;; return the widget
    widget
    ))

;; if rxlist emits a :changed event, then redraw the whole list
;; if rxlist emits a :added, :removed, :inserted event, then add, remove of insert an HTML node via Reblocks commands.
(defwidget rxlist-widget (reblocks/extras/widgets:inline-widget)
  ((rxlist :initarg :rxlist
           :accessor rxlist)))

(defwidget rxconditional-widget (sgml-widget)
  ((test :initarg :test
         :accessor test-of)
   (then :initarg :then
         :accessor then
         :initform nil)
   (else :initarg :else
         :accessor else
         :initform nil)))

(defmethod reblocks/widget:get-html-tag ((widget rxconditional-widget))
  :span)

(defun make-rxlist-widget (rxlist item-renderer &key html-tag attributes)
  (make-instance 'rxlist-widget
                 :rxlist rxlist
                 :renderer item-renderer
                 :html-tag (if attributes
                               (cons html-tag attributes)
                               html-tag)))

(defun make-rxconditional-widget (test then else &key rxvars)
  (let ((widget
          (make-instance 'rxconditional-widget
                         :test test
                         :then then
                         :else else)))
    (dolist (rxvar (remove-duplicates rxvars))
      (onevent rxvar
               (lambda (event &rest args)
                 (declare (ignore event args))
                 (update widget))))

    widget))

(defmethod render-sgml ((widget rxconditional-widget))
  (if (funcall (test-of widget))
      (if (then widget)
          (funcall (then widget))
          ;; if there's no then clause then render empty
          (sgml-composer:sgml (:span ())))
      (if (else widget)
          (funcall (else widget))
          ;; if there's no else clause then render empty
          (sgml-composer:sgml (:span ())))))

(defun make-element (tag attributes children)
  ;; we make an exception here
  ;; if children is a reactive list widget, then we set its tag and attributes
  ;; to the parent element and return it
  (if (and (typep children 'rxlist-widget)
           (null (reblocks/extras/widgets:widget-html-tag children)))
      (progn
        (setf (reblocks/extras/widgets:widget-html-tag children)
              (cons tag attributes))
        children)
      ;; otherwise, just use sgml-composer
      (sgml-composer:make-element tag attributes children)))

(defun transform-sgml-forms (form)
  (if (atom form)
      form
      (cond
        ((eql (first form) 'dolist)
         (destructuring-bind ((var lst) &body body) (cdr form)
           (if (and (listp lst)
                    (eql (car lst) '@))
               `(make-rxlist-widget ,(cadr lst)
                                    (lambda (,var)
                                      ,@(transform-sgml-forms body)))
               `(dolist (,var ,lst)
                  ,@(transform-sgml-forms body)))))
        ((eql (first form) 'if)
         (destructuring-bind (test then else) (cdr form)
           (let ((rxvars (find-rxvars test)))
             (if rxvars
                 `(make-rxconditional-widget
                   (lambda () ,test)
                   (lambda () ,(transform-sgml-forms then))
                   (lambda () ,(transform-sgml-forms else))
                   :rxvars (list ,@rxvars)))
             `(if ,test
                  ,(transform-sgml-forms then)
                  ,(transform-sgml-forms else)))))
        ((eql (first form) 'when)
         (destructuring-bind (test &body body) (cdr form)
           `(make-rxconditional-widget
             (lambda () ,test)
             (lambda () ,@(mapcar #'transform-sgml-forms body))
             nil
             :rxvars (list ,@(find-rxvars test)))))
        ((eql (first form) 'unless)
         (destructuring-bind (test &body body) (cdr form)
           `(make-rxconditional-widget
             (lambda () (not ,test))
             (lambda () ,@(mapcar #'transform-sgml-forms body))
             nil
             :rxvars (list ,@(find-rxvars test)))))
        ((keywordp (first form))
         (destructuring-bind (tag attrs &body body) form
           ;; inspect attributes. extract references to rxvar values.
           ;; forms (@ place)
           (let ((attrs-rxvars (loop for attr in attrs by #'cddr
                                     for value in (cdr attrs) by #'cddr
                                     for attr-rxvars = (find-rxvars value)
                                     ;; actions are not for being updated..or should they?
                                     when (and (not (member attr '(:onclick)))
                                               attr-rxvars)
                                       collect (cons attr attr-rxvars)))
                 (rxvars-in-body (find-rxvars body
                                              :ignore-sgml-forms t
                                              :dont-go-into '(setf incf decf push if when unless dolist))))
             (cond
               ;; if there are rxvars in attributes, create a reactive element
               (attrs-rxvars
                `(make-rx-widget ,tag (list ,@attrs)
                                 (lambda () ,@(transform-sgml-forms body))
                                 :attrs-rxvars (list
                                                ,@(loop for (attr . rxvars) in attrs-rxvars
                                                        collect `(list ,attr
                                                                       (lambda ()
                                                                         ,(getf attrs attr))
                                                                       (list ,@rxvars))))))
               ;; if the element body contains references to reactive variables, create a reactive widget
               (rxvars-in-body
                `(make-rx-widget ,tag (list ,@attrs)
                                 (lambda () ,@(transform-sgml-forms body))
                                 :rxvars (list ,@rxvars-in-body)))
               ;; otherwise create a normal sgml element
               (t
                `(make-element
                  ,tag
                  (list ,@attrs)
                  ,(cond
                     ((= (length body) 0) "")
                     ((= (length body) 1)
                      (transform-sgml-forms (car body)))
                     ((> (length body) 1)
                      `(list ,@(mapcar #'transform-sgml-forms body))))))))))
        ;; no match for the form
        ;; return it
        (t (mapcar #'transform-sgml-forms form)))))

(defmacro with-sgml (args body)
  (declare (ignore args))
  (transform-sgml-forms body))

(defwidget sgml-widget ()
  ()
  (:documentation "Widget renderer using sgml-composer"))

(defgeneric render-sgml (widget))

(defmethod render ((widget sgml-widget))
  (let ((sgml (render-sgml widget)))
    (check-type sgml sgml-composer:element)
    (sgml-composer:write-sgml sgml :destination reblocks/html:*stream*
                                   :quote-char #\')))

(defmethod sgml-composer:as-sgml ((widget widget))
  (sgml-composer:make-element :raw nil
                              (with-output-to-string (reblocks/html:*stream*)
                                (render widget))))

;; reactive widget attributes

(defmethod set-html-attribute ((widget widget) attribute value)
  (reblocks/extras/commands:set-attribute widget attribute value))

(defwidget rx-attr-widget ()
  ((attributes :initarg :attributes
               :accessor html-attributes
               :initform nil
               :documentation "A property list with HTML attributes for the widget. Attribute values can be reactive objects; when they change, the DOM is updated."))
  (:documentation "A widget that supports reactivity at attributes position."))

(defmethod initialize-instance :after ((widget rx-attr-widget) &rest initargs)
  (declare (ignore initargs))
  ;; subscribe to events in attributes
  ;; change the HTML attribute value when some attribute changes
  (let ((html-tag-and-attributes (reblocks/widget:get-html-tag widget)))
    (let ((attributes (append (when (listp html-tag-and-attributes)
                                (rest html-tag-and-attributes))
                              (etypecase (html-attributes widget)
                                (list (html-attributes widget))
                                (function (funcall (html-attributes widget) widget))))))
      (alexandria:doplist (attr-name attr-value attributes)
        (when (typep attr-value 'rxobject)
          (onevent attr-value (lambda (&rest args)
                                (declare (ignore args))
                                (set-html-attribute widget attr-name (@ attr-value)))))))))

(defmethod reblocks/widget:get-html-tag :around ((widget rx-attr-widget))
  ;; return the widget attribute values
  (let ((html-tag-and-attributes (call-next-method)))
    (let ((html-tag (if (listp html-tag-and-attributes)
                        (car html-tag-and-attributes)
                        html-tag-and-attributes))
          (attributes (append (when (listp html-tag-and-attributes)
                                (rest html-tag-and-attributes))
                              (etypecase (html-attributes widget)
                                (list (html-attributes widget))
                                (function (funcall (html-attributes widget) widget))))))
      (if (not attributes)
          html-tag
          (cons html-tag
                (mutils-utils:map-plist
                 (lambda (attr-name attr-value)
                   (declare (ignore attr-name))
                   (if (typep attr-value 'rxobject)
                       (@  attr-value)
                       attr-value))
                 attributes))))))
