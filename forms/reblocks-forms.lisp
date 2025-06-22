;; reblocks-forms - A module for handling web forms in Reblocks, partially inspired by Seaside
;; Author: Mariano Montone <marianomontone@gmail.com>
;; Usage:
;; Use WITH-FORM to render a web form, and setup CALLBACKs to handle field values on submit.
;; Example:
;;
;;     (let ((field-values (list)))
;;       (with-form (:post (lambda (&rest args)
;;                           (declare (ignore args))
;;                           (break "Field values: ~s" field-values)))
;;         (:label "Name: ")
;;         (:input :name (callback (value)
;;                         (push (cons "name" value) field-values)))
;;         (:label "Lastname: ")
;;         (:input :name (callback (value)
;;                             (push (cons "lastname" value) field-values)))
;;         (:input :type "submit" :value "Submit")))

(defpackage :reblocks/forms
  (:use :cl)
  (:import-from #:reblocks/actions
                #:make-action-url
                #:make-action
                #:get-request-action)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:reblocks/request
                #:get-path)
  (:import-from #:reblocks/utils/string
                #:humanize-name
                #:attributize-name)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:reblocks/variables
                #:*action-string*)
  (:export #:with-form
           #:callback
           #:submit-form-action))

(in-package :reblocks/forms)

(defun handle-form-submit (action)
  (lambda (&rest args)
    "Handle form submit"
    ;; Iterate over the submitted values, and process those that
    ;; are bound to an action.
    (log:debug "Form arguments: ~s" args)
    (loop for field-name in args by #'cddr
          for field-value in (rest args) by #'cddr
          do
             (let* ((reblocks/actions::*ignore-missing-actions* t)
                    (action (reblocks/actions::get-session-action (string-downcase (string field-name)))))
               (when action
                 (log:debug "Calling form field callback: ~a with: ~s" action field-value)
                 (funcall action field-value))))
    (funcall action)))

(defmacro callback (args &body body)
  "To register field callbacks use callback as name.
For example: (:input :name (callback (value) ...))"
  `(make-action (lambda ,args ,@body)))

(defmacro submit-form-action (args &body body)
  "Initiate a form submit of the current form.
This action can be used in events that ocurred inside the form.
For example:
(:input :onchange (submit-form-action (&key &allow-other-keys) ...))"
  `(format nil "initiateFormAction(\"~A\", event, this)"
           (url-encode (make-action
                        (handle-form-submit (lambda ,args ,@body))))))

(defmacro with-form
    ((method-type
      action &key
               id
               class
               style
               enctype
               (use-ajax-p t)
               extra-submit-code
               (submit-fn "initiateFormAction(\"~A\", event, this)"))
     &body body)
  "Transforms to a form like (:form) with standard form code (AJAX support, actions, etc.)"
  (let ((action-code (gensym)))
    `(let ((,action-code (make-action (handle-form-submit ,action))))
       (with-html ()
         (:form :id ,id :class ,class :style ,style :action (get-path)
                :method (attributize-name ,method-type) :enctype ,enctype
                :onsubmit (when ,use-ajax-p
                            (format nil "~@[~A~]~A; return false;"
                                    ,extra-submit-code
                                    (format nil ,submit-fn
                                            (url-encode (or ,action-code "")))))
                (:span
                 ,@body
                 (:input :name *action-string* :type "hidden" :value ,action-code)))))))
