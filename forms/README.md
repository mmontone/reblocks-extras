# reblocks-forms

A module for handling web forms in Reblocks, partially inspired by Seaside

[[source code]](../reblocks-forms.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Usage:

 Use WITH-FORM to render a web form, and setup CALLBACKs to handle field values on submit.

 Example:

     (let ((field-values (list)))
       (with-form (:post (lambda (&rest args)
                           (declare (ignore args))
                           (break "Field values: ~s" field-values)))
         (:label "Name: ")
         (:input :name (callback (value)
                         (push (cons "name" value) field-values)))
         (:label "Lastname: ")
         (:input :name (callback (value)
                             (push (cons "lastname" value) field-values)))
         (:input :type "submit" :value "Submit")))




## Macros
### callback

```lisp
(args &body body)
```

To register field callbacks use callback as name.
For example: (:input :name (callback (value) ...))





### submit-form-action

```lisp
(args &body body)
```

Initiate a form submit of the current form.
This action can be used in events that ocurred inside the form.
For example:
(:input :onchange (submit-form-action (&key &allow-other-keys) ...))





### with-form

```lisp
((method-type action &key id class style enctype (use-ajax-p t)
  extra-submit-code (submit-fn "initiateFormAction(\"~A\", event, this)"))
 &body body)
```

Transforms to a form like (:form) with standard form code (AJAX support, actions, etc.)





