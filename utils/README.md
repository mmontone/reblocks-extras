# reblocks-extras-utils

Utilities for Reblocks web framework.

[[source code]](../reblocks-extras-utils.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Utilities for Reblocks web framework.



## Functions
### css-classes

```lisp
(&rest classes)
```


### make-js-callback

```lisp
(callback &rest args)
```

Returns a javascript function to call CALLBACK.




### make-js-value-action

```lisp
(action &optional (js-source "this.value"))
```

Returns JS code which can be inserted into onchange attribute and will
execute given Lisp function when HTML input changes, passing its string value as parameter.



It accepts any function as input and produces a string with JavaScript code.
JS-SOURCE should be a javascript string that is evaluated client side
and passed as argument to ACTION.
The javascript value is accessible in &key value in the callback.

Example:
   (:input :type "text"
           :onchange
               (make-js-value-action
                   (lambda (&key value &allow-other-keys)
                       (setf (input-value widget) value))))
### make-ps-callback

```lisp
(callback &rest args)
```

Returns a javascript function to call CALLBACK.




### test-session-code

```lisp
(func)
```


## Macros
### defwidget\*

```lisp
(name direct-superclasses direct-slots &rest options)
```

Widget definition macro on top of [REBLOCKS/WIDGET:DEFWIDGET](REBLOCKS/WIDGET:DEFWIDGET) that supports extra options.



Options:
(:renderer (widget) &body body)
Specify the widget renderer method, inline.

(:html-tag <html-tag-keyword>)
HTML tag to use by the widget, via [REBLOCKS/WIDGET:GET-HTML-TAG](REBLOCKS/WIDGET:GET-HTML-TAG) .

(:css-classes css-classes-list &key (inherit t))
CSS classes for the widget, via [REBLOCKS/WIDGET:GET-CSS-CLASSES](REBLOCKS/WIDGET:GET-CSS-CLASSES) .

(:dependencies &rest (dependency-class &rest initargs))
List of widget dependencies.

### let\*\*

```lisp
(bindings &body body)
```

Parallel let





### make-js-action\*

```lisp
(&body body)
```



