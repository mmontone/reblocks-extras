# reblocks-extras-commands

Extra commands for Reblocks framework.

[[source code]](../reblocks-extras-commands.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Extra commands for Reblocks framework.

 Usage: add to application dependencies via GET-DEPENDENCIES function.



## Functions
### add-css-class

```lisp
(widget css-class &optional path)
```

Add CSS-CLASS to WIDGET.
If PATH is present, then use jQuery.find method to stablish the target element first.




### alert

```lisp
(message action)
```

Browser alert with MESSAGE, callback with ACTION.




### confirm

```lisp
(message action)
```

Browser confirm with MESSAGE, callback with ACTION.




### get-dependencies

```lisp
()
```

Use this method in your app to add to your dependencies.




### inline-alert

```lisp
(message action)
```


### inline-confirm

```lisp
(message action)
```


### inline-prompt

```lisp
(message action)
```


### insert-child

```lisp
(widget child index)
```

Insert CHILD-WIDGET into WIDGET at INDEX.




### prompt

```lisp
(message action)
```

Browser prompt with MESSAGE, callback with ACTION.




### remove-child

```lisp
(widget index)
```

Insert CHILD-WIDGET into WIDGET at INDEX.




### remove-css-class

```lisp
(widget css-class &optional path)
```

Remove CSS-CLASS from WIDGET.
If PATH is present, then use jQuery.find method to stablish the target element first.




### set-attribute

```lisp
(widget attribute value &optional path)
```

Set ATTRIBUTE of WIDGET to VALUE.
If PATH is present, then use jQuery.find method to stablish the target element first.




### toggle-css-class

```lisp
(widget css-class &optional path)
```

Toggle CSS-CLASS in WIDGET.
If PATH is present, then use jQuery.find method to stablish the target element first.




