# reblocks-rx

Reactive Reblocks.

[[source code]](../reblocks-rx.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Reactive Reblocks provides Reblocks widgets that update with increased granularity;
 instead of redrawing the whole widget, only specific parts of it are redrawn.
 This can be useful for certain widgets that want to prevent losing the current
 status of HTML elements. For example, form inputs can lose its current value
 if the widget that contains it is redrawn; in that situation could be better
 to be able to redraw only certain parts of the widget and prevent losing the
 value and status (like current focus) of the inputs.

 Usage:

 TODO



## Functions
### add

```lisp
(elem rxlist)
```


### add-last

```lisp
(elem rxlist)
```


### get-dependencies

```lisp
()
```


### make-rxlist

```lisp
(&optional elems)
```


### make-rxval

```lisp
(&optional value)
```


### rxlist-remove

```lisp
(elem rxlist &key (test #'eql) key)
```


## Macros
### rxformula

```lisp
(&body body)
```



### with-sgml

```lisp
(args body)
```



## Generic-Functions
### render-sgml

```lisp
(widget)
```


### set-html-attribute

```lisp
(widget attribute value)
```


## Slot-Accessors
### @
### (setf @)
## Classes
### rx-attr-widget
A widget that supports reactivity at attributes position.

### rxobject
### sgml-widget
Widget renderer using sgml-composer

