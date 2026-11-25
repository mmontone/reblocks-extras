# reblocks-notify

Notifications for Reblocks.

[[source code]](../reblocks-notify.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Notifications for Reblocks using notify.js library.

 Usage:

 Add dependencies via GET-DEPENDENCIES to your application.
 Then invoke NOTIFY function.



## Functions
### get-dependencies

```lisp
()
```


### notify

```lisp
(message &optional (style "info") target &rest options)
```

Notify MESSAGE using STYLE.



Examples:

    (notify "Hello world" "success")
    (notify "Hello world" "success" "#myElem")
    (notify "Hello world" "success" "#myElem" :|position| "topcenter")
    (notify "Hello world" "success" nil :|position| "topcenter")
