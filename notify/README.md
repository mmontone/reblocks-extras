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
(message style &key target options)
```

Notify MESSAGE using STYLE.



Example:

    (notify "Hello world" "success")
