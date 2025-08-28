# reblocks-browser-history

Browser history api manipulation for Reblocks.

[[source code]](../reblocks-browser-history.lisp)

- **Author**: Mariano Montone <marianomontone@gmail.com>
- **Version**: 0.1


 Browser history api manipulation for Reblocks.

 Usage:

 TODO



## Functions
### browser-history-back

```lisp
()
```

Invoke history.back() on the client.



See: [https://developer.mozilla.org/en-US/docs/Web/API/History_API](https://developer.mozilla.org/en-US/docs/Web/API/History_API)
### browser-history-forward

```lisp
()
```

Invoke history.forward() on the client.



See: [https://developer.mozilla.org/en-US/docs/Web/API/History_API](https://developer.mozilla.org/en-US/docs/Web/API/History_API)
### browser-history-push-state

```lisp
(url &key (state "{}") pop-action)
```

Invoke history.pushState on the client.



POP-ACTION receives location and

See: [https://developer.mozilla.org/en-US/docs/Web/API/History_API](https://developer.mozilla.org/en-US/docs/Web/API/History_API)
### get-dependencies

```lisp
()
```


