# FSet: Functional Collections for Common Lisp

FSet is a functional, set-theoretic collections library for Common Lisp.
_Functional_ means that all update operations return a new collection rather than
modifying an existing one in place.  Don't be daunted by the term _set-theoretic_!
It just means that collections have clean semantics, and may be nested arbitrarily
with no additional programmer effort — for instance, sets may contain sets, maps may
be keyed by sets, etc.

FSet is designed to be an excellent _default_ choice for collections in Common Lisp
programs: you can use them in practically all cases where you need a collection.  It
has a wide API, with sets, maps, bags (multisets), seqs (sequences), binary
relations, tuples, and more; and lots of useful operations on these types.

NEW!  My e-book, [Modern Common Lisp with
FSet](https://fset.common-lisp.dev/Modern-CL/Top_html/index.html), is now live!  It
has a tutorial, lots of explanatory material, an API reference, and some examples.

The [FSet home page](https://gitlab.common-lisp.net/fset/fset/-/wikis/home) has links
to relevant blog posts, and other info.

There might be some useful information on the [FSet CLiki page](http://cliki.net/FSet).

FSet is installable via Quicklisp:

```
> (ql:quickload "fset")
```

I occasionally post FSet news to [my blog](https://scottlburson2.blogspot.com/).

## Supported Common Lisp implementations

- SBCL (tested on 2.6.0)
- Clozure CL (tested on 1.13)
- ABCL (tested on 1.9.2)
- Franz Allegro (tested on 11.0)
- LispWorks (tested on 8.0.1)
- ECL (tested on 26.3.27)

CLASP support is waiting on a fix for [CLASP bug
1731](https://github.com/clasp-developers/clasp/issues/1731).
