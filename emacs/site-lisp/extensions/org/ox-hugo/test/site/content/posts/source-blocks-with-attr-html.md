+++
title = "Source blocks with ATTR_HTML"
tags = ["src-block", "attr_html", "attr_css"]
draft = false
+++

Some text.

<style>.indent-block { padding-left: 50px;  }</style>

```emacs-lisp { class="indent-block" }
(message (mapconcat #'identity
                    '("Hello," "how" "are" "you?")
                    " "))
```

Some more text.

<style>.blue { color: blue;  }</style>

```goat { class="blue w-40" }
┌─────┐       ┌───┐
│Alice│       │Bob│
└──┬──┘       └─┬─┘
   │            │
   │ Hello Bob! │
   │───────────>│
   │            │
   │Hello Alice!│
   │<───────────│
┌──┴──┐       ┌─┴─┐
│Alice│       │Bob│
└─────┘       └───┘
```
