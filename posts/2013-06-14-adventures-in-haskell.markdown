---
title: Adventures in Haskell
date: 2013-06-14 17:13:01
tags: haskell, lisp
---
Recently, I switched from the Lisp family of languages to Haskell. The difference is quite pronounced as writing in Lisp has many conveniences. For example, Lisp's macro system is still unparalleled by other languages. Despite being a functional language, Lisp still allows mutation so its break from imperative languages is not so wide, except for its eccentric syntax (or lack thereof).

However, Haskell, being a purely functional language, does not allow such conveniences. Everything has to be streamlined into the functional point of view. Mutations are only simulated; types have to be known at compile time. In exchange, Haskell gives you many treats. Powerful optimizations, which even Lisp with its mutation leniency can only dream of, are now possible; a whole class of bug is eliminated with the help of the compiler. This leads to Haskell being able to compete with C with its ruling one true ring of efficiency.

I try to do everything with Haskell to get a feel of the language. As Lisp and Haskell are both functional language, I should try writing progroms previously written in Lisp now in Haskell. Therefore, I have started a project to port Lisp programs in Lisp books to Haskell. The first book will be [Land of Lisp](http://landoflisp.com/).

If you are interested, the ported code is available on [GitHub](http://github.com/mrordinaire/land-of-haskell).
