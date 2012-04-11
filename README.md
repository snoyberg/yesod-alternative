Yesod comes with a fairly comprehensive set of libraries. Templating is handled
by the Shakespearean languages, type-safe URLs are default, and Persistent is
the go-to database storage layer. Many users love this combination. But there's
no such thing as one-size-fits-all.

This repository contains some proof-of-concept libraries and examples for
mixing and matching alternate components, particularly pulling options from
Snap and Happstack. This is a testament to how well designed these libraries
are (they can easily be plugged into a completely separate framework), and how
simple the Haskell type system makes integration of code.
