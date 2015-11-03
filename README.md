# 11/2015 Update

I've given spacemacs a try, and have decided to not pursue this project. If you want to use it for some reason, let me know. I will license it MIT.


# Modal Emacs #

Modal Emacs is an package that adds modal keybindings to Emacs. 

Normally, you access commands in emacs through modifier keys, such as
control, meta, and shift. Modal Emacs makes it easy to change keyboard
"modes", which changes what keys do.

Thus, whenever a keyboard is in "command" mode, we can use `s` to
search, which is easier on the hand (and, in my limited experience, 
faster to type).


# Alpha Warning #

Modal Emacs is still in early development. You should expect lots of
change as the project evolves.

If you have any feedback, suggestions, or would like to help, please
contact me via issues!

# Not a Vi Emulator #

There are a bunch of frameworks that emulate Vim keybindings in
Emacs. This is not one of them.

Vim is a great editor. I used Vim for years, and I really like(d) it. 
However, Emacs and Vim are very different
beasts, and the way that they operate is also very different. Their
philosophies are different, and their intellectual lineage is
different.

Modal Emacs is an attempt to bring modal editing to Emacs, and have it
exist in a native, natural way. It does not try to feel familiar to
Vim converts.

# Installation #

## Manual ##

Download modal-emacs.el someone into your loadpath. For example, you could do: 

```sh
cd ~/.emacs.d/
wget https://raw.github.com/joelmccracken/modal-emacs/master/modal-emacs.el
```

Load it into emacs: 

```emacs-lisp
;; assuming that you've 
(require 'modal-emacs)
```

## Package ##

Modal Emacs should be available as a package soon.

# Usage and Configuration # 

## Enabling Globally ##

The command `modal-emacs-globalized-mode` will enable Modal Emacs as
a globalized minor mode -- thus, it will be enabled in every buffer
currently in emacs, and all buffers that will be created in the
future.

I still don't like Modal Emacs to be active in every buffer.
You may enable Modal Emacs on an per-buffer basis with
`modal-emacs-on`.  I have it selectively enabled with this: 

```emacs-lisp
;; only loads in coffeescript and ruby files
(add-hook 'coffee-mode-hook 'modal-emacs-on)
(add-hook 'ruby-mode-hook 'modal-emacs-on)
```

# Problems? Help? #

Please direct all inquiries to issues!
