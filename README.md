# speed-type

[![MELPA](http://melpa.org/packages/speed-type-badge.svg)](http://melpa.org/#/speed-type)

Practice touch/speed typing in emacs.

This is a fork, since the original maintainer does not seem to actively work on
it. This fork fixes some things, and add some cool new stuff like utf-8 support
and easy keybindings at the end of a run to play again or quit.

![Screenshot](https://raw.github.com/hagleitn/speed-type/master/speed-type-screen-shot.png)

## Installation

~~Install speed-type from [MELPA](melpa.org) with~~ (not working - you will
install the original speed-type which is less featured):

```
M-x package-install RET speed-type
```

If you prefer to install by hand: Put speed-type.el into a directory specified by the load-path variable. Alternatively, you can add a directory to the variable load-path by (add-to-list 'load-path "ADDITIONAL-DIRECTORY").

If you put the file in "~/.emacs.d/speed-type/speed-type.el" for instance, the following snipped in your .emacs file will load and init the extension.

```lisp
(add-to-list 'load-path "~/.emacs.d/speed-type/speed-type.el")
(require 'speed-type)
```

## Running speed-type

Executing M-x speed-type-text will start the typing exercise. A new buffer will open and a random text sample will appear. As you type the text it will change color to show progress and higlight correct and incorrect entries. Timing happens automatically, the clock starts on the first character typed and ends with the last. Statistics like characters typed, words-per-minute, and total time will be shown as soon as the last character is entered.

You can use any buffer or part of it to run speed-type. M-x speed-type-region and M-x speed-type-buffer will do the same thing as speed-type-text, except they take the text sample you've picked.

speed-type-buffer by default will only take a random portion of the buffer - If
you want the whole buffer, use C-u speed-type-buffer.

Random samples are taken from Project Gutenberg. A small number of books will be downloaded on demand and stored in "~/emacs.d/speed-type". They will only be downloaded once.
