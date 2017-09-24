# speed-type [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

Practice touch/speed typing in emacs.

## PLEASE NOTE

**This fork is now the official source for the speed-type package.
Please open pull requests and issues here!**

![Screenshot](https://raw.github.com/parkouss/speed-type/master/speed-type-screen-shot.png)

## Installation

Install speed-type from [MELPA](melpa.org) with:

```
M-x package-install RET speed-type
```

If you prefer to install by hand: Put speed-type.el into a directory specified
by the load-path variable. Alternatively, you can add a directory to the
variable load-path by (add-to-list 'load-path "ADDITIONAL-DIRECTORY").

If you put the file in "~/.emacs.d/speed-type/speed-type.el" for instance, the
following snipped in your .emacs file will load and init the extension.

```lisp
(add-to-list 'load-path "~/.emacs.d/speed-type/speed-type.el")
(require 'speed-type)
```

## Configuration

speed-type can be customized using:

```
M-x customize-group speed-type RET
```

## Running speed-type

Executing M-x speed-type-text will start the typing exercise. A new buffer will
open and a random text sample will appear. As you type the text it will change
color to show progress and higlight correct and incorrect entries. Timing
happens automatically, the clock starts on the first character typed and ends
with the last. Statistics like characters typed, words-per-minute, and total
time will be shown as soon as the last character is entered.

You can use any buffer or part of it to run speed-type. M-x speed-type-region
and M-x speed-type-buffer will do the same thing as speed-type-text, except they
take the text sample you've picked.

speed-type-buffer by default will only take a random portion of the buffer - If
you want the whole buffer, use C-u speed-type-buffer.

Random samples are taken from Project Gutenberg. A small number of books will be
downloaded on demand and stored in "~/emacs.d/speed-type". They will only be
downloaded once.


[melpa-link]: https://melpa.org/#/speed-type
[melpa-stable-link]: https://stable.melpa.org/#/speed-type
[melpa-badge]: https://melpa.org/packages/speed-type-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/speed-type-badge.svg
