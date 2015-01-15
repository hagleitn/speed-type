# speed-type
Practice touch/speed typing in emacs

## Installation

Put speed-type.el into a directory specified by the load-path variable. Alternatively, you can also add a directory to the variable load-path by (add-to-list 'load-path "ADDITIONAL-DIRECTORY").

If you put the file in "~/.emacs.d/speed-type/speed-type.el" for instance, the following snipped in your .emacs file will load and init the extension. 

```lisp
(add-to-list 'load-path "~/.emacs.d/speed-type/speed-type.el")
(require 'speed-type)
```

## Running speed-type

Executing M-x speed-type-text will start the typing exercise. A new buffer will open and a random text sample will appear. As you type the text it will change color to show progress and higlight correct and incorrect entries. Timing happens automatically, the clock starts on the first character typed and ends with the last. Statistics like characters typed, words-per-minute, and total time will be shown as soon as the last character is entered.

You can use any buffer or part of it to run speed-type. M-x speed-type-region and M-x speed-type-buffer will do the same thing as speed-type-text, except they take the text sample you've picked.

Random samples are taken from Project Gutenberg. A small number of books will be downloaded on demand and stored in "~/emacs.d/speed-type". They will only be downloaded once.
