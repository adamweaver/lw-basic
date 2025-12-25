# Cromulent BASIC interpreter
BASIC code is translated into FORTH, since FORTH is quite a nice 
virtual machine to work in. 

The FORTH itself isn't intended for public use, since it's rather
idiosyncratic and intentionally different to actual FORTHs in the
wild.

To support sparse "arrays" like PHP, we have a dictionary-like data
structure called a `keymap` which supports efficient array-like 
insertion (and mapping) as well as arbitrary key values.

However BASIC is *stringly typed* so non-integer `keymap` keys are 
first converted to strings before insertion.

## BASIC
``` common-lisp
(BASIC:PARSE string)
```

## FORTH
``` common-lisp
(FORTH:RUN (FORTH:COMPILE-FORTH (BASIC:PARSE string)))
```

## KEYMAP

``` common-lisp
(MAKE-KEYMAP size-hint-or-dictionary)
(KEYMAP key keymap)
(SETF (KEYMAP key keymap) value)
(KEYMAP-MAP (lambda (val) (frob val)) keymap)
(KEYMAP-MAP (lambda (val idx) (frob val idx)) keymap :index t)
```
