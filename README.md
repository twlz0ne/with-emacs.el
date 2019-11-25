[![Build Status](https://travis-ci.com/twlz0ne/with-emacs.el.svg?branch=master)](https://travis-ci.com/twlz0ne/with-emacs.el)
[![MELPA](https://melpa.org/packages/with-emacs-badge.svg)](https://melpa.org/#/with-emacs)

# with-emacs.el

Evaluate expressions in a separate Emacs process.

## Installation

Clone this repository, or install from MELPA. Add the following to your `.emacs`:

```elisp
(require 'with-emacs)
```

## Usage

### with-emacs

```elisp
;; Evaluate expressions in a separate Emacs.
(with-emacs ...)

;; Specify the version of Emacs and enable lexical binding.
(with-emacs :path "/path/to/{version}/emacs" :lexical t ...)

;; Use partially applied function (see `with-emacs-define-partially-applied` for more)
;; instead of writting verry long parameter each time:
(with-emacs-nightly-t ...)
;; Equaivalent to:
;; (with-emacs :path "/path/to/nightly/emacs" :lexical t ...)
```

### with-emacs-server

```elisp
;; Evaluate expressions in server "name" or signal an error if no such server. 
(with-emacs-server "name" ...)

;; Evaluate expressions in server "name" and start a server if necessary. 
(with-emacs-server "name" :ensure t ...)
(with-emacs-server "name" :ensure "/path/to/{version}/emacs" ...)
```

## Examples

### Determine if a function exists in a specific version of Emacs

```elisp
(with-emacs-24.3
  (fboundp 'string-suffix-p))
;; => nil
```

### Get doc string from a specific version of Emacs

```elisp
(with-emacs-24.4
  (unless (fboundp 'elisp--company-doc-buffer)
    (defun elisp--company-doc-buffer (str)
      (let ((symbol (intern-soft str)))
        ;; FIXME: we really don't want to "display-buffer and then undo it".
        (save-window-excursion
          ;; Make sure we don't display it in another frame, otherwise
          ;; save-window-excursion won't be able to undo it.
          (let ((display-buffer-overriding-action
                 '(nil . ((inhibit-switch-frame . t)))))
            (ignore-errors
              (cond
               ((fboundp symbol) (describe-function symbol))
               ((boundp symbol) (describe-variable symbol))
               ((featurep symbol) (describe-package symbol))
               ((facep symbol) (describe-face symbol))
               (t (signal 'user-error nil)))
              (help-buffer)
              ))))))
  ;; avoid help buttons
  (defun help-window-display-message (quit-part window &optional scroll) nil)
  (message "%s" (with-current-buffer (elisp--company-doc-buffer "assoc")
                  (buffer-substring-no-properties (point-min)
                                                  (point-max)))))
;; => 
;; "assoc is a built-in function in `C source code'.
;;
;; (assoc KEY LIST)
;;
;; Return non-nil if KEY is `equal' to the car of an element of LIST.
;; The value is actually the first element of LIST whose car equals KEY.
;; "
```

### Run test code in isolated scope

```elisp
(defun greet (name)
  (message "Hello, %s" name))

(with-emacs
  (defun greet (name)
    (message "Hi, %s" name))
  (greet "Tom"))
;; => "Hi, Tom"

(greet "Tom")
;; => "Hello, Tom"
```
