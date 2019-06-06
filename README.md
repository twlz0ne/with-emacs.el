[![Build Status](https://travis-ci.com/twlz0ne/with-emacs.el.svg?branch=master)](https://travis-ci.com/twlz0ne/with-emacs.el)

# with-emacs.el

Evaluate expressions in a separate Emacs process.

## Installation

Clone this repository to `~/.emacs.d/site-lisp/with-emacs`. Add the following to your `.emacs`:

```elisp
(require 'with-emacs)
```

## Usage

```elisp
;; Use the same version of current Emacs
(with-emacs 
  (do-something)
  ...)

;; Use the specified version of Emacs
(with-emacs :path "/path/to/version/emacs"
  (do-something)
  ...)
```

## Examples

### Determine if a function exists in a specific version of Emacs

```elisp
(with-emacs :path "/Applications/Emacs-24.3.app/Contents/MacOS/Emacs"
  (fboundp 'string-suffix-p))
;; => nil
```

### Get doc string from a specific version of Emacs

```elisp
(with-emacs :path "/Applications/Emacs-24.4.app/Contents/MacOS/Emacs"
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
