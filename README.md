[![CI](https://github.com/twlz0ne/with-emacs.el/workflows/CI/badge.svg)](https://github.com/twlz0ne/with-emacs.el/actions?query=workflow%3ACI)
[![MELPA](https://melpa.org/packages/with-emacs-badge.svg)](https://melpa.org/#/with-emacs)

# with-emacs.el

Evaluate expressions in a separate Emacs process.

## Installation

Clone this repository, or install from MELPA. Add the following to your `.emacs`:

```elisp
(require 'with-emacs)
```

## Usage

### with-emacs `(&key PATH LEXICAL &rest BODY)`

Start a emacs in a subprocess, and execute BODY there.

If PATH not set, use `with-emacs-executable-path'.\
If LEXICAL not set, use `with-emacs-lexical-binding.'

```elisp
;; Evaluate expressions in a separate Emacs.
(with-emacs ...)

;; Specify the version of Emacs and enable lexical binding.
(with-emacs :path "/path/to/{version}/emacs" :lexical t ...)
```

### with-emacs-server `(SERVER &key ENSURE TIMEOUT &rest BODY)`

Contact the Emacs server named SERVER and evaluate BODY there.

If ENSURE not nil, start a server when necessary. It can be t or
a path of emacs, if it is t, use `with-emacs-executable-path' as default.

The server will be killed after TIMEOUT minutes, if TIMEOUT not given,
use `with-emacs-server-timeout` as default, if TIMEOUT is nil,
disable timeout timer.

```elisp
;; Evaluate expressions in server "name" or signal an error if no such server. 
(with-emacs-server "name" ...)

;; Evaluate expressions in server "name" and start a server if necessary. 
(with-emacs-server "name" :ensure t ...)
(with-emacs-server "name" :ensure "/path/to/{version}/emacs" ...)

;; Kill server after 100 minutes of idle
(with-emacs-server "name" :ensure t :timeout 100 ...)

;; Set default timeout for every new server:
(setq with-emacs-server-timeout 100)
(with-emacs-server "name" :ensure t ...)

;; Disable default timeout temporary:
(with-emacs-server "name" :ensure t :timeout nil ...)
```

### with-emacs-define-partially-applied `(&rest ARGS)`

Generate functions that are partial application of `with-emacs` to ARGS.

The form of ARGS is:

    (part-name1 emacs-path1 lexical-or-not)
    (part-name2 emacs-path2 lexical-or-not)
    ...

For example:

```elisp
(with-emacs-define-partially-applied
 (t      nil t)
 (24.3   "/path/to/emacs-24.3")
 (24.4-t "/path/to/emacs-24.4" t))
```

Will creates following new functions:

```elisp
(with-emacs-t &key PATH &rest BODY)
;; Equivalent to:
;; (with-emacs :lexical t ...)

(with-emacs-24.3 &key LEXICAL &rest BODY)
;; Equivalent to:
;; (with-emacs :path "/path/to/emacs-24.3" ...)

(with-emacs-24.4-t &rest BODY)
;; Equivalent to:
;; (with-emacs :lexical t :path "/path/to/emacs-24.4" ...)
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
