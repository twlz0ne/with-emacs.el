;;; with-emacs.el --- Evaluate expressions in a separate Emacs process -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/20
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/with-emacs.el
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evaluate expressions in a separate Emacs process:
;;
;; ```elisp
;; (with-emacs "/path/to/version/emacs"
;;   (do-something)
;;   ...)
;; ```
;;
;; See README for more information.

;;; Change Log:

;;
;; 0.2.0  2019/06/05
;;
;;   Add function -extract-return-value to replace the regexp to get more
;;   accurate result and remove the outer double quotes from return value.
;;
;; 0.1.0  2019/04/20
;;
;;   Initial version.
;;

;;; Code:

(require 'cl-lib)

(defcustom with-emacs-executable-path (concat invocation-directory invocation-name)
  "Location of Emacs executable."
  :type 'string
  :group 'with-emacs)

(defcustom with-emacs-lexical-binding nil
  "Whether to use lexical binding when evaluating code."
  :type 'boolean
  :group 'with-emacs)

(defcustom with-emacs-output-regexp
  (rx string-start
      (0+ "\n")
      ;; Text that need to be redirected to the `*Messages*' buffer
      (group (0+ anything))
      "\n"
      ;; Result of a expression
      (or (group (0+ (not (any "\n")))) ;; match a symbol
          ;; match a quoted string, e.g. "foo\"bar\""
          (group "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\""))
      string-end)
  "Regexp for extracting message or result from output."
  :type 'string
  :group 'with-emacs)

(defun with-emacs--extract-return-value (s)
  "Extract return value from string S."
  (with-temp-buffer
    (insert s)
    (emacs-lisp-mode)
    (goto-char (point-max))
    (sexp-at-point)))

(cl-defmacro with-emacs (&rest body &key path lexical &allow-other-keys)
  "Start a emacs in a subprocess, and execute BODY there.
If PATH not set, use `with-emacs-executable-path'.
If LEXICAL not set, use `with-emacs-lexical-binding.'"
  (declare (indent defun) (debug t))
  (let ((has-path? (and (plist-member body :path) t))
        (has-lexical? (and (plist-member body :lexical) t)))
    (when has-lexical?
      (setq body (cddr body)))
    (when has-path?
      (setq body (cddr body)))
    `(let* ((process-connection-type nil)
            (eoe-indicator "with-emacs-eoe")
            (comint-prompt-regexp "Lisp expression: ")
            (cmdlist '(,(or path with-emacs-executable-path)
                       "--batch"
                       "--eval"
                       ,(format "(setq lexical-binding %s)" (if has-lexical? lexical with-emacs-lexical-binding))
                       "--eval"
                       ,(format "%s" '(while t (prin1 (eval (read) lexical-binding))))))
            (pbuf ,(current-buffer))
            (output nil)
            (error-msg nil)
            (comint-output-filter-functions
             (lambda (text)
               ;; (message "==> text: %s" text)
               (setq output (concat output text))))
            (buf (apply 'make-comint-in-buffer "elpl"
                        (generate-new-buffer-name "*elpl*")
                        (car cmdlist) nil (cdr cmdlist)))
            (proc (get-buffer-process buf)))

       ;; -------------------
       ;; Execute expressions
       ;; -------------------

       (with-current-buffer buf
         (delete-region (point-min) (point-max))
         (let ((print-escape-newlines t))
           (mapcar (lambda (it)
                     (insert (format "%S" it))
                     (comint-send-input))
                   ',body))

         ;; Finish
         (process-send-string proc (format "%S\n" eoe-indicator))

         ;; Waiting for output
         (while (and (eq 'run (process-status proc))
                     (progn
                       (goto-char comint-last-input-end)
                       (not
                        (save-excursion
                          (re-search-forward
                           (regexp-quote eoe-indicator) nil t)))))
           (accept-process-output (get-buffer-process (current-buffer)))
           (sit-for 0.1))

         ;; Normal exist
         (when (eq 'run (process-status proc))
           (process-send-string proc "(kill-emacs)\n"))

         ;; Waiting for the process exiting
         (while (not (memq (process-status proc) '(exit signal)))
           (sit-for 0.1))

         ;; Handle abnormal exist
         (when (< 0 (process-exit-status proc))
           (setq error-msg
                 (save-excursion
                   (goto-char comint-last-output-start)
                   (save-restriction
                     (narrow-to-region comint-last-output-start
                                       (goto-char (next-property-change (point))))
                     (buffer-substring-no-properties (point-min) (point-max)))))))

       ;; --------------
       ;; Process result
       ;; --------------

       (let ((kill-buffer-query-functions nil))
         (kill-buffer buf))

       (if error-msg
           (signal 'error (list error-msg))
         (when output
           (let* ((strs (split-string output comint-prompt-regexp))
                  (ret (car (cddr (reverse strs)))))
             ;; Redirect message to `*Messages*'
             (mapc (lambda (s)
                     ;; (message "s: [%S]" s) ;; debug
                     (when (string-match with-emacs-output-regexp s)
                       (message (match-string 1 s))))
                   strs)
             ;; Return the result of the last expression as a string
             (with-emacs--extract-return-value ret)))))))

(provide 'with-emacs)

;;; with-emacs.el ends here
