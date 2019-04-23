;;; with-emacs.el --- Evaluate expressions in a separate Emacs process -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/04/20
;; Version: 0.1.0
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

;;  0.1.0  2019/04/20  Initial version.

;;; Code:

(defmacro with-emacs (path &rest body)
  "Start a emacs in a subprocess, and execute BODY there.
PATH is the abs path for emacs."
  `(let* ((process-connection-type nil)
          (eoe-indicator "with-emacs-eoe")
          (comint-prompt-regexp "Lisp expression: ")
          (cmdlist '(,(or path "emacs")
                     "--batch"
                     "--eval"
                     ,(format "%s" '(while t (prin1 (eval (read)))))))
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
                 '(,@body)))

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
                (ret (car (cddr (reverse strs))))
                (rx "\n\\(\\\".*\\\"\\|nil\\)\\'"))
           ;; Redirect message to `*Messages*'
           (mapc (lambda (s)
                   (when (string-match rx s)
                     (message (match-string 1 s))))
                 strs)
           ;; Return the result of the last expression as a string
           (if (string-match rx ret)
               (match-string 1 ret)
             ret)
           )))))

(defmacro with-default-emacs (&rest body)
  "An alias for `(with-emacs nil ...)'."
  (declare (indent defun) (debug t))
  `(with-emacs nil
     ,@body))

(provide 'with-emacs)

;;; with-emacs.el ends here
