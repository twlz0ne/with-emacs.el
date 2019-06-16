;;; with-emacs-test.el --- Test with-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'with-emacs)

(when noninteractive
  (transient-mark-mode))

(ert-deftest with-emacs-test-return-string ()
  (should (equal "foo"
                 (with-emacs
                   "foo"))))

(ert-deftest with-emacs-test-return-string-with-newline ()
  (should (equal "foo\n"
                 (with-emacs
                   "foo\n"))))

(ert-deftest with-emacs-test-return-symbol ()
  (should (equal 'foo
                 (with-emacs
                   'foo))))

(ert-deftest with-emacs-test-return-nil ()
  (should (equal nil
                 (with-emacs
                   nil))))

(ert-deftest with-emacs-test-return-t ()
  (should (equal t
                 (with-emacs
                   t))))

(ert-deftest with-emacs-test-print-message ()
  (should (equal "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"
                 (progn
                   (with-emacs
                     (message "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"))
                   (with-current-buffer "*Messages*"
                     (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                       (when (string-match "\s*\\(AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\\)\s*" s)
                         (match-string 1 s))))))))

(ert-deftest with-emacs-test-print-message-while-return-string ()
  (should (equal "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"
                 (progn
                   (with-emacs
                     (defun foo ()
                       (message "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50")
                       "foo")
                     (foo))
                   (with-current-buffer "*Messages*"
                     (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                       (message "==> [%S]" s)
                       (when (string-match "\s*\\(AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\\)\s*" s)
                         (match-string 1 s))))))))

(ert-deftest with-emacs-test-print-message-while-return-symbol ()
  (should (equal "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"
                 (progn
                   (with-emacs
                     (defun foo ()
                       (message "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50")
                       'foo)
                     (foo))
                   (with-current-buffer "*Messages*"
                     (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                       (message "==> [%S]" s)
                       (when (string-match "\s*\\(AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\\)\s*" s)
                         (match-string 1 s))))))))

(ert-deftest with-emacs-test-print-message-while-return-nil ()
  (should (equal "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"
                 (progn
                   (with-emacs
                     (defun foo ()
                       (message "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50")
                       nil)
                     (foo))
                   (with-current-buffer "*Messages*"
                     (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                       (message "==> [%S]" s)
                       (when (string-match "\s*\\(AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\\)\s*" s)
                         (match-string 1 s))))))))

(ert-deftest with-emacs-test-print-message-while-return-t ()
  (should (equal "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"
                 (progn
                   (with-emacs
                     (defun foo ()
                       (message "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50")
                       t)
                     (foo))
                   (with-current-buffer "*Messages*"
                     (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                       (message "==> [%S]" s)
                       (when (string-match "\s*\\(AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\\)\s*" s)
                         (match-string 1 s))))))))

(ert-deftest with-emacs-test-lexical-t ()
  (should (equal 4
                 (with-emacs
                   :lexical t
                   (defun fun-fun (f)
                     (lambda (x) (funcall f x)))
                   (funcall (fun-fun #'1+) 3)))))

(ert-deftest with-emacs-test-lexical-nil ()
  (should (equal "Symbol’s value as variable is void: f\n"
                 (condition-case err
                     (with-emacs
                       :lexical nil
                       (defun fun-fun (f)
                         (lambda (x) (funcall f x)))
                       (funcall (fun-fun #'1+) 3))
                   (error (replace-regexp-in-string "'" "’" (cadr err)))))))

(defun greet (name)
  (message "Hello, %s" name))

(ert-deftest with-emacs-test-scope-isolate ()
 (should
  (equal "Hi, Tom"
         (with-emacs
           (defun greet (name)
             (message "Hi, %s" name))
           (greet "Tom"))))
 (should
  (equal "Hello, Tom"
         (greet "Tom"))))

(provide 'with-emacs-test)

;;; with-emacs-test.el ends here
