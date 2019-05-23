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

(setq with-emacs-default-path (getenv "EMACS"))

(ert-deftest with-emacs-test-return-value ()
  (should (equal "\"foo\""
                 (with-default-emacs
                   (message "qux")
                   "bar"
                   "foo")))
  (should (equal "foo"
                 (with-default-emacs
                   (message "qux")
                   "bar"
                   'foo)))
  (should (equal "nil"
                 (with-default-emacs
                   "foo"
                   nil)))
  (should (equal "t"
                 (with-default-emacs
                   "foo"
                   t))))

(ert-deftest with-emacs-test-print-message ()
  (should (equal "\"AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\""
                 (progn
                   (with-default-emacs
                     (message "AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50"))
                   (with-current-buffer "*Messages*"
                     (let ((s (buffer-substring-no-properties (point-min) (point-max))))
                       (message "[%s]" s)
                       (when (string-match "\s*\\(\"AFE1CEFE-622C-4CED-B50E-9C95F2AF5F50\"\\)\s*" s)
                         (match-string 1 s)))
                     )))))

(defun greet (name)
  (message "Hello, %s" name))

(ert-deftest with-emacs-test-scope-isolate ()
 (should
  (equal "\"Hi, Tom\""
         (with-default-emacs
           (defun greet (name)
             (message "Hi, %s" name))
           (greet "Tom"))))
 (should
  (equal "Hello, Tom"
         (greet "Tom"))))

(provide 'with-emacs-test)

;;; with-emacs-test.el ends here
