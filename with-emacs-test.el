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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(ert-deftest with-emacs-test-raeturn-encoded-url ()
  (should
   (equal (concat (url-encode-url "https://en.wikipedia.org/wiki/René_Descartes") "\n")
          (with-emacs
            (concat (url-encode-url "https://en.wikipedia.org/wiki/René_Descartes") "\n")))))

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
  (let ((error-string
         (condition-case err
             (with-emacs
               :lexical nil
               (progn
                 (defun fun-fun (f)
                   (lambda (x) (funcall f x)))
                 (funcall (fun-fun #'1+) 3)))
           (error (replace-regexp-in-string "'" "’" (cadr err))))))
    (should (or (string-prefix-p "Symbol’s value as variable is void: f\n"
                                 error-string)
                ;; 28.1
                (string-prefix-p "Debugger entered--Lisp error: (void-variable f)\n"
                                 error-string))))

  (defun greet (name)
    (message "Hello, %s" name)))

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

(ert-deftest with-emacs-test-server ()
  (should
   (equal "No such server: 44f346a5-a85d-940c-b2ca-9f9b5193ee5e"
          (condition-case err
              (with-emacs-server "44f346a5-a85d-940c-b2ca-9f9b5193ee5e" 1)
            (error
             (cadr err)))))
  (should
   (equal 2 (with-emacs-server "44f346a5-a85d-940c-b2ca-9f9b5193ee5e" :ensure t 2)))
  (should
   (equal "No such server: 44f346a5-a85d-940c-b2ca-9f9b5193ee5e"
          (condition-case err
              (progn
                (with-emacs-server "44f346a5-a85d-940c-b2ca-9f9b5193ee5e" (kill-emacs))
                (with-emacs-server "44f346a5-a85d-940c-b2ca-9f9b5193ee5e" 3))
            (error
             (cadr err))))))

(ert-deftest with-emacs-test-server-timeout ()
  (message "Waiting for server1 timeout...")
  (should
   (equal 1 (with-emacs-server "536d7f14-f4eb-661b-4087-f5c3c54e895b" :ensure t :timeout 0.2 1)))
  (dotimes (_ 13)
    (sit-for 1))
  (equal "No such server: 536d7f14-f4eb-661b-4087-f5c3c54e895b"
          (condition-case err
              (with-emacs-server "536d7f14-f4eb-661b-4087-f5c3c54e895b" 1)
            (error
             (cadr err))))
  (message "Waiting for server2 timeout...")
  (let ((with-emacs-server-timeout 0.2))
    (equal 1 (with-emacs-server "ea3803d6-aa6d-4b38-f088-daf2d331bdc2" :ensure t 1))
    (dotimes (_ 13)
      (sit-for 1))
    (equal "No such server: ea3803d6-aa6d-4b38-f088-daf2d331bdc2"
           (condition-case err
               (with-emacs-server "ea3803d6-aa6d-4b38-f088-daf2d331bdc2" 1)
             (error
              (cadr err))))))

(ert-deftest with-emacs-test-control-characters ()
  (should (string= "\r\n" (with-emacs "\r\n"))))

(ert-deftest with-emacs-test-debug-on-error ()
  (should
   (string-prefix-p "Debugger entered--Lisp error:"
                    (condition-case err
                        (with-emacs
                          (toggle-debug-on-error)
                          (error "test debug on error"))
                      (error (cadr err))))))

(provide 'with-emacs-test)

;;; with-emacs-test.el ends here
