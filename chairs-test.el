;;; chairs-test.el --- Tests for chairs.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x) ;; for when-let*
(load-file (expand-file-name "chairs.el" default-directory))

;;; Helper functions for testing

(defmacro chairs-test-with-temp-buffer (content &rest body)
  "Create a temp buffer with CONTENT and execute BODY.
The | character marks the cursor position."
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,content)
     (goto-char (point-min))
     (when (search-forward "|" nil t)
       (delete-char -1))
     ,@body))

(defun chairs-test-get-buffer-string ()
  "Get buffer string with | marking cursor position."
  (concat
   (buffer-substring-no-properties (point-min) (point))
   "|"
   (buffer-substring-no-properties (point) (point-max))))

;;; Tests for helper functions

(ert-deftest chairs-test-get-closing-from-opening ()
  "Test getting closing pair from opening char."
  (should (= (chairs--get-closing-from-opening ?\() ?\)))
  (should (= (chairs--get-closing-from-opening ?\[) ?\]))
  (should (= (chairs--get-closing-from-opening ?\{) ?\}))
  (should (= (chairs--get-closing-from-opening ?\<) ?\>))
  (should (= (chairs--get-closing-from-opening ?\") ?\"))
  (should (= (chairs--get-closing-from-opening ?\') ?\'))
  (should-not (chairs--get-closing-from-opening ?a)))

(ert-deftest chairs-test-get-opening-from-closing ()
  "Test getting opening pair from closing char."
  (should (= (chairs--get-opening-from-closing ?\)) ?\())
  (should (= (chairs--get-opening-from-closing ?\]) ?\[))
  (should (= (chairs--get-opening-from-closing ?\}) ?\{))
  (should (= (chairs--get-opening-from-closing ?\>) ?\<))
  (should (= (chairs--get-opening-from-closing ?\") ?\"))
  (should (= (chairs--get-opening-from-closing ?\') ?\'))
  (should-not (chairs--get-opening-from-closing ?a)))

(ert-deftest chairs-test-pairs ()
  "Test pairs generation."
  (should (equal (chairs--pairs ?\() '("(" . ")")))
  (should (equal (chairs--pairs ?\)) '("(" . ")")))
  (should (equal (chairs--pairs ?\[) '("[" . "]")))
  (should (equal (chairs--pairs ?\]) '("[" . "]")))
  (should (equal (chairs--pairs ?\") '("\"" . "\"")))
  ;; Unknown char should return itself
  (should (equal (chairs--pairs ?a) '("a" . "a"))))

(ert-deftest chairs-test-mode-specific-pairs ()
  "Test mode-specific closing pairs."
  (with-temp-buffer
    (emacs-lisp-mode)
    (should (= (chairs--get-closing-from-opening ?\`) ?\')))
  (with-temp-buffer
    (text-mode)
    (should (= (chairs--get-closing-from-opening ?\`) ?\`))))

(defmacro chairs-test-with-input (keys &rest body)
  "Execute BODY with KEYS as simulated input."
  (declare (indent 1))
  `(let ((unread-command-events (append ,keys nil)))
     ,@body))

;;; Tests for main commands

;; Test for rewrap
(ert-deftest chairs-test-rewrap-parens-to-brackets ()
  "Test rewrapping parentheses to brackets."
  (chairs-test-with-temp-buffer "(foo-|bar)"
    (chairs-test-with-input (list ?\[)
      (chairs-rewrap)
      (should (equal (chairs-test-get-buffer-string) "[foo-|bar]")))))

(ert-deftest chairs-test-rewrap-brackets-to-parens ()
  "Test rewrapping brackets to parentheses."
  (chairs-test-with-temp-buffer "[foo-|bar]"
    (chairs-test-with-input (list ?\()
      (chairs-rewrap)
      (should (equal (chairs-test-get-buffer-string) "(foo-|bar)")))))

(ert-deftest chairs-test-rewrap-quotes-to-parens ()
  "Test rewrapping quotes to parentheses."
  (chairs-test-with-temp-buffer "\"foo-|bar\""
    (chairs-test-with-input (list ?\()
      (chairs-rewrap)
      (should (equal (chairs-test-get-buffer-string) "(foo-|bar)")))))

(ert-deftest chairs-test-rewrap-braces-to-parens ()
  "Test rewrapping braces to parentheses."
  (chairs-test-with-temp-buffer "{foo-|bar}"
    (chairs-test-with-input (list ?\()
      (chairs-rewrap)
      (should (equal (chairs-test-get-buffer-string) "(foo-|bar)")))))

(ert-deftest chairs-test-rewrap-nested-quotes ()
  "Test rewrapping quotes in nested expression."
  (chairs-test-with-temp-buffer "(cons \"fo|o-bar\" baz)"
    (chairs-test-with-input (list ?\()
      (chairs-rewrap)
      (should (equal (chairs-test-get-buffer-string) "(cons (fo|o-bar) baz)")))))

;; test for add
(ert-deftest chairs-test-add-parens-multiple ()
  "Test adding parentheses with multiple test cases."
  (let ((test-cases
         '(;; (input expected description)
           ("foo-|bar" "foo-(|bar)" "simple symbol")
           ("|foo-bar" "(|foo)-bar" "cursor at beginning")
           ("foo-bar|" "foo-(bar|)" "cursor at end")
           ("|(test-123)" "(|(test-123))" "sexp"))))
    (dolist (case test-cases)
      (let ((input (nth 0 case))
            (expected (nth 1 case))
            (description (nth 2 case)))
        (chairs-test-with-temp-buffer input
          (chairs-test-with-input (list ?\()
            (chairs-add)
            (should (equal (chairs-test-get-buffer-string) expected))))))))

(ert-deftest chairs-test-add-brackets ()
  "Test adding brackets around sexp."
  (chairs-test-with-temp-buffer "foo-|bar"
    (chairs-test-with-input (list ?\[)
      (chairs-add)
      (should (equal (chairs-test-get-buffer-string) "foo-[|bar]")))))

(ert-deftest chairs-test-add-braces ()
  "Test adding braces around sexp."
  (chairs-test-with-temp-buffer "foo-|bar"
    (chairs-test-with-input (list ?\{)
      (chairs-add)
      (should (equal (chairs-test-get-buffer-string) "foo-{|bar}")))))

(ert-deftest chairs-test-add-quotes ()
  "Test adding quotes around sexp."
  (chairs-test-with-temp-buffer "foo|bar"
    (chairs-test-with-input (list ?\")
      (chairs-add)
      (should (equal (chairs-test-get-buffer-string) "\"foo|bar\"")))))

(ert-deftest chairs-test-add-in-nested-sexp ()
  "Test adding pairs in nested expression."
  (chairs-test-with-temp-buffer "(cons \"foo-bar\" |'bar-foo)"
    (chairs-test-with-input (list ?\()
      (chairs-add)
      (should (string-match-p "('bar-foo)" (buffer-string))))))

;;; Tests for overlay management

(ert-deftest chairs-test-overlay-cleanup-after-rewrap ()
  "Test that overlays are properly cleaned up after rewrap."
  (chairs-test-with-temp-buffer "(foo-|bar)"
    (chairs-test-with-input (list ?\[)
      (chairs-rewrap)
      (should (null chairs--overlay-list)))))

(ert-deftest chairs-test-overlay-cleanup-after-add ()
  "Test that overlays are properly cleaned up after add."
  (chairs-test-with-temp-buffer "foo-|bar"
    (chairs-test-with-input (list ?\()
      (chairs-add)
      (should (null chairs--overlay-list)))))

(provide 'chairs-test)
;;; chairs-test.el ends here
