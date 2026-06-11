;;; magit-psr-test.el --- Tests for magit-psr  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: kj <webmaste@outcontrol.net>

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

;; ERT tests for magit-psr.  Run with:
;;   emacs -batch -l ert -l test/magit-psr-test.el                  \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;;; Mock magit dependencies for batch testing

(defvar magit-root-section nil)
(defvar magit-psr-section-heading "PSR")
(defvar magit-psr-section-map (make-sparse-keymap))
(defvar magit-section-show-child-count nil)
(defvar magit-psr-mode nil)

(defun magit-git-string (&rest _args) "")
(defun magit-call-git (&rest _args) 0)
(defun magit-toplevel () default-directory)
(defun magit-section-match (&rest _) nil)
(defun magit-get-section (&rest _) nil)
(defun magit-current-section () nil)
(defun magit-insert-section (_ &rest _) nil)
(defun magit-insert-heading (&rest _) (insert "heading\n"))
(defun magit-section-hide (_) nil)
(defun magit-section-show (_) nil)
(defun magit-add-section-hook (_hook _fn &optional _at _type) nil)
(defun magit-visit-thing () nil)

(define-error 'magit-user-error "Magit user error")

;; Provide the 'magit feature so (require 'magit) succeeds
(provide 'magit)

;; Load the package under test
(let ((load-path (cons (file-name-directory
                        (or load-file-name default-directory))
                       load-path)))
  (require 'magit-psr (expand-file-name "../magit-psr" (file-name-directory
                                                         (or load-file-name
                                                             default-directory)))))

;;; JSON parsing helpers

(ert-deftest magit-psr-json-read-json-parse-string ()
  "Test `magit-psr--json-read' with native json-parse-string."
  (skip-unless (fboundp 'json-parse-string))
  (let ((result (magit-psr--json-read "{\"key\": \"value\", \"num\": 42}")))
    (should (hash-table-p result))
    (should (equal (gethash "key" result) "value"))
    (should (equal (gethash "num" result) 42))))

(ert-deftest magit-psr-json-read-nested ()
  "Test `magit-psr--json-read' with nested objects."
  (skip-unless (fboundp 'json-parse-string))
  (let ((result (magit-psr--json-read
                 "{\"files\": {\"file.php\": {\"messages\": [{\"line\": 1}]}}}")))
    (should (hash-table-p result))
    (let ((files (gethash "files" result)))
      (should (hash-table-p files))
      (let ((file.php (gethash "file.php" files)))
        (should (hash-table-p file.php))
        (let ((msgs (gethash "messages" file.php)))
          (should (consp msgs))
          (should (hash-table-p (car msgs)))
          (should (equal (gethash "line" (car msgs)) 1)))))))

(ert-deftest magit-psr-json-read-array ()
  "Test `magit-psr--json-read' with arrays."
  (skip-unless (fboundp 'json-parse-string))
  (let ((result (magit-psr--json-read "[1, 2, 3]")))
    (should (equal result '(1 2 3)))))

(ert-deftest magit-psr-json-read-empty ()
  "Test `magit-psr--json-read' with empty input."
  (should-error (magit-psr--json-read "")))

(ert-deftest magit-psr-json-read-invalid ()
  "Test `magit-psr--json-read' with invalid JSON."
  (should-error (magit-psr--json-read "{invalid}")))

(ert-deftest magit-psr-json-get-hash-table ()
  "Test `magit-psr--json-get' on hash tables."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "foo" "bar" ht)
    (puthash "num" 42 ht)
    (should (equal (magit-psr--json-get ht "foo") "bar"))
    (should (equal (magit-psr--json-get ht "num") 42))
    (should (equal (magit-psr--json-get ht "missing") nil))))

(ert-deftest magit-psr-json-get-alist-with-strings ()
  "Test `magit-psr--json-get' on alists with string keys."
  (let ((alist '(("foo" . "bar") ("num" . 42))))
    (should (equal (magit-psr--json-get alist "foo") "bar"))
    (should (equal (magit-psr--json-get alist "num") 42))
    (should (equal (magit-psr--json-get alist "missing") nil))))

(ert-deftest magit-psr-json-get-alist-with-symbols ()
  "Test `magit-psr--json-get' on alists with symbol keys (json.el compat)."
  (let ((alist '((foo . "bar") (num . 42))))
    (should (equal (magit-psr--json-get alist "foo") "bar"))
    (should (equal (magit-psr--json-get alist "num") 42))))

(ert-deftest magit-psr-json-get-nil ()
  "Test `magit-psr--json-get' with nil object."
  (should (equal (magit-psr--json-get nil "key") nil)))

(ert-deftest magit-psr-json-each-hash-table ()
  "Test `magit-psr--json-each' on hash tables."
  (let ((ht (make-hash-table :test 'equal))
        (keys nil)
        (vals nil))
    (puthash "a" 1 ht)
    (puthash "b" 2 ht)
    (magit-psr--json-each ht (lambda (k v) (push k keys) (push v vals)))
    (should (equal (sort keys #'string<) '("a" "b")))
    (should (equal (sort vals #'<) '(1 2)))))

(ert-deftest magit-psr-json-each-alist ()
  "Test `magit-psr--json-each' on alists."
  (let ((alist '(("a" . 1) ("b" . 2)))
        (keys nil)
        (vals nil))
    (magit-psr--json-each alist (lambda (k v) (push k keys) (push v vals)))
    (should (equal (sort keys #'string<) '("a" "b")))
    (should (equal (sort vals #'<) '(1 2)))))

(ert-deftest magit-psr-json-each-empty ()
  "Test `magit-psr--json-each' on empty collections."
  (let ((ht (make-hash-table :test 'equal))
        (called nil))
    (magit-psr--json-each ht (lambda (_k _v) (setq called t)))
    (should (not called))
    (let ((called2 nil))
      (magit-psr--json-each '() (lambda (_k _v) (setq called2 t)))
      (should (not called2)))))

;;; magit-psr-item struct

(ert-deftest magit-psr-item-creation ()
  "Test creation of `magit-psr-item' struct."
  (let ((item (make-magit-psr-item
               :filename "src/file.php"
               :line 42
               :column 5
               :message "Expected newline after opening brace"
               :source "PSR12.Classes.ClassDeclaration"
               :severity 5
               :type "ERROR")))
    (should (magit-psr-item-p item))
    (should (equal (magit-psr-item-filename item) "src/file.php"))
    (should (equal (magit-psr-item-line item) 42))
    (should (equal (magit-psr-item-column item) 5))
    (should (equal (magit-psr-item-message item)
                   "Expected newline after opening brace"))
    (should (equal (magit-psr-item-source item)
                   "PSR12.Classes.ClassDeclaration"))
    (should (equal (magit-psr-item-severity item) 5))
    (should (equal (magit-psr-item-type item) "ERROR"))))

(ert-deftest magit-psr-item-minimal ()
  "Test creation of `magit-psr-item' with minimal fields."
  (let ((item (make-magit-psr-item
               :filename "test.php"
               :line 1
               :message "Test message")))
    (should (magit-psr-item-p item))
    (should (equal (magit-psr-item-filename item) "test.php"))
    (should (equal (magit-psr-item-line item) 1))
    (should (equal (magit-psr-item-column item) nil))
    (should (equal (magit-psr-item-message item) "Test message"))
    (should (equal (magit-psr-item-source item) nil))
    (should (equal (magit-psr-item-severity item) nil))
    (should (equal (magit-psr-item-type item) nil))))

;;; find-php-files

(ert-deftest magit-psr-find-php-files-basic ()
  "Test `magit-psr--find-php-files' filters .php files from all tracked files."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("README.md" "src/file1.php" "src/file2.php" "Makefile"))))
    (let* ((default-directory "/repo/")
           (files (magit-psr--find-php-files "/repo/")))
      (should (equal files '("src/file1.php" "src/file2.php"))))))

(ert-deftest magit-psr-find-php-files-case-insensitive ()
  "Test `magit-psr--find-php-files' matches .PHP and .php files."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("file.PHP" "file2.Php" "file3.php" "note.txt"))))
    (let* ((default-directory "/repo/")
           (files (magit-psr--find-php-files "/repo/")))
      (should (equal (sort files #'string<)
                     '("file.PHP" "file2.Php" "file3.php"))))))

(ert-deftest magit-psr-find-php-files-empty ()
  "Test `magit-psr--find-php-files' with no PHP files."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("README.md" "Makefile"))))
    (should (equal (magit-psr--find-php-files "/repo/") nil))))

(ert-deftest magit-psr-find-php-files-nil ()
  "Test `magit-psr--find-php-files' when git ls-files returns empty."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () nil)))
    (should (equal (magit-psr--find-php-files "/repo/") nil))))

(ert-deftest magit-psr-find-php-files-exclude-vendor ()
  "Test `magit-psr--find-php-files' excludes vendor/*."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda ()
               '("README.md" "src/App.php" "vendor/autoload.php" "lib/Util.php"))))
    (let ((magit-psr-exclude-globs '("vendor/*" "node_modules/*")))
      (should (equal (magit-psr--find-php-files "/repo/")
                     '("src/App.php" "lib/Util.php"))))))

(ert-deftest magit-psr-find-php-files-depth ()
  "Test `magit-psr--find-php-files' respects depth limit."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda ()
               '("a.php" "b/c.php" "d/e/f.php" "g/h/i/j.php" "Makefile"))))
    (let ((magit-psr-depth 2))
      (should (equal (magit-psr--find-php-files "/repo/")
                     '("a.php" "b/c.php" "d/e/f.php"))))))

(ert-deftest magit-psr-find-php-files-exclude-and-depth ()
  "Test `magit-psr--find-php-files' with both exclude and depth."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda ()
               '("README.md" "src/Controller/Foo.php" "vendor/pkg/Bar.php" "src/Baz.php"))))
    (let ((magit-psr-exclude-globs '("vendor/*"))
          (magit-psr-depth 1))
      (should (equal (magit-psr--find-php-files "/repo/")
                     '("src/Baz.php"))))))

;;; scan with mocked phpcs

(ert-deftest magit-psr-scan-no-files ()
  "Test `magit-psr--scan' with no PHP files."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("README.md" "Makefile"))))
    (should (equal (magit-psr--scan) nil))))

(ert-deftest magit-psr-scan-with-mocked-output ()
  "Test `magit-psr--scan' processes phpcs JSON output correctly."
  (skip-unless (fboundp 'json-parse-string))
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("README.md" "src/App.php")))
            ((symbol-function 'shell-command-to-string)
             (lambda (&rest _)
               (json-encode
                (let ((ht (make-hash-table :test 'equal)))
                  (puthash "files"
                           (let ((fht (make-hash-table :test 'equal)))
                             (puthash "src/App.php"
                                      (let ((mht (make-hash-table :test 'equal)))
                                        (puthash "messages"
                                                 (list
                                                  (let ((msg (make-hash-table :test 'equal)))
                                                    (puthash "line" 15 msg)
                                                    (puthash "column" 3 msg)
                                                    (puthash "message"
                                                             "Spaces must be used for indentation" msg)
                                                    (puthash "source"
                                                             "PSR12.Classes.OpeningBraceSpace" msg)
                                                    (puthash "type" "ERROR" msg)
                                                    msg)
                                                  (let ((msg (make-hash-table :test 'equal)))
                                                    (puthash "line" 20 msg)
                                                    (puthash "column" 1 msg)
                                                    (puthash "message"
                                                             "Expected newline after opening brace" msg)
                                                    (puthash "source"
                                                             "PSR12.Classes.ClassDeclaration" msg)
                                                    (puthash "type" "WARNING" msg)
                                                    msg))
                                                 mht)
                                        mht)
                                      fht)
                             fht)
                           ht)
                  ht)))))
    (let* ((magit-psr-show-warnings nil)
           (items (magit-psr--scan)))
      (should (equal (length items) 1))
      (let ((item (car items)))
        (should (equal (magit-psr-item-filename item) "src/App.php"))
        (should (equal (magit-psr-item-line item) 15))
        (should (equal (magit-psr-item-column item) 3))
        (should (equal (magit-psr-item-message item)
                       "Spaces must be used for indentation"))
        (should (equal (magit-psr-item-type item) "ERROR"))))))

(ert-deftest magit-psr-scan-with-warnings ()
  "Test `magit-psr--scan' includes warnings when `magit-psr-show-warnings' is set."
  (skip-unless (fboundp 'json-parse-string))
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("README.md" "src/App.php")))
            ((symbol-function 'shell-command-to-string)
             (lambda (&rest _)
               (json-encode
                (let ((ht (make-hash-table :test 'equal)))
                  (puthash "files"
                           (let ((fht (make-hash-table :test 'equal)))
                             (puthash "src/App.php"
                                      (let ((mht (make-hash-table :test 'equal)))
                                        (puthash "messages"
                                                 (list
                                                  (let ((msg (make-hash-table :test 'equal)))
                                                    (puthash "line" 5 msg)
                                                    (puthash "column" 1 msg)
                                                    (puthash "message" "Warning msg" msg)
                                                    (puthash "source" "PSR1.Foo" msg)
                                                    (puthash "type" "WARNING" msg)
                                                    msg))
                                                 mht)
                                        mht)
                                      fht)
                             fht)
                           ht)
                  ht)))))
    (let ((magit-psr-show-warnings t))
      (should (equal (length (magit-psr--scan)) 1)))
    (let ((magit-psr-show-warnings nil))
      (should (equal (magit-psr--scan) nil)))))

(ert-deftest magit-psr-scan-bad-json ()
  "Test `magit-psr--scan' handles invalid JSON gracefully."
  (cl-letf (((symbol-function 'magit-psr--git-ls-files)
             (lambda () '("src/App.php")))
            ((symbol-function 'shell-command-to-string)
             (lambda (&rest _) "not valid json")))
    (should (equal (magit-psr--scan) nil))))

;;; changed-files (mocked git)

(ert-deftest magit-psr-git-changed-files-uncommitted ()
  "Test `magit-psr--git-changed-files' with uncommitted changes."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &optional infile destination display &rest args)
               (when (and (string= program "git") (eq destination t))
                 (cond
                  ((equal args '("diff" "--name-only" "HEAD"))
                   (insert "src/new.php\nlib/old.php\n") 0)
                  ((equal args '("ls-files" "--others" "--exclude-standard"))
                   (insert "vendor/test.php\n") 0)
                  ((equal args '("log" "--name-only" "--pretty=format:" "-5" "--diff-filter=AMR"))
                   0)
                  (t 0))))))
    (let ((magit-psr-recent-commits 5))
      (should (equal (sort (magit-psr--git-changed-files "/repo/") #'string<)
                     '("lib/old.php" "src/new.php" "vendor/test.php"))))))

(ert-deftest magit-psr-git-changed-files-with-recent-commits ()
  "Test `magit-psr--git-changed-files' includes recent commits."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &optional infile destination display &rest args)
               (when (and (string= program "git") (eq destination t))
                 (cond
                  ((equal args '("diff" "--name-only" "HEAD"))
                   0)
                  ((equal args '("ls-files" "--others" "--exclude-standard"))
                   0)
                  ((equal args '("log" "--name-only" "--pretty=format:" "-5" "--diff-filter=AMR"))
                   (insert "src/old.php\nsrc/new.php\n") 0)
                  (t 0))))))
    (let ((magit-psr-recent-commits 5))
      (should (equal (sort (magit-psr--git-changed-files "/repo/") #'string<)
                     '("src/new.php" "src/old.php"))))))

(ert-deftest magit-psr-git-changed-files-empty ()
  "Test `magit-psr--git-changed-files' when nothing changed."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &optional infile destination display &rest args)
               (when (and (string= program "git") (eq destination t))
                 0))))
    (let ((magit-psr-recent-commits 5))
      (should (equal (magit-psr--git-changed-files "/repo/") nil)))))

(ert-deftest magit-psr-git-changed-files-zero-commits ()
  "Test `magit-psr--git-changed-files' with recent-commits=0 (no recent)."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &optional infile destination display &rest args)
               (when (and (string= program "git") (eq destination t))
                 (cond
                  ((equal args '("diff" "--name-only" "HEAD"))
                   (insert "src/new.php\n") 0)
                  ((equal args '("ls-files" "--others" "--exclude-standard"))
                   0)
                  ((equal args '("log" "--name-only" "--pretty=format:" "-0" "--diff-filter=AMR"))
                   0)
                  (t 0))))))
    (let ((magit-psr-recent-commits 0))
      (should (equal (magit-psr--git-changed-files "/repo/")
                     '("src/new.php"))))))

(ert-deftest magit-psr-find-php-files-changed ()
  "Test `magit-psr--find-php-files' with recent-commits enabled."
  (cl-letf (((symbol-function 'call-process)
             (lambda (program &optional infile destination display &rest args)
               (when (and (string= program "git") (eq destination t))
                 (cond
                  ((equal args '("diff" "--name-only" "HEAD"))
                   0)
                  ((equal args '("ls-files" "--others" "--exclude-standard"))
                   0)
                  ((equal args '("log" "--name-only" "--pretty=format:" "-3" "--diff-filter=AMR"))
                   (insert "src/App.php\nlib/Util.php\nvendor/pkg/Lib.php\n") 0)
                  (t 0))))))
    (let ((magit-psr-recent-commits 3)
          (magit-psr-exclude-globs '("vendor/*")))
      (should (equal (sort (magit-psr--find-php-files "/repo/") #'string<)
                     '("lib/Util.php" "src/App.php"))))))

;;; Section helpers (no buffer required)

(ert-deftest magit-psr-delete-section-no-section ()
  "Test `magit-psr--delete-section' when no section exists."
  (with-temp-buffer
    (should (equal (magit-psr--delete-section [* psr]) nil))))

;;; Customization

(ert-deftest magit-psr-customization-defaults ()
  "Test default values of customization options."
  (should (equal magit-psr-executable "phpcs"))
  (should (equal magit-psr-standard "PSR12"))
  (should (equal magit-psr-max-items 20))
  (should (equal magit-psr-insert-after '(bottom)))
  (should (equal magit-psr-show-warnings nil))
  (should (equal magit-psr-exclude-globs '("vendor/" "node_modules/")))
  (should (equal magit-psr-depth nil))
  (should (equal magit-psr-phpcs-args nil))
  (should (equal magit-psr-recent-commits nil))
  (should (equal magit-psr-show-placeholder t))
  (should (equal magit-psr-project-config-file nil))
  (should (equal magit-psr-custom-rules nil)))

;;; find-config-file

(ert-deftest magit-psr-find-config-file-not-found ()
  "Test `magit-psr--find-config-file' when no config file exists."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/nonexistent-")))
    (should (equal (magit-psr--find-config-file) nil))))

(ert-deftest magit-psr-find-config-file-auto-detect ()
  "Test `magit-psr--find-config-file' auto-detects known filenames."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/magit-psr-test-dir"))
            ((symbol-function 'file-exists-p)
             (lambda (f) (string-suffix-p "phpcs.xml.dist" f))))
    (let ((magit-psr-project-config-file nil))
      (should (magit-psr--find-config-file))
      (should (string-suffix-p
               "phpcs.xml.dist" (magit-psr--find-config-file))))))

(ert-deftest magit-psr-find-config-file-auto-detect-order ()
  "Test auto-detect order prefers phpcs.xml.dist over phpcs.xml."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/magit-psr-test-dir"))
            ((symbol-function 'file-exists-p)
             (lambda (f) (or (string-suffix-p "phpcs.xml.dist" f)
                              (string-suffix-p "phpcs.xml" f)))))
    (let ((magit-psr-project-config-file nil))
      (should (string-suffix-p
               "phpcs.xml.dist" (magit-psr--find-config-file))))))

(ert-deftest magit-psr-find-config-file-custom-path ()
  "Test `magit-psr--find-config-file' with custom path."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/magit-psr-test-dir"))
            ((symbol-function 'file-exists-p)
             (lambda (f) (string-suffix-p "backend/phpcs.xml" f))))
    (let ((magit-psr-project-config-file "backend/phpcs.xml"))
      (should (magit-psr--find-config-file))
      (should (string-suffix-p
               "backend/phpcs.xml" (magit-psr--find-config-file))))))

(ert-deftest magit-psr-find-config-file-custom-path-not-found ()
  "Test `magit-psr--find-config-file' falls back to nil when custom path missing."
  (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/tmp/magit-psr-test-dir"))
            ((symbol-function 'file-exists-p) (lambda (_) nil)))
    (let ((magit-psr-project-config-file "backend/phpcs.xml"))
      (should (equal (magit-psr--find-config-file) nil)))))

;;; temp-ruleset

(ert-deftest magit-psr-create-temp-ruleset-creates-file ()
  "Test `magit-psr--create-temp-ruleset' creates a temp XML file."
  (let ((magit-psr-custom-rules nil)
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (progn
            (should (file-exists-p temp-file))
            (should (string-suffix-p ".xml" temp-file)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-no-custom ()
  "Test temp ruleset with no custom rules (just base standard)."
  (let ((magit-psr-custom-rules nil)
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "ref=\"PSR12\"" nil t))
            (should (search-forward "</ruleset>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-with-custom ()
  "Test temp ruleset with custom rule override."
  (let ((magit-psr-custom-rules
         '((:rule "Generic.Files.LineLength"
            :properties (("lineLimit" . "140")
                         ("absoluteLineLimit" . "140")))))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "ref=\"PSR12\"" nil t))
            (should (search-forward "ref=\"Generic.Files.LineLength\"" nil t))
            (should (search-forward "name=\"lineLimit\" value=\"140\"" nil t))
            (should (search-forward "name=\"absoluteLineLimit\" value=\"140\"" nil t))
            (should (search-forward "</ruleset>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-with-config-file ()
  "Test temp ruleset references config file path as base."
  (let ((magit-psr-custom-rules
         '((:rule "Generic.Files.LineLength"
            :properties (("lineLimit" . "140")))))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset "/repo/backend/phpcs.xml")))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "ref=\"/repo/backend/phpcs.xml\"" nil t))
            (should (search-forward "ref=\"Generic.Files.LineLength\"" nil t))
            (should (search-forward "</ruleset>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-custom-no-properties ()
  "Test temp ruleset with a custom rule that has no properties."
  (let ((magit-psr-custom-rules '((:rule "PSR1.Classes.ClassDeclaration")))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "ref=\"PSR1.Classes.ClassDeclaration\"" nil t))
            ;; Should NOT have a <properties> block
            (should-not (search-forward "<properties>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-exclude-pattern ()
  "Test temp ruleset with exclude-pattern."
  (let ((magit-psr-custom-rules
         '((:rule "Generic.Files.LineLength"
            :exclude-pattern "/routes")))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "ref=\"Generic.Files.LineLength\"" nil t))
            (should (search-forward "<exclude-pattern>/routes</exclude-pattern>" nil t))
            (should (search-forward "</ruleset>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-exclude-pattern-list ()
  "Test temp ruleset with multiple exclude-patterns."
  (let ((magit-psr-custom-rules
         (list (list :rule "Generic.Files.LineLength"
                     :exclude-pattern (list "/tests" "/migrations"))))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "<exclude-pattern>/tests</exclude-pattern>" nil t))
            (should (search-forward "<exclude-pattern>/migrations</exclude-pattern>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-include-pattern ()
  "Test temp ruleset with include-pattern."
  (let ((magit-psr-custom-rules
         '((:rule "Generic.Files.LineLength"
            :include-pattern "*.php")))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "<include-pattern>*.php</include-pattern>" nil t)))
        (delete-file temp-file)))))

(ert-deftest magit-psr-create-temp-ruleset-exclude-and-properties ()
  "Test temp ruleset with exclude-pattern and properties combined."
  (let ((magit-psr-custom-rules
         '((:rule "Generic.Files.LineLength"
            :properties (("lineLimit" . "140"))
            :exclude-pattern "/routes")))
        (magit-psr-standard "PSR12"))
    (let ((temp-file (magit-psr--create-temp-ruleset nil)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents temp-file)
            (goto-char (point-min))
            (should (search-forward "ref=\"Generic.Files.LineLength\"" nil t))
            (should (search-forward "name=\"lineLimit\" value=\"140\"" nil t))
            (should (search-forward "<exclude-pattern>/routes</exclude-pattern>" nil t))
            (should (search-forward "</rule>" nil t)))
        (delete-file temp-file)))))

;;; Integration tests (use real git on this repo)

(ert-deftest magit-psr-integration-git-ls-files ()
  "Integration test: real git ls-files should find test/test_psr.php."
  (skip-unless (executable-find "git"))
  (let ((files (with-temp-buffer
                 (call-process "git" nil t nil
                               "ls-files" "--cached" "--others" "--exclude-standard")
                 (split-string (buffer-string) "\n" t))))
    (should (member "test/test_psr.php" files))
    (should (member "magit-psr.el" files))
    (should (member "README.md" files))))

(ert-deftest magit-psr-integration-git-ls-files-via-magit-psr ()
  "Integration test: `magit-psr--git-ls-files' finds test/test_psr.php
using real git (no mock override needed since it uses `call-process')."
  (skip-unless (executable-find "git"))
  (let ((files (magit-psr--git-ls-files)))
    (should (member "test/test_psr.php" files))))

(ert-deftest magit-psr-integration-find-php-files ()
  "Integration test: `magit-psr--find-php-files' detects test/test_psr.php
using real git (no mock override needed)."
  (skip-unless (executable-find "git"))
  (let ((files (magit-psr--find-php-files default-directory)))
    (should (member "test/test_psr.php" files))
    ;; Non-PHP files should be excluded
    (should-not (member "magit-psr.el" files))
    (should-not (member "README.md" files))
    (should-not (member "Makefile" files))))

(provide 'magit-psr-test)
;;; magit-psr-test.el ends here
