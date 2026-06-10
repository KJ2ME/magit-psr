;;; magit-psr.el --- Show PHP PSR errors in Magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: kj <webmaste@outcontrol.net>
;; URL: https://github.com/KJ2ME/magit-psr
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (magit "2.13.0"))
;; Keywords: magit, php, psr

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

;; This package displays PHP PSR errors from phpcs in the Magit
;; status buffer.  Activating an item jumps to the error in its file.
;;
;; Usage:
;; Run `magit-psr-mode', then open a Magit status buffer.

;;; Code:

(require 'json)
(require 'seq)
(require 'magit)

;;;; Customization

(defgroup magit-psr nil
  "Show PHP PSR errors in Magit status buffer."
  :group 'magit)

(defcustom magit-psr-executable "phpcs"
  "Path to the phpcs executable."
  :type 'string)

(defcustom magit-psr-standard "PSR12"
  "PHPCS coding standard to check against."
  :type 'string)

(defcustom magit-psr-max-items 20
  "Automatically collapse the section if there are more than this many items."
  :type 'integer)

(defcustom magit-psr-insert-after '(bottom)
  "Where to insert the PSR section in the Magit status buffer."
  :type '(repeat (choice (const :tag "Top" top)
                         (const :tag "Bottom" bottom)
                         (const :tag "Recent commits" unpushed)
                         (const :tag "Untracked files" untracked)
                         (const :tag "Unstaged files" unstaged)
                         (const :tag "Staged files" staged)
                         (const :tag "Stashes" stashes)
                         (symbol :tag "Specified section"))))

(defcustom magit-psr-show-warnings nil
  "If non-nil, show warnings in addition to errors."
  :type 'boolean)

(defcustom magit-psr-exclude-globs '("vendor/" "node_modules/")
  "Glob patterns to exclude from phpcs scans."
  :type '(repeat string))

(defcustom magit-psr-depth nil
  "Maximum depth of files to scan.  nil means unlimited."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "N levels")))

(defcustom magit-psr-phpcs-args nil
  "Additional arguments to pass to phpcs."
  :type '(repeat string))

(defcustom magit-psr-recent-commits nil
  "Number of recent commits to include when checking for changed files.
When nil, scan all tracked files (default, legacy behavior).
When 0, only scan files with uncommitted changes (staged, unstaged, untracked).
When N>0, also includes files changed in the last N commits."
  :type '(choice (const :tag "All files" nil)
                 (integer :tag "N recent commits")))

(defcustom magit-psr-show-placeholder t
  "Whether to show a loading placeholder while the async scan runs.
When non-nil, the PSR section shows a hourglass indicator during scanning.
When nil, the section only appears once results are available."
  :type 'boolean)

;;;; Structs

(cl-defstruct magit-psr-item
  filename line column message source severity type)

;;;; Variables

(defvar-local magit-psr-item-cache nil
  "Items found by most recent scan.")

(defvar-local magit-psr-last-update-time nil
  "When the items were last updated.")

(defvar-local magit-psr-updating nil
  "Whether items are being updated now.
t means explicit update requested, 'async means async scan in progress.")

(defvar magit-psr--async-process nil
  "Current async phpcs process, if any.")

(defvar magit-psr-section-heading "PSR"
  "Allows overriding of section heading.")

(defvar magit-psr-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magit-psr-jump-to-item)
    (define-key map (kbd "RET") #'magit-psr-jump-to-item)
    map)
  "Keymap for `magit-psr' item sections.")

;;;; Mode

(defun magit-psr--invalidate-cache ()
  "Invalidate PSR cache before Magit refreshes, forcing a new scan."
  (when (derived-mode-p 'magit-status-mode)
    (setq magit-psr-last-update-time nil)))

;;;###autoload
(define-minor-mode magit-psr-mode
  "Show PHP PSR errors in Magit status buffer for PHP files in repo."
  :require 'magit-psr
  :group 'magit-psr
  :global t
  (if magit-psr-mode
      (progn
        (magit-add-section-hook 'magit-status-sections-hook
                                #'magit-psr--insert
                                nil
                                'append)
        (add-hook 'magit-pre-refresh-hook #'magit-psr--invalidate-cache))
    (remove-hook 'magit-status-sections-hook #'magit-psr--insert)
    (remove-hook 'magit-pre-refresh-hook #'magit-psr--invalidate-cache)))

;;;; Commands

(defun magit-psr-update ()
  "Update the PSR error list manually."
  (interactive)
  (unless magit-psr-mode
    (user-error "Please activate `magit-psr-mode'"))
  (when (and magit-psr--async-process
             (process-live-p magit-psr--async-process))
    (delete-process magit-psr--async-process))
  (let ((inhibit-read-only t))
    (magit-psr--delete-section [* psr])
    (setq magit-psr-updating t)
    (magit-psr--insert)))

(defun magit-psr-jump-to-item (&optional item)
  "Jump to the PSR error ITEM at point."
  (interactive)
  (let* ((item (or item (get-text-property (point) 'magit-psr-item)))
         (filename (magit-psr-item-filename item))
         (line (magit-psr-item-line item))
         (column (magit-psr-item-column item)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (when column
      (forward-char (1- column)))))

;;;; Sections

(defun magit-psr--delete-section (condition)
  "Delete the section specified by CONDITION."
  (save-excursion
    (goto-char (point-min))
    (when-let ((section (cl-loop until (magit-section-match condition)
                                 do (forward-line 1)
                                 when (eobp)
                                 return nil
                                 finally return (magit-current-section))))
      (object-remove-from-list magit-root-section 'children section)
      (with-slots (start end) section
        (delete-region start end)))))

(defun magit-psr--section-end (condition)
  "Return end position of section matching CONDITION."
  (save-excursion
    (goto-char (point-min))
    (pcase condition
      ('top (when-let ((section (or (magit-get-section (app (lambda (s) (oref s type)) (or 'tag 'tags 'branch)))
                                    (magit-get-section (cadr (oref magit-root-section children))))))
              (1+ (oref section end))))
      ('bottom (oref (car (last (oref magit-root-section children))) end))
      (_ (when-let ((section (magit-get-section condition)))
           (oref section end))))))

(defun magit-psr--insert ()
  "Insert PSR error items into current Magit status buffer.
Starts an async phpcs scan on first run or explicit update.
Shows cached items if available, or a loading indicator while scanning."
  (when (or magit-psr-updating
            (not (file-remote-p default-directory)))
    (if (and magit-psr-last-update-time
             (not magit-psr-updating))
        (magit-psr--insert-items (current-buffer) magit-psr-item-cache)
      (if (eq magit-psr-updating 'async)
          (when magit-psr-show-placeholder
            (magit-psr--insert-placeholder (current-buffer)))
        (setq magit-psr-updating 'async)
        (when magit-psr-show-placeholder
          (magit-psr--insert-placeholder (current-buffer)))
        (let* ((default-directory (magit-toplevel))
               (files (magit-psr--find-php-files default-directory)))
          (if (not files)
              (progn
                (message "magit-psr: No PHP files found")
                (magit-psr--delete-section [* psr])
                (magit-psr--insert-items (current-buffer) nil)
                (setq magit-psr-updating nil))
            (magit-psr--start-async-scan (current-buffer) files)))))))

(defun magit-psr--insert-placeholder (buffer)
  "Insert loading placeholder section in BUFFER."
  (with-current-buffer buffer
    (let* ((magit-insert-section--parent magit-root-section)
           (inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (goto-char (or (cl-loop for section in magit-psr-insert-after
                                for pos = (magit-psr--section-end section)
                                when pos return pos)
                       (magit-psr--section-end 'bottom)))
        (let ((section (magit-insert-section (psr nil t)
                         (magit-insert-heading
                          (format "%s (⏳)"
                                  (propertize magit-psr-section-heading
                                              'face 'magit-section-heading
                                              'font-lock-face 'magit-section-heading)))
                         (insert "\n"))))
          (push section (oref magit-root-section children)))))))

(defun magit-psr--start-async-scan (buffer files)
  "Start async phpcs scan in BUFFER for FILES."
  (when (and magit-psr--async-process
             (process-live-p magit-psr--async-process))
    (delete-process magit-psr--async-process))
  (let* ((args (append (list "--report=json"
                              (format "--standard=%s" magit-psr-standard)
                              "-s")
                        magit-psr-phpcs-args
                        files))
         (process (make-process
                   :name "magit-psr"
                   :buffer (generate-new-buffer " *magit-psr-output*")
                   :command (cons magit-psr-executable args)
                   :noquery t
                   :sentinel #'magit-psr--sentinel)))
    (setq magit-psr--async-process process)
    (process-put process 'magit-psr-buffer buffer)))

(defun magit-psr--sentinel (process event)
  "Sentinel for magit-psr async process.
Parses phpcs output and updates the status buffer."
  (when (or (string= event "finished\n")
            (string-prefix-p "exited " event))
    (let* ((output-buffer (process-buffer process))
           (target-buffer (process-get process 'magit-psr-buffer))
           (output (with-current-buffer output-buffer
                     (buffer-string)))
           (items (condition-case nil
                      (magit-psr--parse-phpcs-output output)
                    (error
                     (message "magit-psr: Failed to parse phpcs output")
                     nil))))
      (when (buffer-live-p target-buffer)
        (with-current-buffer target-buffer
          (let ((inhibit-read-only t))
            (magit-psr--delete-section [* psr])
            (magit-psr--insert-items target-buffer items))))))
  (when (and (process-buffer process)
             (buffer-live-p (process-buffer process)))
    (kill-buffer (process-buffer process))))

(cl-defun magit-psr--insert-items (buffer items)
  "Insert PSR ITEMS into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((num-items (length items))
             (magit-section-show-child-count t)
             (magit-insert-section--parent magit-root-section)
             (inhibit-read-only t))
        (when magit-psr-updating
          (setq magit-psr-item-cache items
                magit-psr-last-update-time (current-time)
                magit-psr-updating nil))
        (save-excursion
          (goto-char (point-min))
          (goto-char (or (cl-loop for section in magit-psr-insert-after
                                  for pos = (magit-psr--section-end section)
                                  when pos return pos)
                         (magit-psr--section-end 'bottom)))
          (if (not items)
              (let ((magit-insert-section--parent magit-root-section))
                (magit-insert-section (psr)
                  (magit-insert-heading
                   (concat (propertize magit-psr-section-heading
                                       'face 'magit-section-heading
                                       'font-lock-face 'magit-section-heading)
                           " (0)\n"))))
            (let ((section (magit-insert-section (psr)
                             (magit-insert-heading
                              (format "%s (%s)"
                                      (propertize magit-psr-section-heading
                                                  'face 'magit-section-heading
                                                  'font-lock-face 'magit-section-heading)
                                      num-items))
                             (magit-psr--insert-groups items)
                             (insert "\n"))))
              (push section (oref magit-root-section children))
              (if (> num-items magit-psr-max-items)
                  (magit-section-hide section)
                (magit-section-show section)))))))))

(defun magit-psr--insert-groups (items)
  "Insert grouped PSR ITEMS by filename."
  (let* ((groups (seq-group-by #'magit-psr-item-filename items))
         (sorted-groups (sort groups (lambda (a b) (string< (car a) (car b))))))
    (dolist (group sorted-groups)
      (let ((filename (car group))
            (file-items (cdr group)))
        (magit-insert-section ((eval (intern filename)))
          (magit-insert-heading
           (concat (propertize filename 'face 'magit-filename 'font-lock-face 'magit-filename)
                   (format " (%s)" (length file-items))))
          (dolist (item file-items)
            (let* ((type (magit-psr-item-type item))
                   (type-face (if (string= type "ERROR") 'error 'warning))
                   (message (magit-psr-item-message item))
                   (line (magit-psr-item-line item))
                   (column (magit-psr-item-column item))
                   (line-str (format ":%s:%s" line column)))
              (magit-insert-section ((psr-item item) :keymap magit-psr-section-map)
                (let ((start (point)))
                  (insert
                   (propertize line-str 'face 'magit-line-number 'font-lock-face 'magit-line-number)
                   " "
                   (propertize (format "[%s]" type) 'face type-face 'font-lock-face type-face)
                   " "
                   message
                   "\n")
                  (put-text-property start (point) 'magit-psr-item item)
                   (put-text-property start (point) 'keymap magit-psr-section-map))))))))))

;;;; Scanning

(defun magit-psr--git-changed-files (&optional directory)
  "Return list of files with changes in DIRECTORY.
Includes staged, unstaged, and untracked files.
When `magit-psr-recent-commits' is a positive integer, also
includes files changed in that many recent commits."
  (let* ((default-directory (or directory default-directory))
         (git (if (boundp 'magit-git-executable) magit-git-executable "git"))
         (files (list)))
    ;; Uncommitted changes: staged + unstaged
    (with-temp-buffer
      (when (= (call-process git nil t nil "diff" "--name-only" "HEAD") 0)
        (dolist (f (split-string (buffer-string) "\n" t))
          (push f files))))
    ;; Untracked files
    (with-temp-buffer
      (when (= (call-process git nil t nil "ls-files" "--others" "--exclude-standard") 0)
        (dolist (f (split-string (buffer-string) "\n" t))
          (push f files))))
    ;; Recent commits
    (when (and magit-psr-recent-commits
               (>= magit-psr-recent-commits 1))
      (with-temp-buffer
        (when (= (call-process git nil t nil "log" "--name-only" "--pretty=format:"
                                (format "-%d" magit-psr-recent-commits)
                                "--diff-filter=AMR") 0)
          (dolist (f (split-string (buffer-string) "\n" t))
            (push f files)))))
    (delete-dups files)))

(defun magit-psr--find-php-files (directory)
  "Find PHP files in DIRECTORY using git.
Respects `magit-psr-exclude-globs' and `magit-psr-depth'.
When `magit-psr-recent-commits' is non-nil, only returns changed files."
  (let* ((default-directory directory)
         (all-files (if magit-psr-recent-commits
                        (magit-psr--git-changed-files directory)
                      (magit-psr--git-ls-files)))
         (php-files (seq-filter (lambda (f) (string-suffix-p ".php" f t)) all-files))
         (glob-regexps (mapcar (lambda (glob)
                                (wildcard-to-regexp
                                 (if (and (string-suffix-p "/" glob)
                                          (not (string-match-p "[*?]" glob)))
                                     (concat glob "*")
                                   glob)))
                              magit-psr-exclude-globs))
         (filtered (cl-remove-if (lambda (file)
                                    (cl-some (lambda (r) (string-match-p r file)) glob-regexps))
                                  php-files)))
    (if magit-psr-depth
        (cl-remove-if-not (lambda (f) (<= (cl-count ?/ f) magit-psr-depth)) filtered)
      filtered)))

(defun magit-psr--git-ls-files ()
  "Return list of tracked and untracked (non-ignored) files in the git repo."
  (let ((git (if (boundp 'magit-git-executable) magit-git-executable "git")))
    (with-temp-buffer
      (when (= (call-process git nil t nil
                             "ls-files" "--cached" "--others" "--exclude-standard")
               0)
        (split-string (buffer-string) "\n" t)))))

(defun magit-psr--parse-phpcs-output (output)
  "Parse phpcs JSON OUTPUT into list of `magit-psr-item' structs."
  (let* ((default-directory (magit-toplevel))
         (json-data (condition-case nil
                        (magit-psr--json-read output)
                      (error nil)))
         (items nil))
    (when json-data
      (let* ((files-table (magit-psr--json-get json-data "files")))
        (when files-table
          (magit-psr--json-each
           files-table
           (lambda (filename file-data)
             (let ((messages (magit-psr--json-get file-data "messages")))
               (dolist (msg messages)
                 (let ((type (magit-psr--json-get msg "type")))
                   (when (or magit-psr-show-warnings (string= type "ERROR"))
                     (let* ((relative (file-relative-name filename default-directory))
                            (item (make-magit-psr-item
                                   :filename relative
                                   :line (magit-psr--json-get msg "line")
                                   :column (magit-psr--json-get msg "column")
                                   :message (magit-psr--json-get msg "message")
                                   :source (magit-psr--json-get msg "source")
                                   :type type)))
                       (push item items))))))))))
    (nreverse items))))

(defun magit-psr--scan ()
  "Run phpcs synchronously and return list of `magit-psr-item' structs."
  (let* ((default-directory (magit-toplevel))
         (files (magit-psr--find-php-files default-directory)))
    (if (not files)
        (progn
          (message "magit-psr: No PHP files found")
          nil)
      (let* ((args (append (list magit-psr-executable
                                 "--report=json"
                                 (format "--standard=%s" magit-psr-standard)
                                 "-s")
                           magit-psr-phpcs-args
                           files))
             (command (mapconcat #'shell-quote-argument args " "))
             (output (shell-command-to-string command)))
         (magit-psr--parse-phpcs-output output)))))

;;;;; JSON parsing helpers

(defun magit-psr--json-read (string)
  "Read JSON from STRING.
Uses `json-parse-string' if available, otherwise falls back to
`json-read-from-string'."
  (if (fboundp 'json-parse-string)
      (json-parse-string string :object-type 'hash-table :array-type 'list)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (json-read))))

(defun magit-psr--json-get (object key)
  "Get KEY from OBJECT.
OBJECT may be a hash-table or an alist.
KEY is a string.  When OBJECT is an alist (from json.el), the key
may be interned as a symbol, so we try both."
  (pcase object
    ((pred hash-table-p) (gethash key object))
    ((pred consp) (or (alist-get key object nil nil #'equal)
                      (alist-get (intern key) object nil nil #'equal)))))

(defun magit-psr--json-each (object fn)
  "Call FN on each entry of OBJECT.
OBJECT may be a hash-table or an alist.
FN is called with (key value)."
  (pcase object
    ((pred hash-table-p)
     (maphash fn object))
    ((pred consp)
     (dolist (pair object)
       (funcall fn (car pair) (cdr pair))))))

(provide 'magit-psr)

;;; magit-psr.el ends here
