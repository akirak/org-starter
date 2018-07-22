;;; org-starter.el --- A basic configuration framework for org mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.10"))
;; URL: https://github.com/akirak/org-starter

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package helps you configure org mode.  See README for details.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'org-capture)

;;;; Compatibility

(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1))))

;;;; Custom variables
(defcustom org-starter-capture-template-map-function nil
  "A function used to transform each entry in `org-capture' templates."
  :type 'function
  :group 'org-starter)

(defcustom org-starter-alternative-find-function
  (cond
   ((fboundp 'helm-org-rifle-files) #'helm-org-rifle-files)
   (t #'dired-jump))
  "An alternative function to find an Org file.

This function is called by `org-starter-find-file-by-key' when
a sequence of two universal arguments are given."
  :group 'org-starter
  :type 'function)

(defcustom org-starter-extra-refile-map
  '(("/" . org-refile))
  "Extra bindings available in `org-starter-refile-by-key'."
  :group 'org-starter
  :type '(alist :key-type key-sequence
                :value-type function))

;;;; The error buffer and error logging
;; This is used by `org-starter-verify-configuration'.

(defconst org-starter-error-buffer "*org-starter errors*")

(defvar org-starter-found-errors nil
  "Non-nil if an error is found while configuring org-starter.")

(defun org-starter--clear-errors ()
  "Reset the status of the error buffer."
  (when-let* ((buf (get-buffer org-starter-error-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (setq org-starter-found-errors nil))

(defun org-starter--create-error-buffer ()
  "Return the error buffer."
  (or (get-buffer org-starter-error-buffer)
      (with-current-buffer (generate-new-buffer org-starter-error-buffer)
        (local-set-key "q" 'quit-window)
        (local-set-key "g" 'org-starter-verify-configuration)
        (setq buffer-read-only t)
        (current-buffer))))

(defun org-starter--log-error-no-newline (format-string &rest args)
  "Variant of `org-starter--log-error' that does not append a newline.

FORMAT-STRING is the format spec, and ARGS are parameters."
  (declare (indent 0))
  (with-current-buffer (org-starter--create-error-buffer)
    (let ((inhibit-read-only t))
      (insert (apply 'format format-string args))))
  (setq org-starter-found-errors (1+ (or org-starter-found-errors 0))))

(defun org-starter--log-error (format-string &rest args)
  "Like `message', but logs a string to `org-starter-error-buffer'.

FORMAT-STRING is the format spec, and ARGS are parameters."
  (declare (indent 0))
  (org-starter--log-error-no-newline (concat format-string "\n") args))

;;;; File registry: known files and directories, path, deprecated files, etc.
(defvar org-starter-known-files nil
  "List of files registered by `org-starter-define-file'.")

(defcustom org-starter-path nil
  "List of directories to search org files for."
  :group 'org-starter
  :type '(repeat string))

(defvar org-starter-deprecated-files nil
  "List of deprecated org files currently existing.")

(defvar org-starter-known-directories nil)

(defun org-starter--lookup-known-file (filename)
  "Look up a FILENAME (without a directory) in `org-starter-known-files'."
  (car (cl-remove-if-not (lambda (fpath)
                           (equal filename
                                  (file-name-nondirectory fpath)))
                         org-starter-known-files)))

(defun org-starter--search-file (filename &optional check-known-files)
  "Search FILENAME from `org-starter-path'.

If CHECK-KNOWN-FILES is non-nil, `org-starter-known-files' is first checked for
the FILENAME."
  (or (and check-known-files
           (org-starter--lookup-known-file filename))
      (cl-loop for dir in org-starter-path
               for fpath = (expand-file-name filename dir)
               when (file-exists-p fpath)
               return fpath)))

(defun org-starter-locate-file (filename &optional directory check-known-files)
  "Search FILENAME from DIRECTORY and `org-starter-path' and return its path.

This function returns an existing path to a file which has the same
sans-directory file name as FILENAME.  If such a file is not found, this
function returns nil.

If CHECK-KNOWN-FILES is non-nil, `org-starter-known-files' is first checked for
the FILENAME."
  (cond
   ((file-name-absolute-p filename)
    (when (file-exists-p filename)
      filename))
   ((null directory)
    (org-starter--search-file filename check-known-files))
   (t
    (let* ((dir (cond
                 ((stringp directory) directory)
                 ((symbolp directory) (symbol-value directory))
                 (t (error "Unsupported type of directory: %s"
                           (prin1-to-string directory)))))
           (default-file (expand-file-name filename dir)))
      (if (file-exists-p default-file)
          default-file
        (let ((found-file (org-starter--search-file filename
                                                    check-known-files)))
          (when (and found-file
                     (yes-or-no-p (format "%s does not exist, but %s is found. Add it instead?"
                                          default-file found-file)))
            found-file)))))))

(defun org-starter--to-symbol-list (obj)
  "Make a list of symbols from a symbol or a list.

OBJ should be a symbol, or a list of symbols."
  (cond
   ((null obj) nil)
   ((symbolp obj) (list obj))
   ((listp obj) obj)))

;;;; Exclude files in org-starter from recentf

(defcustom org-starter-exclude-from-recentf nil
  "If non-nil, exclude items `org-starter-known-files' from recentf."
  :group 'org-starter
  :type '(set (const :tag "Exclude known files" 'known-files)
              (const :tag "Exclude files in path" 'path)))

(defun org-starter-recentf-excluded-p (file)
  "Check if FILE is an org-starter file that should be excluded from recentf."
  (and (listp org-starter-exclude-from-recentf)
       org-starter-exclude-from-recentf
       (or (and (memq 'known-files org-starter-exclude-from-recentf)
                (cl-member file org-starter-known-files
                           :test #'file-equal-p))
           (and (memq 'path org-starter-exclude-from-recentf)
                (string-match-p org-agenda-file-regexp file)
                (cl-member (file-name-directory file) org-starter-path
                           :test #'file-equal-p)))))

(add-hook 'recentf-exclude 'org-starter-recentf-excluded-p t)

;;;; Defining directories

(defvar org-starter-directory-origins nil
  "Alist of pairs of a directory and its origin.

Each item in this alist is a cons of a path to a local directory and its origin
repository URL.")

(defun org-starter--clone (origin dest)
  "Clone a repository at ORIGIN to DEST."
  ;; TODO: check origin URL
  (process-lines "git" "clone" origin dest))

(defun org-starter-clone-all ()
  "Clone all missing repositories specified in `org-starter-directory-origins'."
  (interactive)
  (cl-loop for (dest . origin) in org-starter-directory-origins
           unless (file-directory-p dest)
           do (org-starter--clone origin dest)))

(defmacro org-starter--define-glob-function (dpath id)
  "Define a function to glob entries in a directory.

This function defines a function to glob a list of org files in a directory.
DPATH is a path to the directory, and ID is a symbol/string to uniquely
identify the directory."
  `(defun ,(intern (format "org-starter--glob-%s-for-refile"
                           (cond
                            ((symbolp id) (symbol-name id))
                            ((stringp id) id)))) ()
     (when (file-directory-p ,dpath)
       (directory-files ,dpath t org-agenda-file-regexp))))

(cl-defun org-starter-define-directory (dpath &key
                                              agenda
                                              refile
                                              id
                                              origin
                                              ensure
                                              add-to-path
                                              files)
  "Define a directory that contains org files.

DPATH is a path to the directory. This option cannot be nil.

If AGENDA is non-nil, the directory is added to `org-agenda-files'.

If REFILE is set, files in the directory are added to `org-refile-targets'.
In this case, you have to also specify ID to uniquely identify the directory.
This identifier is used to determine the name of a function to glob files in
the directory, which is called by `org-refile'.

ORIGIN is a repository URL from which you want to clone the repository. If this
option is set and ENSURE option is non-nil, this function automatically clones
the repository when the directory does not exist.

If ADD-TO-PATH is non-nil, the directory is added to `org-starter-path'.

FILES is a list whose item accepts the same options as `org-starter-define-file',
except for `:directory' option. You can define files in the directory."
  (declare (indent 1))
  (let ((exists (file-directory-p dpath)))
    (when (and ensure (not exists))
      (unless origin
        (error "%s is a required directory, but `:ensure' property is unset" dpath))
      (org-starter--clone origin dpath))
    (when agenda
      (add-to-list 'org-agenda-files dpath 'append #'file-equal-p))
    (when origin
      (add-to-list 'org-starter-directory-origins (cons dpath origin)))
    (when (and add-to-path exists)
      (add-to-list 'org-starter-path dpath))
    (when refile
      (unless id
        (error "To add %s to refile targets, you must set `:id' property" dpath))
      (let* ((func (org-starter--define-glob-function dpath id))
             (pair (assq func org-refile-targets)))
        (if pair
            (setf (cdr pair) refile)
          (add-to-list 'org-refile-targets (cons func refile) 'append))))
    (cl-loop for (filename . options) in files
             do (apply #'org-starter-define-file filename :directory dpath
                       options))
    (add-to-list 'org-starter-known-directories dpath)))

;;;; Defining files
;;;;; File-local variables
(defvar org-starter-file-local-variables nil
  "Alist of file-local variable definitions.")

(defun org-starter-load-local-variables ()
  "Load local variables defined for the current buffer file by org-starter."
  (when-let* ((fpath (buffer-file-name))
              (vars (cl-assoc fpath org-starter-file-local-variables
                              :test #'file-equal-p)))
    (cl-loop for (symbol . value) in vars
             do (cond
                 ((symbolp symbol) (set (make-local-variable symbol) value))
                 (t (error "Not a symbol: %s in %s"
                           (prin1-to-string symbol)
                           (prin1-to-string value)))))))
(add-hook 'org-mode-hook #'org-starter-load-local-variables t)

;;;;; Keymap for visiting a known file (deprecated)
(defvar org-starter-file-map (make-sparse-keymap)
  "Keymap used to find a file.")

(defcustom org-starter-define-file-commands 'with-keys
  "Define commands to find a specific file.

When non-nil, org-starter define commands to find (or jump to) a specific file
defined by `org-starter-define-file'.  The defined commands are also used to
bind keys to files.

This is convenient for the follwing
reasons:

- You can access a file quickly using \\[execute-command\\].
- You can use the command names for which-key replacements."
  :group 'org-starter
  :type '(choice (const :tag "All defined files" all)
                 (const :tag "Files with keys" with-keys)
                 (const :tag "Never" nil)))

(defcustom org-starter-file-command-template "org-starter-find-file:%s"
  "Template used to determine command names for files.

'%s' in the template is replaced with the base name of the file.

This is applicable when `org-starter-define-file-commands' is non-nil."
  :type 'string
  :group 'org-starter)

(defmacro org-starter--file-command-name (fpath)
  "Build a command name for FPATH."
  `(intern (format org-starter-file-command-template (file-name-base ,fpath))))

(defun org-starter--define-file-command (fpath)
  "Define a command to find FPATH."
  (eval
   `(defun ,(org-starter--file-command-name fpath) ()
      (interactive)
      (find-file ,fpath))))

(defvar org-starter-key-file-alist nil)

(defun org-starter--funcall-on-file-by-key (func &optional
                                                 prompt
                                                 parent-map)
  "Pick an Org file by key and apply a function on it.

This function picks an Org file by a key specified as :key argument
of `org-starter-define-file', and apply FUNC on it.

If PROMPT is given, use it as the prompt.

If PARENT-MAP is given, use it as the parent map."
  (let ((map (make-sparse-keymap))
        (message-log-max nil)
        (msg (mapconcat (lambda (cell) (format "[%s]: %s"
                                               (car cell)
                                               (file-name-nondirectory (cdr cell))))
                        org-starter-key-file-alist "\n")))
    (dolist (cell org-starter-key-file-alist)
      (define-key map (car cell)
        (lambda () (interactive) (funcall func (cdr cell)))))
    (message (if prompt (concat prompt "\n" msg) msg))
    (set-transient-map (if parent-map
                           (make-composed-keymap map parent-map)
                         map))))

;;;###autoload
(defun org-starter-find-file-by-key (&optional arg)
  "Visit an Org file quickly by key.

With this command, you can quickly open a file by a key sequence
specified as :key property of the file.

To access a file which is not assigned a key, you can select it
using `completing-read' by pressing \"/\" key.

If a universal prefix (C-u) is given as ARG, open the file in other
window.

If two universal prefix arguments (C-u C-u) is given, call a function
specified as `org-starter-alternative-find-function' with the file
as the argument."
  (interactive "P")
  (pcase arg
    ('(4) (org-starter--funcall-on-file-by-key
           #'find-file-other-window "Find an Org file in other window:"
           (let ((map (make-sparse-keymap)))
             (define-key map (kbd "/") #'org-starter-select-file-other-window)
             map)))
    ('(16) (org-starter--funcall-on-file-by-key
            org-starter-alternative-find-function
            (format "Call %s:"
                    (if (symbolp org-starter-alternative-find-function)
                        (symbol-name org-starter-alternative-find-function)
                      "the function"))
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "/")
                (lambda ()
                  (interactive)
                  (funcall org-starter-alternative-find-function
                           (org-starter-select-file "Select an Org file: "))))
              map)))
    (_ (org-starter--funcall-on-file-by-key
        #'find-file "Find an Org file:"
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "/") #'org-starter-select-file)
          map)))))

(defun org-starter--refile-target-of-file (file)
  "Get a refile target spec to FILE."
  (cl-assoc file (cl-remove-if-not (lambda (cell) (stringp (car cell)))
                                   org-refile-targets)
            :test 'file-equal-p))

;;;###autoload
(defun org-starter-refile-by-key (&optional arg)
  "Run `org-refile' with the target limited to a file by key.

With this command, you can quickly refile the current entry to a file by a key
sequence specified as :key property of the file.

ARG is passed to `org-refile' function.

You can also access keybindings defined in
`org-starter-extra-refile-map'.
For example, you can run a normal `org-refile' by pressing \"/\" key
by default."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (org-starter--funcall-on-file-by-key
   (lambda (file)
     (let ((org-refile-targets (list (or (org-starter--refile-target-of-file file)
                                         `(,file :maxlevel . 9)))))
       (org-refile arg)))
   "Refile to file:"
   (let ((map (make-sparse-keymap)))
     (cl-loop for (key . command) in org-starter-extra-refile-map
              do (define-key map (if (stringp key)
                                     (kbd key)
                                   key)
                   command))
     map)))

(defun org-starter--bind-file-key (key fpath)
  "Bind KEY to a command to visit FPATH."
  (cl-pushnew (cons key fpath) org-starter-key-file-alist
              :key 'car :test 'equal)
  ;; (let ((command-name (org-starter--file-command-name fpath)))
  ;;   (define-key 'org-starter-file-map key
  ;;     (if (and org-starter-define-file-commands
  ;;              (fboundp command-name))
  ;;         command-name
  ;;       `(lambda () (interactive) (find-file ,fpath)))))
  )

;;;;; Defining a file

(cl-defun org-starter-define-file (filename &key
                                            directory
                                            (required t)
                                            deprecated
                                            agenda
                                            refile
                                            set-default
                                            capture
                                            key
                                            local-variables)
  "Define an org file.

FILENAME is the file name of the org file. This can be either a file name
without a directory or an absolute file path.

DIRECTORY is a directory that should contain the file. FILENAME and DIRECTORY
are passed to `org-starter-locate-file' function to find an actual path to the
file.

By default, this function raises an error if the file does not exist. This can
be prevented by setting REQUIRED to nil or DEPRECATED to t.

If DEPRECATED is non-nil, this file is not added to `org-refile-targets' even if
the refile option is specified. The file is added to
`org-starter-deprecated-files' which you can use for testing.

If AGENDA is non-nil, the file is added to `org-agenda-files'.

If REFILE is set, the file is added to `org-refile-targets' with the option
value. For example, if you set REFILE to \"'(:maxlevel . 5)\", then
\"'(PATH (:maxlevel . 5))\" is added to `org-refile-targets'.

If you specify variable names as a list of symbols in SET-DEFAULT, those
variables are set to the path of the defined file using `set-default'.
For example, you can use this function to set `org-default-notes-file' based
on the actual path.

CAPTURE specifies a list of `org-capture' templates into the file.
To properly override an existing template with the same key, items in
`org-capture-templates' should be sorted lexicographically by key.

KEY is a string to represent a key binding used to jump to the file.
If this property is nil, the file will not be bound on the map.

LOCAL-VARIABLES allows you to specify file-local variables which should be set
after org-mode is initialized. This can be done in the file footer, but it is
sometimes convenient to be able to define them outside of the file, especially
if you define a complex function. This option should be an alist of variable
names and values."
  (declare (indent 1))
  (let ((fpath (org-starter-locate-file filename directory)))
    (cond
     (fpath (progn
              (when (cl-ecase org-starter-define-file-commands
                      (all t)
                      (with-keys key)
                      (nil nil))
                (org-starter--define-file-command fpath))
              (when deprecated
                (org-starter--log-error "%s file is deprecated"
                                        (abbreviate-file-name fpath))
                (add-to-list 'org-starter-deprecated-files fpath))
              (when agenda
                (add-to-list 'org-agenda-files fpath 'append #'file-equal-p))
              (mapc (lambda (symbol) (set-default symbol fpath))
                    (org-starter--to-symbol-list set-default))
              (when (and refile (not deprecated))
                (let ((pair (assoc fpath org-refile-targets)))
                  (if pair
                      (setf (cdr pair) refile)
                    (add-to-list 'org-refile-targets (cons fpath refile) 'append))))
              (dolist (spec (mapcar (or org-starter-capture-template-map-function
                                        #'identity)
                                    capture))
                (org-starter-add-file-capture-template fpath spec))
              (when key
                (org-starter--bind-file-key key fpath))
              (when local-variables
                (push (cons fpath local-variables) org-starter-file-local-variables))
              (add-to-list 'org-starter-known-files fpath)))
     ((and (not deprecated) required)
      (error "Required org file %s is not found" filename))
     ((not deprecated)
      (org-starter--log-error "%s is missing" filename)))))

(defun org-starter--add-capture-template (spec)
  "Insert SPEC into `org-capture-templates'."
  (cl-destructuring-bind
      (former latter)
      (--split-with (string< (car it) (car spec))
                    org-capture-templates)
    (setq org-capture-templates
          `(,@former
            ,spec
            ,@(if (string-equal (caar latter) (car spec))
                  (cdr latter)
                latter)))))

;;;###autoload
(defun org-starter-undefine-file (filename)
  "Delete an entry with FILENAME from the list of known files."
  (interactive (list (completing-read "Known file to undefine: "
                                      org-starter-known-files nil t)))
  (let ((fpath (expand-file-name
                (cond
                 ((file-name-absolute-p filename) filename)
                 (t (org-starter--lookup-known-file filename))))))
    (cl-delete fpath org-starter-deprecated-files)
    (cl-delete fpath org-agenda-files)
    (cl-delete fpath org-refile-targets :key #'car)
    (cl-delete fpath org-starter-known-files)
    (message "Deleted %s from org-starter-known-files" fpath)))

;;;;; Configure a particular aspect of a file
(defun org-starter-add-file-capture-template (file spec)
  "Add a capture template for an already defined file.

FILE is a file name or a file path as passed to `org-starter-locate-file'
as the first argument.

SPEC is the same as an item in :capture option of `org-starter-define-file'."
  (declare (indent 1))
  (let* ((fpath (org-starter-locate-file file nil t))
         (target (pcase (nth 3 spec)
                   ('file `(file ,fpath))
                   (`(file+headline ,headline) `(file+headline ,fpath ,headline))
                   (`(file+olp . ,olp) `(file+olp ,fpath ,@olp))
                   (`(file+regexp ,regexp)
                    `(file+regexp ,fpath ,regexp))
                   (`(file+olp+datetree . ,olp)
                    `(file+olp+datetree ,fpath ,@olp))
                   (`(file+function ,function)
                    `(file+function ,fpath ,function))
                   (orig orig))))
    ;; Override the target if and only if it has one
    ;; The spec can be a list of two elements, i.e. a group
    (when target
      (setf (car (nthcdr 3 spec)) target))
    (org-starter--add-capture-template spec)))

;;;; Miscellaneous functionality

;;;###autoload
(defun org-starter-cleanup-entries (&optional all)
  "Remove missing files and directories from various variables.

This function removes files and directories that no longer exists from the
following variables:

- `org-starter-known-directories'
- `org-starter-known-files'

If ALL is non-nil, the following variables are also checked for missing entries:

- `org-agenda-files'
- `org-refile-targets'"
  (interactive "P")
  (cl-remove-if-not #'file-directory-p org-starter-known-directories)
  (cl-remove-if-not #'file-exists-p org-starter-known-files)
  (when all
    (cl-remove-if-not #'file-exists-p org-agenda-files)
    (cl-remove-if (lambda (pair)
                    (let ((path (car pair)))
                      (and (stringp path)
                           (not (file-exists-p path)))))
                  org-refile-targets)))

;;;###autoload
(defun org-starter-verify-configuration ()
  "Check the current configuration."
  (interactive)
  (org-starter--clear-errors)
  (cl-loop for fpath in org-starter-known-files
           unless (file-exists-p fpath)
           do (when (yes-or-no-p (format "%s no longer exists. Delete it from the known file list?"
                                         fpath))
                (org-starter-undefine-file fpath)))
  (when-let* ((deprecated-files (cl-remove-if-not #'file-exists-p
                                                  org-starter-deprecated-files)))
    (org-starter--log-error-no-newline "%d deprecated files still exist:\n%s"
                                       (length deprecated-files)
                                       (cl-loop for fpath in deprecated-files
                                                concat (format "- %s\n"
                                                               (abbreviate-file-name fpath)))))
  (when org-starter-found-errors
    (pop-to-buffer org-starter-error-buffer)
    (message "%d errors found" org-starter-found-errors)))

;;;; Loading files

(defun org-starter--load-file (fpath)
  "If there is no buffer visiting FPATH, load it into Emacs.

Unlike `find-file-noselect', this function does not care about changes in files
that are already loaded."
  (unless (find-buffer-visiting fpath)
    (find-file-noselect fpath)))

;;;###autoload
(defun org-starter-select-file (prompt)
  "Select a file from known files and agenda files.

This function select an Org file using `completing-read' with PROMPT.

If this function is called interactively, it visits the selected file.
If a prefix argument is given, it visits the selected file in
other window."
  (interactive
   (list "Select an Org file: "))
  (let ((file (completing-read
               prompt
               (cl-remove-duplicates
                (mapcar #'abbreviate-file-name
                        (append org-starter-known-files
                                (org-agenda-files)))
                :test #'string-equal) nil 'require-match)))
    (if (called-interactively-p nil)
        (if prefix-argument
            (find-file-other-window file)
          (find-file file))
      (expand-file-name file))))

;;;###autoload
(defun org-starter-select-file-other-window ()
  "A variant of `org-starter-select-file' targetting other window."
  (interactive)
  (find-file-other-window
   (org-starter-select-file "Select an Org file in other window: ")))

;;;###autoload
(defun org-starter-load-all-known-files ()
  "Load all files registered in `org-starter-known-files' into Emacs.

This can be convenient in some situations where you want ensure that all org
files are in buffers."
  (interactive)
  (mapc #'org-starter--load-file org-starter-known-files))

(defun org-starter-get-all-files-in-path ()
  "Get a list of org files in `org-starter-path'.

`org-agenda-file-regexp' is used to match the files."
  (cl-loop for dpath in org-starter-path
           append (directory-files dpath t org-agenda-file-regexp)))

;;;###autoload
(defun org-starter-load-all-files-in-path ()
  "Load all org files in `org-starter-path' into Emacs.

This can be convenient in some situations where you want ensure that all org
files are in buffers.

`org-starter-get-all-files-in-path' is used to get a list of org files."
  (interactive)
  (mapc #'org-starter--load-file (org-starter-get-all-files-in-path)))

(provide 'org-starter)
;;; org-starter.el ends here
