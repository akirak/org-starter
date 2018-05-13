;;; org-starter.el --- A basic configuration framework for org mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
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

;; This package helps you configure org mode. See README for details.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

;;;; Compatibility

(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1))))

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
  (or (get-buffer org-starter-error-buffer)
      (with-current-buffer (generate-new-buffer org-starter-error-buffer)
        (local-set-key "q" 'quit-window)
        (local-set-key "g" 'org-starter-verify-configuration)
        (setq buffer-read-only t)
        (current-buffer))))

(defun org-starter--log-error-no-newline (format-string &rest args)
  "Variant of `org-starter--log-error' that does not append a newline."
  (declare (indent 0))
  (with-current-buffer (org-starter--create-error-buffer)
    (let ((inhibit-read-only t))
      (insert (apply 'format format-string args))))
  (setq org-starter-found-errors (1+ (or org-starter-found-errors 0))))

(defun org-starter--log-error (format-string &rest args)
  "An alternative to `message' which logs a string to `org-starter-error-buffer'."
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
sans-directory file name as FILENAME. If such a file is not found, this function
returns nil.

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
      (add-hook 'org-agenda-files dpath 'append))
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

;;;;; Keymap for visiting a known file
(defvar org-starter-file-map (make-sparse-keymap)
  "Keymap used to find a file.")

(defcustom org-starter-define-file-commands 'with-keys
  "Define commands to find a specific file.

When non-nil, org-starter define commands to find (or jump to) a specific file
defined by `org-starter-define-file'. The defined commands are also used to
bind keys to files.

This is convenient for the follwing
reasons:

- You can access a file quickly using \"M-x\".
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
  `(intern (format org-starter-file-command-template (file-name-base ,fpath))))

(defun org-starter--define-file-command (fpath)
  "Define a command to find FPATH."
  (eval
   `(defun ,(org-starter--file-command-name fpath) ()
      (interactive)
      (find-file ,fpath))))

(defun org-starter--bind-file-key (key fpath)
  "Bind KEY to a command to visit FPATH."
  (let ((command-name (org-starter--file-command-name fpath)))
    (define-key 'org-starter-file-map key
      (if (and org-starter-define-file-commands
               (fboundp command-name))
          command-name
        `(lambda () (interactive) (find-file ,fpath))))))

;;;;; Defining a file

(cl-defun org-starter-define-file (filename &key
                                            directory
                                            (required t)
                                            deprecated
                                            agenda
                                            refile
                                            set-default
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
                (add-hook 'org-agenda-files fpath 'append))
              (mapc (lambda (symbol) (set-default symbol fpath))
                    (org-starter--to-symbol-list set-default))
              (when (and refile (not deprecated))
                (let ((pair (assoc fpath org-refile-targets)))
                  (if pair
                      (setf (cdr pair) refile)
                    (add-to-list 'org-refile-targets (cons fpath refile) 'append))))
              (when key
                (org-starter--bind-file-key key fpath))
              (when local-variables
                (push (cons fpath local-variables) org-starter-file-local-variables))
              (add-to-list 'org-starter-known-files fpath)))
     ((and (not deprecated) required)
      (error "Required org file %s is not found" filename))
     ((not deprecated)
      (org-starter--log-error "%s is missing" filename)))))

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

;;;; Miscellaneous functionality

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

(defun org-starter-load-all-files-in-path ()
  "Load all org files in `org-starter-path' into Emacs.

This can be convenient in some situations where you want ensure that all org
files are in buffers.

`org-starter-get-all-files-in-path' is used to get a list of org files."
  (interactive)
  (mapc #'org-starter--load-file (org-starter-get-all-files-in-path)))

(provide 'org-starter)
;;; org-starter.el ends here
