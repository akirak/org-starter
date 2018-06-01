;;; helm-org-starter.el --- Helm interface for org-starter.el -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (helm "1.9.4"))
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

;; This is a Helm interface to Org buffers.

;;; Code:

(require 'org-starter)
(require 'seq)
(require 'helm)
(require 'cl-lib)

(defclass helm-org-starter-known-file-source-class (helm-source-sync)
  ((candidates :initform
               (lambda ()
                 (cl-loop for fpath in org-starter-known-files
                          collect (cons (helm-org-starter--format-file-candidate fpath)
                                        fpath))))))

(defcustom helm-org-starter-file-actions
  '(("Find file" . find-file))
  "Customizable action list for helm-org-starter.

The function in each item takes a file name as the argument."
  :group 'helm-org-starter
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-starter-buffer-sort-method nil
  "How to sort buffer entries in each Helm source."
  :group 'helm-org-starter
  :type '(choice (const :tag "Not sorted" nil)
                 (const :tag "By file path" filepath)))

(defun helm-org-starter--get-file-info (&optional buffer)
  "Get information on the Org BUFFER.

The result is an alist."
  (with-current-buffer (or buffer (current-buffer))
    (org-with-wide-buffer
     `((title . ,(progn (goto-char (point-min))
                        (when (re-search-forward
                               "^\\#\\+[Tt][Ii][Tt][Ll][Ee]: \\(.+\\)$"
                               nil t)
                          (match-string-no-properties 1))))
       (subtitle . ,(progn (goto-char (point-min))
                           (when (re-search-forward
                                  "^\\#\\+[Ss][Uu][Bb][Tt][Ii][Tt][Ll][Ee]: \\(.+\\)$"
                                  nil t)
                             (match-string-no-properties 1))))
       (filetags . ,(progn (goto-char (point-min))
                           (when (re-search-forward
                                  "^\\#\\+[Ff][Ii][Ll][Ee][Tt][Aa][Gg][Ss]: \\(.+\\)$"
                                  nil t)
                             (match-string-no-properties 1))))))))

(defcustom helm-org-starter-column-width 40
  "The width of the first column."
  :group 'helm-org-starter
  :type 'integer)

(defun helm-org-starter--format-buffer-candidate (buf)
  "Generate a Helm candidate title from BUF."
  (format (concat "%-" (int-to-string helm-org-starter-column-width)
                  "s : %s")
          (let ((fpath (buffer-file-name buf)))
            (concat (if fpath
                        (concat (abbreviate-file-name (file-name-directory fpath))
                                (propertize (file-name-nondirectory fpath) 'face 'helm-buffer-file))
                      (buffer-name buf))
                    (if (buffer-modified-p buf) " *" "")))
          (let-alist (helm-org-starter--get-file-info buf)
            (concat (cond
                     ((and .title .subtitle)
                      (concat (propertize .title 'face 'org-document-title)
                              " --- " .subtitle))
                     (.title (propertize .title 'face 'org-document-title))
                     (t ""))
                    (if .filetags (propertize .filetags 'face 'org-tag) "")))))

(defun helm-org-starter--group-buffers ()
  "Group live Org buffers."
  (let ((agenda-files (mapcar #'expand-file-name (org-agenda-files))))
    (seq-group-by (lambda (buf)
                    (let ((fpath (buffer-file-name buf)))
                      (cond
                       ((null fpath) 'nonfile)
                       ((cl-member fpath org-starter-deprecated-files :test #'file-equal-p)
                        'deprecated)
                       ((cl-member fpath agenda-files :test #'file-equal-p)
                        'agenda)
                       ((cl-member fpath org-starter-known-files :test #'file-equal-p)
                        'known)
                       (t 'other))))
                  (cl-remove-if-not (lambda (buf)
                                      (with-current-buffer buf
                                        (eq major-mode 'org-mode)))
                                    (buffer-list)))))

(defvar helm-org-starter-buffer-actions
  '(("Switch to buffer" . switch-to-buffer)
    ("Switch to buffer (other-window)" . switch-to-buffer)
    ("Kill buffer" . (lambda (x) (mapc #'kill-buffer (helm-marked-candidates))))))

(defun helm-org-starter--sort-buffers (buffers method)
  "Sort a list of BUFFERS with METHOD.

METHOD is a symbol that is supported by `helm-org-starter-buffer-sort-method'."
  (cl-case method
    (filepath (cl-sort buffers #'string< :key #'buffer-file-name))
    (otherwise buffers)))

(defun helm-org-starter--make-source-from-buffers (name buffers)
  "Create a Helm source named NAME with BUFFERS as its candidates."
  (declare (indent 1))
  (helm-build-sync-source name
    :action 'helm-org-starter-buffer-actions
    :candidates (mapcar (lambda (buf)
                          (cons (helm-org-starter--format-buffer-candidate buf)
                                buf))
                        (helm-org-starter--sort-buffers
                         buffers
                         helm-org-starter-buffer-sort-method))))

(defun helm-org-starter--sources-from-buffer ()
  "Build a list of Helm sources for Org buffers."
  (let ((groups (helm-org-starter--group-buffers))
        (captions '(("Org agenda files" . agenda)
                    ("Known files" . known)
                    ("Deprecated" . deprecated)
                    ("Other file buffers" . other)
                    ("Non-file buffers" .nonfile))))
    (cl-loop for (name . symbol) in captions
             collect (helm-org-starter--make-source-from-buffers name
                       (alist-get symbol groups)))))

(defun helm-org-starter--format-file-candidate (filename)
  "Format a helm candidate title for FILENAME."
  (if-let ((buf (find-buffer-visiting filename)))
      (helm-org-starter--format-buffer-candidate buf)
    (abbreviate-file-name filename)))

(defvar helm-org-starter-known-file-source
  (helm-make-source "org-starter known files"
      'helm-org-starter-known-file-source-class
    :action 'helm-org-starter-file-actions))

;;;###autoload
(defun helm-org-starter-known-files ()
  "Helm for `org-starter-known-files'."
  (interactive)
  (helm :sources 'helm-org-starter-known-file-source))

(defun helm-org-starter-create-file-in-known-directory (filename)
  "Create FILENAME in a known directory via a Helm interface."
  (let ((action (lambda (dpath) (find-file (expand-file-name filename dpath)))))
    (helm :prompt (format "Choose a directory to save %s: " filename)
          :sources
          (list (helm-build-sync-source "Directories in the path"
                  :candidates 'org-starter-path
                  :action action)
                (helm-build-sync-source "Other known directories"
                  :candidates (seq-difference org-starter-known-directories
                                              org-starter-path)
                  :action action)))))

(defvar helm-org-starter-dummy-file-source
  (helm-build-dummy-source "New file in a known directory"
    :action 'helm-org-starter-create-file-in-known-directory))

;;;###autoload
(defun helm-org-starter (&optional arg)
  "Helm for a bunch of Org files and buffers.

If prefix ARG is given, load all Org files in `org-starter-path'.
Otherwise, all known files are loaded."
  (interactive "P")
  (if arg
      (org-starter-load-all-files-in-path)
    (org-starter-load-all-known-files))
  (helm :prompt "Org buffers: "
        :buffer "*helm-org-starter*"
        :sources (append (helm-org-starter--sources-from-buffer)
                         (list 'helm-org-starter-dummy-file-source))))

(provide 'helm-org-starter)
;;; helm-org-starter.el ends here
