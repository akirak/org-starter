;;; counsel-org-starter.el --- Counsel interface to org-starter -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (ivy "0.10") (memoize "1.1") (org-starter "0.1"))
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

;; This is a Counsel/Ivy interface to org-starter.

;;; Code:

(require 'ivy)
(require 'org-starter)
(require 'cl-lib)
(require 'seq)
(require 'memoize)

(defcustom counsel-org-starter-goto-beginning nil
  "If non-nil, jump to the beginning of the file."
  :type 'boolean
  :group 'org-starter)

(defun counsel-org-starter--get-buffer (filename)
  "Get a buffer visiting a known file named FILENAME (with/without its directory)."
  (let* ((fpath (org-starter-locate-file filename nil t))
         (buf (or (find-buffer-visiting fpath)
                  (find-file-noselect fpath))))
    (when counsel-org-starter-goto-beginning
      (with-current-buffer buf
        (goto-char (point-min))))
    buf))

(defun counsel-org-starter--file-list (&optional deprecated agenda-files)
  "Return a list of known Org files.

By default, this function returns a list of files in `org-starter-known-file',
excluding deprecated files.  Each file name in the result does not contain its
directory.

If DEPRECATED is non-nil, the result includes deprecated files.

If AGENDA-FILES is non-nil, files in `org-agenda-files` are appended to the
result."
  (let ((files (cl-copy-list org-starter-known-files))
        filenames)
    (unless deprecated
      (cl-delete-if (lambda (fpath) (member fpath org-starter-deprecated-files))
                    files))
    (setq filenames (mapcar #'file-name-nondirectory files))
    (if agenda-files
        (append filenames
                (mapcar #'abbreviate-file-name
                        (seq-difference (org-agenda-files) files #'file-equal-p)))
      filenames)))

(memoize 'counsel-org-starter--file-list)

(defun counsel-org-starter-known-file (&optional arg)
  "Choose a known file.

If prefix ARG is given, deprecated files are included in the candidates."
  (interactive "P")
  (ivy-read "org-starter known file: "
            (counsel-org-starter--file-list arg)
            :caller 'counsel-org-starter-known-file
            :action (lambda (cand)
                      (switch-to-buffer (counsel-org-starter--get-buffer cand)))))

(defun counsel-org-starter (&optional arg)
  "Choose a known file or a file in `org-agenda-files'.

If prefix ARG is given, deprecated files are included in the candidates."
  (interactive "P")
  (ivy-read "org-starter: "
            (counsel-org-starter--file-list arg t)
            :caller 'counsel-org-starter-known-file
            :action (lambda (cand)
                      (switch-to-buffer (counsel-org-starter--get-buffer cand)))))

(ivy-add-actions
 #'counsel-org-starter-known-file
 '(("j" (lambda (cand)
          (switch-to-buffer-other-window (counsel-org-starter--get-buffer cand)))
    "other window")))

(provide 'counsel-org-starter)
;;; counsel-org-starter.el ends here
