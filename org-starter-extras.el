;;; org-starter-extras.el --- Utilities that require extra dependencies -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org-starter "0.2"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a bunch of utilities for org-starter that
;; depend on extra packages (e.g. org-reverse-datetree) other than
;; org-starter itself.

;;; Code:

(require 'cl-lib)
(require 'org-starter)

(declare-function 'org-reverse-datetree-refile-to-file "org-reverse-datetree")

;;;###autoload
(cl-defmacro org-starter-extras-def-reverse-datetree-refile
    (filename date-properties &key ask-always)
  "Define a refile function to a reverse datetree.

FILENAME is the name of the file which usually doesn't contain a
directory. See `org-starter-define-file'.

DATE-PROPERTIES is a list of property names to be used as the
target date.

If ASK-ALWAYS is non-nil, it is used as the value of the same
property of `org-reverse-datetree-refile-to-file'.
If it is nil, it is determined by the prefix argument."
  (declare (indent 1))
  (let ((function-name (intern (format "org-starter-refile-%s-datetree"
                                       (file-name-sans-extension filename)))))
    `(defun ,function-name (arg)
       (interactive "P")
       (require 'org-reverse-datetree)
       (org-reverse-datetree-refile-to-file
        (org-starter-locate-file ,filename nil t) nil
        :ask-always (or ,ask-always arg)
        :prefer ,(cl-typecase date-properties
                   (symbol (symbol-value date-properties))
                   (t date-properties))))))

(provide 'org-starter-extras)
;;; org-starter-extras.el ends here
