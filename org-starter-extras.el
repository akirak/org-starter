;;; org-starter-extras.el --- Utilities that require extra dependencies -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org-starter "0") (org-reverse-datetree "0"))
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

;; This library provides a bunch of utilities for org-starter that
;; depend on extra packages (e.g. org-reverse-datetree) other than
;; org-starter itself.

;;; Code:

(require 'cl-lib)
(require 'org-starter)

(declare 'org-reverse-datetree-refile-to-file "org-reverse-datetree")

;;;###autoload
(cl-defmacro org-starter-extras-def-reverse-datetree-refile
    (filename date-properties)
  "Define a refile function to a reverse datetree.

FILENAME is the name of the file which usually doesn't contain a
directory
(see `org-starter-define-file').

DATE-PROPERTIES is a list of property names to be used as the
target date."
  (declare (indent 1))
  (let ((function-name (intern (format "org-starter-refile-%s-datetree"
                                       (file-name-sans-extension filename)))))
    `(defun ,function-name (arg)
       (interactive "P")
       (org-reverse-datetree-refile-to-file
        (org-starter-locate-file ,filename nil t) nil
        :ask-always arg
        :prefer ,(cl-typecase date-properties
                   (symbol (symbol-value date-properties))
                   (t date-properties))))))

(provide 'org-starter-extras)
;;; org-starter-extras.el ends here
