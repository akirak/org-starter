;;; org-starter-utils.el --- A collection of utilities for Org mode -*- lexical-binding: t; byte-compile-warnings: (not noruntime) -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.2.6
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

;; This library defines a bunch of functions that do not depend on
;; org-starter itself but can be useful with/without the package.

;;; Code:

(declare-function 'avy-jump "ext:avy")
(declare-function 'avy-goto-line "ext:avy")
(declare-function 'avy-with "ext:avy")

;;;; Avy utilities for Org mode

;; Based on part of `avy-org-refile-as-child' in avy.el.
(eval-when-compile
  (require 'avy nil t)
  (defmacro org-starter-utils--with-avy (&rest progn)
    "Select an Org heading with avy and evaluate PROGN."
    `(unless (eq 't (progn
                      (require 'avy)
                      (avy-with avy-goto-line
                        (avy-jump (rx bol (1+ "*") (1+ space))))))
       (unless (derived-mode-p 'org-mode)
         (user-error "Not in org-mode"))
       ,@progn)))

;;;###autoload
(defun org-starter-utils-avy-id ()
  "Retrieve the ID to an entry selected with avy."
  (save-excursion
    (org-starter-utils--with-avy
     (org-id-get-create t))))

;;;###autoload
(defun org-starter-utils-avy-custom-id ()
  "Retrieve the custom ID to an entry selected with avy.

This function returns a pair of a file name of the entry and the
custom ID.

If the selected entry does not have a custom ID, confirm the user
to create it."
  (save-excursion
    (org-starter-utils--with-avy
     (cons (or (buffer-file-name)
               (buffer-file-name (org-base-buffer (current-buffer)))
               (user-error "This is not a file buffer"))
           (or (org-entry-get nil "CUSTOM_ID")
               (progn
                 (org-set-property "CUSTOM_ID" nil)
                 (org-entry-get nil "CUSTOM_ID")))))))

;;;###autoload
(defun org-starter-utils-avy-store-link-to-heading ()
  "Store a link to an Org heading selected with avy."
  (save-excursion
    (org-starter-utils--with-avy
     (org-store-link nil))))

(provide 'org-starter-utils)
;;; org-starter-utils.el ends here
