;;; org-starter-swiper.el --- Swiper for org-starter -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (swiper "0.11"))
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

;; This library provides swiper integration for org-starter.
;; At present, only `org-starter-swiper-config-files' is provided.

;;; Code:

(require 'org-starter)
(require 'swiper)
(require 'ivy)

(defgroup org-starter-swiper nil
  "Swiper integration for org-starter."
  :group 'org-starter
  :group 'swiper)

;;;###autoload
(defun org-starter-swiper-config-files ()
  "Run `swiper-multi' for the config files."
  (interactive)
  (let ((buffers (mapcar (lambda (file)
                           (or (find-buffer-visiting file)
                               (find-file-noselect file)))
                         (org-starter--get-existing-config-files))))
    (ivy-read "Swiper: " (swiper--multi-candidates buffers)
              :action #'swiper-multi-action-2
              :update-fn #'org-starter-swiper--update-fn
              :unwind #'swiper--cleanup
              :caller 'org-starter-swiper-config-files)))

(defun org-starter-swiper--config-file-other-window (x)
  "Move to candidate X."
  (when (> (length x) 0)
    (let ((buffer-name (get-text-property 0 'buffer x)))
      (when buffer-name
        (switch-to-buffer-other-window buffer-name)
        (goto-char (point-min))
        (forward-line (1- (read (get-text-property 0 'swiper-line-number x))))
        (re-search-forward
         (ivy--regex ivy-text)
         (line-end-position) t)))))

(ivy-add-actions 'org-starter-swiper-config-files
                 '(("j" org-starter-swiper--config-file-other-window "other window")))

(defun org-starter-swiper--update-fn ()
  "Function run when input is updated in `org-starter-swiper-config-files'."
  (when-let ((current (ivy-state-current ivy-last))
             (buffer-name (get-text-property 0 'buffer current)))
    (with-ivy-window
      (swiper--cleanup)
      (switch-to-buffer buffer-name)
      (goto-char (point-min))
      (forward-line (1- (read (get-text-property 0 'swiper-line-number current))))
      (re-search-forward
       (ivy--regex ivy-text)
       (line-end-position) t)
      (isearch-range-invisible (line-beginning-position)
                               (line-end-position))
      (swiper--add-overlays
       (ivy--regex ivy-text)))))

(provide 'org-starter-swiper)
;;; org-starter-swiper.el ends here
