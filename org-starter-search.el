;;; org-starter-search.el --- Agenda integration with org-starter -*- lexical-binding: t -*-

(require 'org-starter)
(require 'org-id)
(require 'org-ql)
(require 'org-ql-agenda)
(require 'eieio)

(defgroup org-starter-search nil
  "Search facility for org-starter."
  :group 'org-starter)

;;;; Variables

(defvar org-starter-search-query-tracker nil
  "Track search queries.")

(defvar org-starter-search-predicate-history nil)

(defvar org-starter-search-temporary-agenda-files nil)

(defvar org-starter-search-predicate-minibuffer-map
  (make-composed-keymap minibuffer-local-map emacs-lisp-mode-map))

;;;; org-starter-search-generic-query

;; This needs to be defined before `org-starter-search-generic-query'.
(defun org-starter-search--uuid ()
  "Generate a UUID."
  ;; Just stolen an implementation from org-id.el
  (org-trim (shell-command-to-string org-id-uuid-program)))

(defclass org-starter-search-generic-query
  (eieio-instance-tracker
   eieio-persistent)
  ((tracking-symbol :initform org-starter-search-query-tracker)
   (title :initarg :title :initform nil
          :type (or null string)
          :documentation "Optional title for the human.")
   (uuid :initform (org-starter-search--uuid)
         :type string
         :documentation "Unique identifier.")
   (created :initform (current-time)
            :type list
            :documentation "When the search object was created.")
   (last-run :initform (current-time)
             :type list
             :documentation "The last search by the query was done.")
   (counter :initform 0
            :type integer
            :documentation "How many times the query was called.")
   (starred :initarg :starred :initform nil
            :type (or null list)
            :documentation "Whether the query is starred.")
   (key :initarg :key :initform nil
        :type (or null string)
        :documentation "Keybinding to call the query quickly."))
  "Generic class for a search query object."
  :abstract t)

;;;;; Operations and utility functions on org-starter-search-generic-query
;;;###autoload
(defmethod org-starter-search-dispatch ((obj org-starter-search-generic-query))
  "Dispatch a search OBJ and log the execution."
  (org-starter-search-dispatch-internal obj)
  (org-starter-search--log obj))

(defmethod org-starter-search--log ((obj org-starter-search-generic-query))
  "Log the execution of query OBJ."
  (oset obj last-run (current-time))
  (oset obj counter (1+ (oref obj counter))))

(defmethod org-starter-search-dispatch-internal ((obj org-starter-search-generic-query))
  "Mean body of a search dispatch of OBJ.

This method is meant to be the implementation of a search query.")

;;;;; Operations on the instance list

(defmethod org-starter-search--delete-from-history
  ((query-object org-starter-search-generic-query))
  "Delete QUERY-OBJECT from the instance list."
  (delete-instance query-object))

(defun org-starter-search--find-query-by-uuid (uuid)
  "Find a query object by UUID."
  (eieio-instance-tracker-find uuid 'uuid 'org-starter-search-query-tracker))

(defun org-starter-search--get-queries-by-class (class)
  "Find query objects of CLASS."
  (-filter (lambda (obj) (object-of-class-p obj class))
           org-starter-search-query-tracker))

(defun org-starter-search--query-history ()
  "Return the search history without duplication."
  (-sort (lambda (a b)
           (> (oref a last-run) (oref b last-run)))
         org-starter-search-query-tracker))

;;;; org-starter-search-org-query
;;;;; Temporary agenda files

;;;###autoload
(defun org-starter-search-push-agenda-file (file)
  "Push FILE to `org-starter-search-temporary-agenda-files'."
  (interactive (list (read-file-name "Org file to add to the agenda files: "
                                     default-directory nil t
                                     (when (derived-mode-p 'org-mode)
                                       (buffer-file-name))
                                     (lambda (fp)
                                       (string-match-p org-agenda-file-regexp fp)))))
  (setq org-starter-search-temporary-agenda-files
        (cl-adjoin file org-starter-search-temporary-agenda-files
                   :test #'file-equal-p)))

;;;###autoload
(defun org-starter-search-delete-agenda-file (file)
  "Delete FILE from `org-starter-search-temporary-agenda-files'."
  (interactive (list (completing-read "Delete an agenda file: "
                                      org-starter-search-temporary-agenda-files
                                      nil t)))
  (delq file org-starter-search-temporary-agenda-files))

;;;;; Class definition

(defclass org-starter-search-org-query
  (org-starter-search-generic-query)
  ((files :initarg :files
          :type (or null list))
   (pred :initarg :pred
         :type list)
   (sort :initarg :sort
         :type list)
   (groups :initarg :groups
           :type list))
  "Search query class for org-ql.")

;;;;; Dispatch
(defmethod org-starter-search-dispatch-internal ((obj org-starter-search-org-query))
  (let* ((files (or (oref obj files) (org-agenda-files)))
         (bufs (mapcar (lambda (file)
                         (cl-etypecase file
                           (buffer file)
                           (string (let ((file (if (file-name-absolute-p file)
                                                   file
                                                 (org-starter-locate-file file nil t))))
                                     (or (find-buffer-visiting file)
                                         (find-file-noselect file))))))
                       files)))
    (org-ql-agenda--agenda bufs
      (oref obj pred)
      :sort (oref obj sort)
      :super-groups (oref obj groups))))

;;;;; User interface to build a query

;;;###autoload
(defun org-starter-search-org-create-and-dispatch (source &optional title)
  "Create a new search object on SOURCE with TITLE and dispatch it."
  (interactive (list org-starter-search-temporary-agenda-files
                     (when current-prefix-arg
                       (read-string "Title of the query: "))))
  (org-starter-search-dispatch
   (org-starter-search-org-make-query source title)))

;;;;;; Functions

(defun org-starter-search-org-make-query (files &optional title)
  "Interactively build an instance of of org query.

This function lets you build an instance of
`org-starter-search-org-query' interactively."
  (let* ((pred (org-starter-search-org--read-predicate "Filter: " files))
         (sort (org-starter-search-org--read-sort "Sort: " files))
         (groups (org-starter-search-org--read-groups "Groups: " files)))
    (make-instance 'org-starter-search-org-query
                   :title title
                   :files files
                   :pred pred
                   :sort sort
                   :groups groups)))

(defun org-starter-search-org--read-predicate (prompt files-or-buffers)
  ;; TODO: Better input interface
  (read-from-minibuffer prompt
                        nil org-starter-search-predicate-minibuffer-map
                        'sexp
                        org-starter-search-predicate-history))

(defun org-starter-search-org--read-sort (prompt files-or-buffers)
  ;; FIXME: Add support for nil as well as multiple values
  (mapcar #'intern
          (list (completing-read prompt
                                 '(date deadline scheduled todo priority)))))

(defun org-starter-search-org--read-groups (prompt files)
  ;; FIXME: Read groups
  nil)

(provide 'org-starter-search)
;;; org-starter-search.el ends here
