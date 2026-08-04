;;; own-notes.el --- System for write notes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-id)

(defgroup own-notes nil
  "My own system for writter notes."
  :group 'convenience
  :package-version '(own-notes . "0.0.1")
  :tag "Own Notes")

(defvar own-notes--default-directory "~/.emacs.d/own-notes/")
(defconst own-notes-extension "org" "Extension for notes.")

(defcustom own-notes-directory (expand-file-name own-notes--default-directory)
  "Default directory path for own-notes."
  :type 'path
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol (expand-file-name value)))
  :group 'own-notes)

(defun own-notes--get-property-from-note (filename property)
  "Get property of a note from file name.
The `FILENAME' should be a exists org file on `own-notes-directory'.
If `PROPERTY' not exist return nil."
  (let ((value) (property (upcase property)))
    (with-current-buffer (find-file-noselect filename)
      (setq value (cadr (assoc property (org-collect-keywords `(,property))))))
    (when value (substring-no-properties value))))

(defun own-notes--get-all-notes ()
  "TODO: Dcostring."
  (directory-files-recursively own-notes-directory "\\.org$"))

(defun own-notes--get-property-completions (property)
  "Search files recursively and get ours `PROPERTY' for completions."
  (let ((notes (own-notes--get-all-notes))
        (properties '()))
    (dolist (note notes)
      (push (own-notes--get-property-from-note note property) properties))
    properties))

(defun own-notes--get-title-completions ()
  "TODO: Docstring."
  (own-notes--get-property-completions "title"))

(defun own-notes--get-tags-completions ()
  "TODO: Docstring."
  (let ((tags-for-files (own-notes--get-property-completions "filetags"))
        (tags '()))
    (dolist (tags-for-file tags-for-files)
      (let ((tags-for-file (seq-filter #'(lambda (tag)
                                           (not (string-empty-p tag)))
                                       (string-split tags-for-file ":"))))
        (dolist (tag tags-for-file)
          (unless (seq-contains-p tags tag)
            (push tag tags)))))
    tags))

(defvar-local own-notes--title-completions nil)
(defvar-local own-notes--tags-completions nil)
(defvar-local own-notes--files-completions nil)

(defun own-notes--update-title-completions ()
  "TODO: Dcostring."
  (setq-local own-notes--title-completions (own-notes--get-title-completions)))

(defun own-notes--update-tags-completions ()
  "TODO: Dcostring."
  (setq-local own-notes--tags-completions (own-notes--get-title-completions)))

(defun own-notes--update-files-completions ()
  "TODO: Dcostring."
  (setq-local own-notes--files-completions (own-notes--get-all-notes)))

(defun own-notes--get-title ()
  "TODO: Dcostring."
  (interactive)
  (completing-read "title: " own-notes--title-completions nil nil))

(defun own-notes--get-tags ()
  "TODO: Docstring."
  (interactive)
  (completing-read-multiple "tags: " own-notes--tags-completions nil nil))

(defun own-notes--processing-tags (tags)
  "TODO: Docstring, `TAGS'."
  (if tags (format ":%s:" (string-join (mapcar #'string-trim tags) ":")) ""))

(defun own-notes--create-new-uuid ()
  "TODO: Docstring."
  (org-id-new "note"))

(defun own-notes-create-new (filename)
  "TODO: Docstring, `FILENAME'."
  (let*  ((title (call-interactively #'own-notes--get-title))
          (tags  (call-interactively #'own-notes--get-tags))
          (tags  (own-notes--processing-tags tags)))
    (make-empty-file filename)
    (own-notes--format-new-note :filename filename
                                :title title
                                :tags tags)))

(defun own-notes--format-new-note (&rest args)
  "TODO: Docstring, `ARGS'."
  (let ((filename (plist-get args :filename))
        (title    (plist-get args :title))
        (tags     (plist-get args :tags))
        (date     (format-time-string "[%Y-%m-%d %A %H:%M]"))
        (uuid     (own-notes--create-new-uuid))
        (note-format   nil))
    (setq note-format (concat (format "#+title: %s\n" title)
                              (format "#+filetags: %s\n" tags)
                              (format "#+date: %s\n" date)
                              (format "#+idendifier: %s\n" uuid)))
    (with-current-buffer (find-file-noselect filename)
      (insert note-format)
      (save-buffer))
    (own-notes--update-title-completions)
    (own-notes--update-tags-completions)))

(defun own-notes--format-filename (filename)
  "TODO: Docstring, `FILENAME'."
  (let ((filename  (expand-file-name filename))
        (extension (file-name-extension filename)))
    (unless extension
      (setq filename (concat filename "." own-notes-extension)))
    filename))

(defun own-notes--create-subdirectory (filename)
  "TODO: Docstring, `FILENAME'."
  (make-directory (file-name-directory filename) t))

(defun own-notes--get-filename ()
  "TODO: Docstring."
  (interactive)
  (own-notes--format-filename
   (let ((completions own-notes--files-completions)
         (prompt "note: "))
     (if completions
         (completing-read prompt completions)
       (read-file-name prompt own-notes-directory)))))

(defun own-notes-subdirectory ()
  "Open or create note in a subdirectory."
  (interactive)
  (let* ((default-directory own-notes-directory)
         (filename (call-interactively #'own-notes--get-filename))
         (subdirectory (file-name-directory filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (unless (directory-name-p subdirectory)
        (own-notes--create-subdirectory filename))
      (own-notes-create-new filename)
      (find-file filename))))

(defun own-notes-add-link ()
  "TODO: Docstring."
  (interactive)
  (let* ((default-directory own-notes-directory)
         (filename (call-interactively #'own-notes--get-filename))
         (linkname (read-string "Description: " nil nil "link")))
    (unless (file-exists-p filename)
      (unless (directory-name-p (file-name-directory filename))
        (own-notes--create-subdirectory filename))
      (own-notes-create-new filename))
    (org-insert-link "file:" filename linkname)))

;;;###autoload
(defun own-notes-open-or-create ()
  "Open or create note.  If not exist create a new in `own-notes-direcotory'."
  (interactive)
  (let* ((default-directory own-notes-directory)
         (filename (call-interactively #'own-notes--get-filename))
         (exists   (file-exists-p filename)))
    (unless exists (own-notes-create-new filename))
    (find-file filename)))

;;;###autoload
(defun own-notes ()
  "Alias for `own-notes-open-or-create'."
  (interactive)
  (own-notes-open-or-create))

(unless (directory-name-p own-notes-directory)
  (make-directory own-notes-directory t))

(provide 'own-notes)
;;; own-notes.el ends here
