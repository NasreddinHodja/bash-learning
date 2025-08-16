;;; build-toc.el --- Generate hierarchical TOC for org files -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)

(defvar notes-dir "notes/"
  "Directory containing org files.")

(defvar notes-index-file "notes/index.org"
  "Path to the index file.")

(defvar toc-title "Table of Contents"
  "Title of the section in 'notes-index-file' containg the table of contents")

(defun validate-env ()
  "Validate environment"

  (unless (file-directory-p notes-dir)
    (error "Notes directory %s does not exist" notes-dir))

  (unless (file-exists-p notes-index-file)
    (message "Creating index file %s" notes-index-file)
    (create-index-file)))

(defun get-notes-files ()
  "Returns all .org files in 'notes-dir' except 'notes-index-file'"

  (seq-filter (lambda (f)
                                (not (string= f (file-name-nondirectory notes-index-file))))
                              (directory-files notes-dir nil "\\.org$")))

(defun print-message-completed ()
  "Print completion message"

    (message "TOC generation complete! Processed %d org files."
             (length (get-notes-files))))

(defun create-index-file ()
  "Create index file 'notes-index-file'"

  (let ((title (read-string "Notes title: ")))
    (with-temp-file notes-index-file
      (insert (format "#+title: %s\n" title))
      (insert "\n")
      (insert (format "* %s\n\n" toc-title)))))

(defun collect-files-info ()
  "Collect info from all note files"

  (let ((files-info '()))
    (dolist (filename (get-notes-files))
      (let ((filepath (expand-file-name filename notes-dir)))
        (message (format "OIEEEE %s" filepath))
        ;; TODO collec file info
        ))

    files-info))

(collect-files-info)

(defun generate-toc-content ()
  "Generate hierarchical TOC lines for all org files"

  (let ((files-info (collect-files-info))
        (toc-lines '()))

    ;; TODO Generate lines for each file

    toc-lines))

(defun update-toc ()
  "Update * Table of Contents in 'notes-index-file'"

  (with-temp-buffer
    (insert-file-contents notes-index-file)
    (org-mode)
    (goto-char (point-min))

    (if (re-search-forward (format "^\\* %s" toc-title) nil t)
        (progn
          (let ((toc-start (point)))
            (if (re-search-forward "^\\*" nil t)
                (progn
                  (beginning-of-line)
                  (delete-region toc-start (point)))
              (delete-region toc-start (point-max)))))

      (goto-char (point-max))
      (unless (bolp) insert "\n")
      (insert (format "* %s" toc-title)))

    (insert "\n")
    ;; <fill (under the heading) with the generated lines of the TOC section>

    (write-file notes-index-file)))

(defun generate-notes-toc ()
  "Main function to generate notes TOC"
  (interactive)

  (validate-env)

  (message "Generating TOC for notes in %s..." notes-index-file)
  (update-toc)

  (print-message-completed))

(provide 'build-toc)
;;; build-toc.el ends here
