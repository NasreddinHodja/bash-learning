;;; build-toc.el --- Generate hierarchical TOC for org files -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; This package provides functionality to automatically generate a hierarchical
;;; table of contents for a collection of Org-mode files.
;;;
;;; The main function `generate-notes-toc' scans all .org files in a notes
;;; directory, extracts their titles, index numbers, and heading structures,
;;; then generates a comprehensive TOC in an index.org file.
;;;
;;; Features:
;;; - Automatic TOC generation from multiple org files
;;; - Hierarchical structure with proper numbering (1.0, 1.1, 1.1.1, etc.)
;;; - Clickable org-mode links to sections and files
;;; - Customizable notes directory and index file locations
;;; - Interactive user prompts for index file creation
;;;
;;; Usage:
;;; (require 'build-toc)
;;; (generate-notes-toc)
;;;
;;; Customization:
;;; Set `notes-dir' and `notes-index-file' variables to customize paths.
;;;
;;; Code:

(require 'org)
(require 'org-element)

(defvar notes-dir "notes/"
  "Directory containing org files.")

(defvar notes-index-file "notes/index.org"
  "Path to the index file.")

(defvar toc-title "Table of Contents"
  "Title of the section in `notes-index-file' containg the table of contents.")

(defun validate-env ()
  "Validate environment."

  (unless (file-directory-p notes-dir)
    (error "Notes directory %s does not exist" notes-dir))

  (unless (file-exists-p notes-index-file)
    (message "Creating index file %s" notes-index-file)
    (create-index-file)))

(defun get-notes-files ()
  "Return all .org files in `notes-dir' except `notes-index-file'."

  (seq-filter (lambda (f)
                (not (string= f (file-name-nondirectory notes-index-file))))
              (directory-files notes-dir nil "\\.org$")))

(defun print-message-completed ()
  "Print completion message."

  (message "TOC generation complete! Processed %d org files."
           (length (get-notes-files))))

(defun create-index-file ()
  "Create index file `notes-index-file'."

  (let ((title (read-string "Notes title: ")))
    (with-temp-file notes-index-file
      (insert (format "#+title: %s\n" title))
      (insert "\n")
      (insert (format "* %s\n\n" toc-title)))))

(defun extract-note-headings ()
  "Extract headings from the current buffer."

  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (headline)
      (list :title (org-element-property :raw-value headline)
            :level (org-element-property :level headline)))))

(defun extract-note-info (filepath)
  "Extract title, index number, and headings from FILEPATH."

  (with-temp-buffer
    (insert-file-contents filepath)
    (org-mode)
    (let* ((filename (file-name-nondirectory filepath))
           (relative-path (concat "./" filename))  ; for links from index.org
           (title (or (cadar (org-collect-keywords '("title")))
                      filename))
           (index-num (string-to-number (cadar (org-collect-keywords '("index")))))
           (headings (extract-note-headings)))
      (list :link relative-path
            :title title
            :index index-num
            :headings headings))))

(defun collect-notes-infos ()
  "Collect info from all note files."

  (let ((notes-infos '()))
    (dolist (filename (get-notes-files))
      (let ((filepath (expand-file-name filename notes-dir)))
        (message "Processing %s..." filename)
        (push (extract-note-info filepath) notes-infos)))
    (sort notes-infos (lambda (a b) (< (plist-get a :index)
                                       (plist-get b :index))))))

(defun generate-toc-section (note-info)
  "Generate TOC section for one NOTE-INFO."

  (let* ((link (plist-get note-info :link))
         (title (plist-get note-info :title))
         (index (plist-get note-info :index))
         (headings (plist-get note-info :headings))
         (section-lines '())
         (heading-counter 0))

    ;; Add file entry as ** heading
    (push (format "** [[%s][%d.0 %s]]" link index title) section-lines)

    ;; Add each heading
    (dolist (heading headings)
      (setq heading-counter (1+ heading-counter))
      (let* ((h-title (plist-get heading :title))
             (h-level (plist-get heading :level))
             (number (format "%d.%d" index heading-counter))
             (bullet (if (= h-level 1) "+" "-"))
             (indent (make-string (* (- h-level 1) 2) ?\s))
             (link-target (format "%s::*%s" link h-title)))
        (push (format "%s%s [[%s][%s %s]]"
                      indent bullet link-target number h-title)
              section-lines)))

    (reverse section-lines)))

(defun generate-toc-content ()
  "Generate hierarchical TOC lines for all org files."

  (let ((notes-info (collect-notes-infos))
        (toc-lines '()))
    (dolist (note-info notes-info)
      (let ((section-lines (generate-toc-section note-info)))
        (setq toc-lines (append toc-lines section-lines))))
    toc-lines))

(defun update-toc ()
  "Update * Table of Contents in `notes-index-file'."

  (with-temp-buffer
    (insert-file-contents notes-index-file)
    (org-mode)
    (goto-char (point-min))

    (when (re-search-forward (format "^\\* %s" toc-title) nil t)
      (beginning-of-line)
      (let ((section-start (point)))
        (forward-line 1)
        (if (re-search-forward "^\\* " nil t)
            (progn
              (beginning-of-line)
              (delete-region section-start (point)))
          (delete-region section-start (point-max)))))

    (unless (save-excursion
              (goto-char (point-min))
              (re-search-forward (format "^\\* %s" toc-title) nil t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n")))

    (insert (format "* %s\n" toc-title))
    (let ((toc-lines (generate-toc-content)))
      (dolist (line toc-lines)
        (insert line "\n")))

    (write-file notes-index-file)
    ))

(defun generate-notes-toc ()
  "Main function to generate notes TOC."

  (interactive)

  (validate-env)

  (message "Generating TOC for notes in %s..." notes-index-file)
  (update-toc)

  (print-message-completed))

(provide 'build-toc)
;;; build-toc.el ends here
