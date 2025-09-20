;;; -*- lexical-binding: t; -*-

;; setup package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; install htmlize if not available
(unless (package-installed-p 'htmlize)
  (package-refresh-contents)
  (package-install 'htmlize))

;; setup project publishing
(use-package htmlize)
(require 'ox-publish)
(require 'ox-html)

(require 'ansi-color)

(defun nas/org-ansi-colorize-blocks (beg end)
  "Apply ANSI colors to source block results in region."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^[[:space:]]*:.*" end t)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (match-beginning 0) (match-end 0))))))

(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (nas/org-ansi-colorize-blocks
               (org-babel-where-is-src-block-result)
               (org-babel-result-end)))))


;; htmlize output
(setq org-html-htmlize-output-type 'inline-css)
(setq org-export-with-sub-superscripts nil)
(setq org-html-doctype "html5")

(defun nas/extract-keyword-from-file (filename keyword)
  "Extract keyword from org file."
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))

      (when (re-search-forward (format "^[ \t]*#\\+%s:[ \t]*\\(.+\\)[ \t]*$" keyword) nil t)
        (string-trim (match-string 1))))))


(defun nas/generate-breadcrumb-html (crumbs-list)
  "Generate HTML for breadcrumbs."
  (let ((breadcrumb-items
         (mapcar (lambda (crumb)
                   (let ((parts (split-string crumb ":")))
                     (if (= (length parts) 2)
                         ;; format: "text:url"
                         (format "<a href=\"%s\">%s</a>"
                                (string-trim (cadr parts))
                                (string-trim (car parts)))
                       ;; format: "text" (no link)
                       (format "<span>%s</span>" (string-trim crumb)))))
                 crumbs-list)))

    (format "<nav class=\"breadcrumbs\">\n%s\n</nav>"
            (mapconcat 'identity breadcrumb-items " <span class=\"separator\">/</span> "))))

(defun nas/generate-nav-buttons-html (nav-buttons-list)
  "Generate HTML previous and/or next buttons."
  (let ((button-items
         (mapcar (lambda (button)
                   (let ((parts (split-string button ":")))
                     (if (= (length parts) 2)
                         ;; format: "text:url"
                         (format "<a href=\"%s\" class=\"nav-button\">%s</a>"
                                (string-trim (cadr parts))
                                (string-trim (car parts)))
                       ;; format: "text" (no link)
                       (format "<span class=\"nav-button disabled\">%s</span>" (string-trim button)))))
                 nav-buttons-list)))
    (format "<div class=\"nav-buttons\">\n%s\n</div>"
            (mapconcat 'identity button-items "\n"))))

(defun nas/nav-buttons-filter (string backend info)
  "Add navigation buttons after the title in HTML output."
  (when (eq backend 'html)
    (let* ((input-file (plist-get info :input-file))
           (back-button (nas/extract-keyword-from-file input-file "back"))
           (next-button (nas/extract-keyword-from-file input-file "next")))

      (when (or back-button next-button)
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))

          ;; Find the title and insert nav buttons after it
          (when (re-search-forward "<h1[^>]*class=\"title\"[^>]*>.*?</h1>" nil t)
            (let ((nav-buttons '()))
              ;; Add back button if it exists
              (when back-button
                (push back-button nav-buttons))
              ;; Add next button if it exists
              (when next-button
                (push next-button nav-buttons))

              (insert "\n" (nas/generate-nav-buttons-html (reverse nav-buttons)))))

          (buffer-string))))))

(defun nas/breadcrumbs-filter (string backend info)
  "Add breadcrumbs below the title in HTML output."
  (when (eq backend 'html)
    (let* ((input-file (plist-get info :input-file))
           (breadcrumbs (nas/extract-keyword-from-file input-file "breadcrumbs")))

      (when breadcrumbs
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))

          ;; find the title and insert breadcrumbs after it
          (when (re-search-forward "<h1[^>]*class=\"title\"[^>]*>.*?</h1>" nil t)
            (let* ((crumbs-list (split-string breadcrumbs " > "))
                   (breadcrumb-html (nas/generate-breadcrumb-html (append crumbs-list '(".")))))
              (insert "\n" breadcrumb-html)
          (buffer-string))))))))

;; filter for replacing toc
(defun my-collapsible-toc-filter (string backend _info)
  "Add collapsible functionality to TOC in final HTML output."
  (when (eq backend 'html)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))

      ;; find and replace the toc header
      (when (re-search-forward "<div id=\"table-of-contents\" role=\"doc-toc\">\n<h2>\\([^<]+\\)</h2>" nil t)
        (replace-match "<div id=\"table-of-contents\" role=\"doc-toc\">\n<div class=\"toc-header\" onclick=\"toggleToc()\">\n<h2>\\1</h2>\n<div class=\"toggle-icon\">+</div>\n</div>"))
      (buffer-string))))

;; add filters
(add-to-list 'org-export-filter-final-output-functions 'my-collapsible-toc-filter)
(add-to-list 'org-export-filter-final-output-functions 'nas/breadcrumbs-filter)
(add-to-list 'org-export-filter-final-output-functions 'nas/nav-buttons-filter)

;; set default header arguments for all source blocks
(setq org-babel-default-header-args
      '((:exports . "both")
        (:results . "output verbatim")
        (:session . "none")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")))

;; define publish project
(setq org-publish-project-alist
      '(("content"
         :base-directory "./notes"
         :publishing-directory "./public"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-creator nil
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil
         :html-head-extra "<script src=\"/static/toc-toggle.js\"></script>")
        ("static"
         :base-directory "./static"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg"
         :publishing-directory "./public/static"
         :recursive t
         :publishing-function org-publish-attachment)
        ("website" :components ("content" "static"))))

;; customize html output
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"/static/style.css\" >")

;; generate output
(org-publish-all t)

;; remove .html files in public/ that no longer have a matching .org source
(let* ((org-src-dir (expand-file-name "./notes"))
       (output-dir (expand-file-name "./public"))
       (org-html-files
        (directory-files-recursively org-src-dir "\\.org$"))
       (valid-html
        (mapcar (lambda (f)
                  (concat
                   (file-name-sans-extension
                    (expand-file-name (file-relative-name f org-src-dir) output-dir))
                   ".html"))
                org-html-files)))
  (dolist (html-file (directory-files-recursively output-dir "\\.html$"))
    (unless (member html-file valid-html)
      (delete-file html-file)
      (message "Deleted orphan: %s" html-file))))

(message "Build complete!")
