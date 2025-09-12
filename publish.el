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

;; htmlize output
(setq org-html-htmlize-output-type 'inline-css)
(setq org-export-with-sub-superscripts nil)
(setq org-html-doctype "html5")

(defun nas/breadcrumbs-filter (string backend info)
  "Add breadcrumbs below the title in HTML output."
  (when (eq backend 'html)
    (let* ((input-file (plist-get info :input-file))
           (breadcrumbs (nas/get-breadcrumbs-from-file input-file)))

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

(defun nas/get-breadcrumbs-from-file (filename)
  "Extract BREADCRUMBS keyword from org file."
  (when (and filename (file-exists-p filename))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))

      (when (re-search-forward "^[ \t]*#\\+breadcrumbs:[ \t]*\\(.+\\)[ \t]*$" nil t)
        (string-trim (match-string 1))))))

(defun nas/generate-breadcrumb-html (crumbs-list)
  "Generate HTML for breadcrumbs from a list of crumb specifications."
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

;; add both filters
(add-to-list 'org-export-filter-final-output-functions 'my-collapsible-toc-filter)
(add-to-list 'org-export-filter-final-output-functions 'nas/breadcrumbs-filter)

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
