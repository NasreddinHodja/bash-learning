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

;; collapsible TOC filter using final output
(defun my-collapsible-toc-filter (string backend _info)
  "Add collapsible functionality to TOC in final HTML output."
  (when (eq backend 'html)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      ;; Find and replace the TOC header
      (when (re-search-forward "<div id=\"table-of-contents\" role=\"doc-toc\">\n<h2>\\([^<]+\\)</h2>" nil t)
        (replace-match "<div id=\"table-of-contents\" role=\"doc-toc\">\n<h2 onclick=\"toggleTOC()\">\\1<span class=\"toggle-icon\">▼</span></h2>"))
      (buffer-string))))

;; add the filter
(add-to-list 'org-export-filter-final-output-functions 'my-collapsible-toc-filter)

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
         :html-head-extra "
<script>
function toggleTOC() {
    const content = document.getElementById('text-table-of-contents');
    const icon = document.querySelector('.toggle-icon');

    if (content && icon) {
        content.classList.toggle('expanded');
        icon.classList.toggle('expanded');
    }
}

document.addEventListener('DOMContentLoaded', function() {
    const content = document.getElementById('text-table-of-contents');
    const icon = document.querySelector('.toggle-icon');

    if (content && icon) {
        content.classList.remove('expanded');
        icon.classList.remove('expanded');
    }
});
</script>
"
         )

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

;; generate site output
(org-publish-all t)

;; compute which html files are still valid based on existing .org files
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
  ;; remove .html files in public/ that no longer have a matching .org source
  (dolist (html-file (directory-files-recursively output-dir "\\.html$"))
    (unless (member html-file valid-html)
      (delete-file html-file)
      (message "Deleted orphan: %s" html-file))))

(message "Build complete!")
