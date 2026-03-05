;;; knorgpub project config

;; directories
(setq knorgpub/content-dir (expand-file-name "content" default-directory))
(setq knorgpub/public-dir  (expand-file-name "public"  default-directory))

;; table of contents
(setq knorgpub/toc-title "Table of Contents")

;; html output
(setq knorgpub/css-file  "/static/style.css")
(setq knorgpub/favicon   "/static/favicon.ico")  ; set to nil to disable
(setq knorgpub/scripts   '("toc-toggle.js" "scroll-to-top.js"))
(setq knorgpub/with-toc  t)
(setq knorgpub/section-numbers nil)

;; org-babel source block defaults
(setq knorgpub/babel-header-args
      '((:exports . "both")
        (:results . "output verbatim")
        (:session . "none")
        (:cache   . "no")
        (:noweb   . "no")
        (:hlines  . "no")
        (:tangle  . "no")))
