;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Show line numbers
;;(global-linum-mode)

;; Default font and font-size
(set-face-attribute 'default nil
                    :family "Sans"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(setq line-spacing '0.25)

(setq org-hide-emphasis-markers t)

;; Better bullets 
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; make igore of headlines possible
(defun org-remove-headlines (backend)
  "Remove headlines with :no_title: tag."
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "no_title"))
(add-hook 'org-export-before-processing-hook #'org-remove-headlines)
