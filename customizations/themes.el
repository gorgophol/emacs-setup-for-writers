;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; (load-theme 'tomorrow-night-bright t)
;; (load-theme 'tomorrow t)
;;(load-theme 'subatomic t)
;; (load-theme 'monokai t)
 (load-theme 'zenburn t)
;; (load-theme 'paper t)
;; (load-theme 'material-light t)
;; (load-theme 'material t)
 (load-theme 'leuven t)
;; (load-theme 'spacegray t)

;; Fontify the whole line for headings (with a background color). Works well with Leuven theme
(setq org-fontify-whole-heading-line t)


