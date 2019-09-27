;; Open a tree in the right frame in an indirect buffer with S-return
(defun org-tree-open-in-right-frame ()
  (interactive)
  (org-tree-to-indirect-buffer)
  (windmove-right))

;;(define-key cua--rectangle-keymap [C-return] nil) ;; frees the keybinding for my uses
;;(global-set-key (kbd "C-return") `org-tree-open-in-right-frame) ;;bind it to my indirect buffer setting
;;(global-set-key (kbd "C--") `cua-set-rectangle-mark)
;;(define-key cua--rectangle-keymap (kbd "C--") `cua-set-rectangle-mark)



;;MY OWN KEYBINDINGS
;; Word-Counting per Headline and view Tags
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'org-tags-view)
            (local-set-key (kbd "C-c w") 'org-wc-display)
            (local-set-key (kbd "C-c q") 'org-wc-remove-overlays)))
;; use indirect buffer
  (define-key org-mode-map [S-return] nil) ;; frees the keybinding from org-mode for my use
  (define-key global-map (kbd "<S-return>") `org-tree-open-in-right-frame)


;; spellchecking
       (setq-default ispell-program-name "aspell")
       (set-default 'ispell-change-dictionary "de_DE")
       (set-default 'ispell-local-dictionary "de_DE")
       ;;(set-default 'ispell-change-dictionary "ndeutsch8-15")
       ;;(set-default 'ispell-local-dictionary "ndeutsch8-15")
       ;;  (set-default ispell-change-dictionary "english")
       ;;  (set-default ispell-local-dictionary "english")
       ;; ===================================================================
       ;; ISPELL
       ;; ===================================================================
       (require 'ispell)
       (add-to-list 'ispell-dictionary-alist
                    '("ndeutsch8-15"
                      "[a-zA-Z\304\326\334\344\366\337\374]"
                      "[^a-zA-Z\304\326\334\344\366\337\374]"
                      "[']" t
                      ("-C" "-d" "german")
                      "~latin1" iso-8859-15))
       ;; ===================================================================
       ;; ISPELL End
       ;; ===================================================================
       ;; ===================================================================
       ;; FLYSPELL
       ;; ===================================================================
       (add-hook 'flyspell-mode-hook
                 (function (lambda ()
                             (setq ispell-local-dictionary "de_DE")
                             )))
       ;;(add-hook 'flyspell-mode-hook
       ;;          (function (lambda ()
       ;;                      (setq ispell-local-dictionary "english")
       ;;                      )))
       
       (autoload 'flyspell-mode "flyspell"  
         "On-the-fly spelling checking" t)
       (autoload 'global-flyspell-mode "flyspell"
         "On-the-fly spelling" t)
       (add-hook 'htm-mode-hook 'flyspell-mode)
       (add-hook 'html-mode-hook 'flyspell-mode)
       (add-hook 'latex-mode-hook 'flyspell-mode)
       (add-hook 'tex-mode-hook 'flyspell-mode)
       (add-hook 'text-mode-hook 'flyspell-mode)
       (add-hook 'post-mode-hook 'flyspell-mode)
       (add-hook 'message-mode-hook 'flyspell-mode)
       ;; ===================================================================
       ;; FLYSPELL End
       ;; ===================================================================

