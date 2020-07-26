;; Emacs starter configuration
;; Ben Maughan http://www.pragmaticemacs.com
;; $Id: bjm-starter-init.el,v 1.7 2016/11/13 20:38:05 bjm Exp bjm $

;; look for the string CUSTOMIZE for places where you might want to
;; make changes for yourself

(defconst bjm-init-version "$Id: bjm-starter-init.el,v 1.7 2016/11/13 20:38:05 bjm Exp bjm $")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; You shouldn't need to change anything in this block
;;
;; Prepare Emacs package handling. This is actually only needed to get
;; `use-package' in place, so we don't have to think about this ever again.
(require 'package)
(setq package-enable-at-startup nil)
;;enable melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

;; Bootstrap `use-package', the one and only package that we want to install
;; manually. It will do automagic installation, delayed loading and things
;; like that for us.
;; https://www.reddit.com/r/emacs/comments/3n7fs2/what_is_the_most_conventional_way_for_writing/
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


;; open my init file
(defun bjm/open-my-init-file ()
  "Open the user's init.el file."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el"))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save, backups and session                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto save often
;; save every 20 characters typed (this is the minimum)
(setq auto-save-interval 20)

;; backup settings
;; https://www.emacswiki.org/emacs/BackupFiles
(setq
 backup-by-copying t     ; don't clobber symlinks
 kept-new-versions 10    ; keep 10 latest versions
 kept-old-versions 0     ; don't bother with old versions
 delete-old-versions t   ; don't ask about deleting old versions
 version-control t       ; number backups
 vc-make-backup-files t) ; backup version controlled files

;; backup every save
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; https://www.emacswiki.org/emacs/backup-each-save.el
(defvar bjm/backup-file-size-limit (* 5 1024 1024)
  "Maximum size of a file (in bytes) that should be copied at each savepoint.

  If a file is greater than this size, don't make a backup of it.
  Default is 5 MB")

;; CUSTOMISE - put your own directory here for backups
(defvar bjm/backup-location (expand-file-name "~/emacs-backups")
  "Base directory for backup files.")

(defvar bjm/backup-trash-dir (expand-file-name "~/.Trash")
  "Directory for unwanted backups.")

(defvar bjm/backup-exclude-regexp nil
  "Don't back up files matching this regexp.

  Files whose full name matches this regexp are backed up to `bjm/backup-trash-dir'. Set to nil to disable this.")

;; Default and per-save backups go here:
;; N.B. backtick and comma allow evaluation of expression
;; when forming list
(setq backup-directory-alist
      `(("" . ,(expand-file-name "per-save" bjm/backup-location))))

;; add trash dir if needed
(if bjm/backup-exclude-regexp
    (add-to-list 'backup-directory-alist `(,bjm/backup-exclude-regexp . ,bjm/backup-trash-dir)))

(defun bjm/backup-every-save ()
  "Backup files every time they are saved.

  Files are backed up to `bjm/backup-location' in subdirectories \"per-session\" once per Emacs session, and \"per save\" every time a file is saved.

  Files whose names match the REGEXP in `bjm/backup-exclude-regexp' are copied to `bjm/backup-trash-dir' instead of the normal backup directory.

  Files larger than `bjm/backup-file-size-limit' are not backed up."

  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;;
    ;; Override the default parameters for per-session backups.
    ;;
    (let ((backup-directory-alist
           `(("." . ,(expand-file-name "per-session" bjm/backup-location))))
          (kept-new-versions 3))
      ;;
      ;; add trash dir if needed
      ;;
      (if bjm/backup-exclude-regexp
          (add-to-list
           'backup-directory-alist
           `(,bjm/backup-exclude-regexp . ,bjm/backup-trash-dir)))
      ;;
      ;; is file too large?
      ;;
      (if (<= (buffer-size) bjm/backup-file-size-limit)
          (progn
            (message "Made per session backup of %s" (buffer-name))
            (backup-buffer))
        (message "WARNING: File %s too large to backup - increase value of bjm/backup-file-size-limit" (buffer-name)))))
  ;;
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  ;;
  (let ((buffer-backed-up nil))
    ;;
    ;; is file too large?
    ;;
    (if (<= (buffer-size) bjm/backup-file-size-limit)
        (progn
          (message "Made per save backup of %s" (buffer-name))
          (backup-buffer))
      (message "WARNING: File %s too large to backup - increase value of bjm/backup-file-size-limit" (buffer-name)))))

;; add to save hook
(add-hook 'before-save-hook 'bjm/backup-every-save)


;; auto revert mode - revert buffer if file changes on disk
(global-auto-revert-mode 1)

;; save various settings between sessions
;; lighter version of desktop
(use-package session
  :init
  (add-hook 'after-init-hook 'session-initialize))

;; save recent files list
(use-package recentf
  :config
  (progn
    ;; save every 60 minutes
    (run-at-time nil (* 60 60) 'recentf-save-list)
    (setq recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
      recentf-exclude '("/ssh:"))
    (recentf-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance options                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CUSTOMISE
;;
;; uncomment one of these to choose a nicer theme
;;
;; a nice dark theme
;; (use-package labburn-theme
;;   :config
;;   (mapcar #'disable-theme custom-enabled-themes)
;;   (load-theme 'labburn t))

;; (use-package solarized-theme
  ;; :ensure t
  ;; :config
  ;; (mapcar #'disable-theme custom-enabled-themes)
  ;; (load-theme 'solarized-light t))


;; CUSTOMISE
;;
;; uncomment to set some custom faces that work well with labburn theme
;; (custom-set-faces
 ;; '(default ((t (:foreground "#dadaca" :background "#3f3f3f"))))
 ;; '(flyspell-incorrect ((t (:background "selectedKnobColor" :underline (:color "red" :style wave) :weight bold))))
 ;; '(hl-line ((t (:background "grey12"))))
 ;; '(ivy-current-match ((t (:foreground "yellow" :background "#3B99FC" :weight bold))))
 ;; '(region ((t (:background "#363983"))))
 ;; '(sp-show-pair-match-face ((t (:foreground "green3" :background "gray40"))))
 ;; '(sp-show-pair-mismatch-face ((t (:foreground "red"))))
 ;; '(swiper-line-face ((t (:background "#3B99FC"))))
 ;; '(swiper-match-face-2 ((t (:foreground "yellow" :background "#3B99FC" :weight bold)))))

;; no startup message
(setq inhibit-startup-message t)

;;echo command keys more quickly
(setq echo-keystrokes 0.5)

;; highlight current line
(global-hl-line-mode +1)

;; diminish minor modes from mode line to save space
(use-package diminish
  :ensure t
  :demand t
  :diminish abbrev-mode
  :diminish auto-fill-function)

;; visualise whitespace
(use-package whitespace
  :diminish whitespace-mode)

;;turn off highlight long lines
(setq whitespace-line-column 10000)

;; volatile highlights - highlight changes from pasting etc
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; meaningful names for buffers with the same name
;; from prelude
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; from prelude
(setq frame-title-format
      '("" invocation-name " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; rainbow-mode - colourise colours in the buffer
(use-package rainbow-mode)

;; rainbow-delimiters - show matching brackets etc
(use-package rainbow-delimiters)

;; show page breaks
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode 1)
  (setq page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode org-mode ess-mode latex-mode)))

;; scroll buffer if cursor is this many lines from the top or bottom
(setq scroll-margin 3)

;; restore window configurations
(winner-mode t)

;; multiline emphasis
(require 'org)
(setcar (nthcdr 4 org-emphasis-regexp-components) 1000)
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line wrapping                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; fill or unfill paragraph
;; from endless parentheses
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; related to unfill - join following lines
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; searching and movement                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swiper for improved searching
(use-package swiper
  :pin melpa-stable
  :ensure t
  :bind (("C-s" . swiper)))

;; avy to jump to a word
(use-package avy
  :pin melpa-stable
  :ensure t
  :bind (("M-s" . avy-goto-word-1))
  :config
  (setq avy-style 'at-full))

;; scrollers
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v "))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; mark ring navigation
(setq set-mark-command-repeat-pop t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; improved text expansion with M-/
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; abbreviations
(setq-default abbrev-mode t)     ;; enable abbreviations
(setq save-abbrevs 'silently)    ;; save abbreviations upon exiting emacs
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))  ;; reads the abbreviations file on startup

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; delete the selection with a keypress
(delete-selection-mode t)

;; copy with mouse
(setq mouse-drag-copy-region t)

;; Save whatever’s in the current (system) clipboard before replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::
;;;;;;;;;;;; BEN: I DON'T NEED THIS, because it prevents my preferred key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cua-mode for rectangles
;;(use-package cua-base
;;  :init (progn
;;          (cua-mode 1)
;;          (cua-selection-mode t)                  ; Use rectangle mode,
;;          )
;;  :config
;;  (progn
;;    (setq cua-enable-cua-keys nil) ;; only for rectangles
;;    (setq cua-auto-tabify-rectangles nil)   ; Don't tabify after rectangle commands
;;    ))

;; anzu for nicer query replace
(use-package anzu
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("M-." . mc/mark-next-like-this)
         ("M-," . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-'" . mc-hide-unmatched-lines)))

;; expand region with a key
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; undo tree mode - powerful undo/redo visualisation
;;(use-package undo-tree
;;  :ensure t
;;  :bind (("C-z" . undo-tree-undo)
;;         ("C-S-z" . undo-tree-redo)
;;         ("C-x u" . undo-tree-visualize)))
;;  :init
;;  (progn
;;    (global-undo-tree-mode)
;;    (defalias 'redo 'undo-tree-redo)
;;    (defalias 'undo 'undo-tree-undo)
;;    )

;; yasnippet to insert text templates
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1)
  (setq yas-indent-line nil))

;; shrink whitespace, cycling through amount
(use-package shrink-whitespace
  :ensure t
  :bind (("M-SPC" . shrink-whitespace)))

;; clean whitespace upon saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; copy line if no region selected
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)))

;; add the ability to cut the current line, without marking it
;; from prelude
;; note - this should be after volatile-highlights is required
(require 'rect)
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end) rectangle-mark-mode)
     (list (line-beginning-position)
           (line-beginning-position 2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commenting tools                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full width comment box
(defun bjm-comment-box (b e)
  "Draw a box comment around the region but arrange for the region to extend to at least the fill column. Place the point after the comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

;; modified comment region to also comment current line
;; http://endlessparentheses.com/implementing-comment-line.html
(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "C-M-;") 'endless/comment-line-or-region)

;; comment lines easily
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :diminish (flyspell-mode . "spell")
  :config
  (set-face-attribute 'flyspell-incorrect nil :background
"selectedKnobColor" :underline '(:color "red") :weight 'bold)
  )

;; turn on flyspell in desired modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq ispell-dictionary "british")
(setq ispell-check-comments t)
(setq ispell-really-hunspell t)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary-alist
      `(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil
utf-8)))

;; CUSTOMISE - hunspell
;;
;; comment the two lines above and uncomment the lines below to set up
;; hunspell on windows. You'll need to edit the path to the hunspell
;; directory in both of the following lines - i.e. replace "Users/ben"
;; and "Users\\ben" with the path to your hunspell directory
;;
;; (setq ispell-program-name "C:/Users/ben/hunspell/bin/hunspell.exe")
;; (setq ispell-local-dictionary-alist
;;       `(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB"
;; "-p" "C:\\Users\\ben\\hunspell\\share\\hunspell\\en_GB") nil
;; utf-8)))


;; change case of letters
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    )
  )
;;set this to M-c
(global-set-key "\M-c" 'toggle-letter-case)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired - file management                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq dired-listing-switches "-alh")

;; dired-x for dired-jump and hiding some files
(require 'dired-x)

;; dired plus for some extra features
;;(use-package dired+
;;  :ensure t
;;  :bind (:map dired-mode-map
;;              ("C-S-o" . diredp-find-file-other-frame)
;;              ("C-o" . bjm/toggle-multiframe-window)))

;; narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; copy/paste files in dired
(use-package dired-ranger
  :ensure t
  :defer t ; don't access `dired-mode-map' until `dired-ranger' is loaded
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))


;; insert file name at point
;; https://www.emacswiki.org/emacs/InsertFileName
(defun bjm/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (expand-file-name filename)))
        ((not (null args))
         (insert filename))
        (t
         (insert (file-relative-name filename)))))

;; reveal file in finder
(use-package reveal-in-osx-finder)

;; spotlight search on mac
(use-package spotlight)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff to compare files                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
(setq ediff-version-control-package 'rcs)

;; Make org files behave in ediff
;; unfold the entire org file within ediff
;; credit: https://github.com/cmcmahan/elisp/blob/master/emacs-org.el
(add-hook 'ediff-prepare-buffer-hook 'f-ediff-prepare-buffer-hook-setup)
(defun f-ediff-prepare-buffer-hook-setup ()
  ;; specific modes
  (cond ((eq major-mode 'org-mode)
         (f-org-vis-mod-maximum))
        ;; room for more modes
        ))
(defun f-org-vis-mod-maximum ()
  "Visibility: Show the most possible."
  (cond
   ((eq major-mode 'org-mode)
    (visible-mode 1)) ; default 0
   (t
    (message "ERR: not in Org mode")
    (ding))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commenting tools                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full width comment box
(defun bjm-comment-box (b e)
  "Draw a box comment around the region but arrange for the region to extend to at least the fill column. Place the point after the comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

;; modified comment region to also comment current line
;; http://endlessparentheses.com/implementing-comment-line.html
(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "C-M-;") 'endless/comment-line-or-region)

;; comment lines easily
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spelling                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :diminish (flyspell-mode . "spell")
  :config
  (set-face-attribute 'flyspell-incorrect nil :background
"selectedKnobColor" :underline '(:color "red") :weight 'bold)
  )

;; turn on flyspell in desired modes
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq ispell-dictionary "british")
(setq ispell-check-comments t)
(setq ispell-really-hunspell t)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary-alist
      `(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil
utf-8)))

;; CUSTOMISE - hunspell
;;
;; comment the two lines above and uncomment the lines below to set up
;; hunspell on windows. You'll need to edit the path to the hunspell
;; directory in both of the following lines - i.e. replace "Users/ben"
;; and "Users\\ben" with the path to your hunspell directory
;;
;; (setq ispell-program-name "C:/Users/ben/hunspell/bin/hunspell.exe")
;; (setq ispell-local-dictionary-alist
;;       `(("british" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB"
;; "-p" "C:\\Users\\ben\\hunspell\\share\\hunspell\\en_GB") nil
;; utf-8)))


;; change case of letters
;; http://ergoemacs.org/emacs/modernization_upcase-word.html
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    )
  )
;;set this to M-c
(global-set-key "\M-c" 'toggle-letter-case)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion packages                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy
(use-package ivy
  :pin melpa-stable
  :demand
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t))

;; counsel - some extra functions built on ivy
(use-package counsel
  :pin melpa-stable
  :bind
  (("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :init
  ;; guess file name from text at point
  (setq counsel-find-file-at-point t))

;; helm
(use-package helm
  :diminish helm-mode
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t))
    ;;replace locate with spotlight - uncomment next 2 lines on Mac
    ;;(setq locate-command "mdfind -name")
    ;;(setq helm-locate-command "mdfind -name %s %s")
  :bind (("C-x f" . helm-for-files)
         ("M-x" . helm-M-x)))

;; flx for fuzzy matching
(use-package flx)
(use-package helm-flx)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc packages and functions                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; crux for misc useful functions
(use-package crux
  :bind (("C-c o" . crux-open-with)
         ("C-a" . crux-move-beginning-of-line)))

;; which-key gives help on partially completed key combos
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; insert date
;; from http://emacswiki.org/emacs/InsertingTodaysDate
(defun bjm-insert-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))

;; sentences end with single space
(setq sentence-end-double-space nil)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)


;; global keys for exiting
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x w") 'delete-frame)


;; need to set these keys at the end so that org-mode doesn't steal them
;; comment box
(global-set-key (kbd "C-c b b") 'bjm-comment-box)

;; insert file name
(global-set-key (kbd "C-c b i") 'bjm/insert-file-name)

;;insert date
(global-set-key (kbd "C-c b d") 'bjm-insert-date)

;; calculator
(global-set-key (kbd "C-c b c") 'quick-calc)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Dropbox/benjamin/schreiben/2018-Stalker/stalker.org")))
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (org-plus-contrib which-key crux ess auctex htmlize helm-flx flx helm spotlight reveal-in-osx-finder dired-ranger peep-dired dired-narrow dired+ comment-dwim-2 easy-kill shrink-whitespace yasnippet volatile-highlights use-package swiper session rainbow-mode rainbow-delimiters page-break-lines multiple-cursors expand-region diminish avy anzu)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; LOAD ALL OTHER STUFF ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set load-path for addons
(add-to-list 'load-path "~/.emacs.d/addons")

;; set load-path for customizations
(add-to-list 'load-path "~/.emacs.d/customizations")

;; use org-tree-view
(load "org-tree-view.el")

;; use org-bullets for better bullets
(load "org-bullets.el")

;; use my own writing stuff
(load "org-wc.el")
(load "writingstuff.el")
(load "agendastuff.el")

;; load navigation-enhancements from scrivener-vim-guy
(load "navigation.el")
(load "themes.el")
(load "usefulstuff.el")

;; load sentence-length-highlighting from Arne
(load "highlight-sentence-length.el")

;; fallback for commands if hyper is locked via numlock
(defun numlockwarning ()
  (interactive)
  (message "numlock?"))
(global-set-key (kbd "H-M-x") 'numlockwarning)

;; use a NORMAL line mode
(global-visual-line-mode 1)


(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
