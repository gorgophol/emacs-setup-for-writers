;;;;;;;;;;;;;;;;;;;;
;;; Minor mode definition
;;;;;;;;;;;;;;;;;;;;
(defvar org-tree-view-mode-map (make-sparse-keymap))

(define-key org-tree-view-mode-map (kbd "<return>")  #'org-tree-view/open-headline-at-point)
(define-key org-tree-view-mode-map (kbd "<mouse-1>") #'org-tree-view/open-headline-at-point)
(define-key org-tree-view-mode-map (kbd "C-g")       #'org-tree-view/close)
(define-key org-tree-view-mode-map (kbd "<tab>")     #'org-tree-view/toggle-visibility)
(define-key org-tree-view-mode-map [remap end-of-buffer]
  (lambda () (interactive) (goto-char (point-max)) (move-beginning-of-line 1)))

;; Re-define self-insert-command
(map-keymap (lambda (key fun)
              (if (equal fun #'self-insert-command)
                  ;; Append ((from . to) #'org-tree-view/self-insert-command))
                  (nconc org-tree-view-mode-map
                        (list (cons key #'org-tree-view/self-insert-command)))))
            (current-global-map))
(setq org-tree-view-mode-map ; develop (from . to) ranges, please
      (keymap-canonicalize org-tree-view-mode-map))

(define-minor-mode org-tree-view-mode
  ""
  nil
  :lighter " tree-view"
  org-tree-view-mode-map)

(provide 'org-tree-view-mode)


;;;;;;;;;;;;;;;;;;;;
;;; Rendering the tree view
;;;;;;;;;;;;;;;;;;;;
(require 'subr-x)

(defun org-tree-view/get-headlines (&optional level &optional base-buffer)
  "Get a list of all headlines in `base-buffer' of a level less
than or equal to provided `level'. Includes headlines outside of
any potential narrowing."
  (let ((level (or level org-tree-view/level))
        (base-buffer
         (or base-buffer
             (if (org-tree-view/is-tree-view)
                 (get-buffer (org-tree-view/make-base-buffer-name))
               (current-buffer))))
        (headlines))
    (with-current-buffer base-buffer
      (let ((widened-buffer
             (clone-indirect-buffer
              (concat "<widened>" (buffer-name base-buffer)) nil)))
        (with-current-buffer widened-buffer
          (widen)
          (setq headlines (org-element-map (org-element-parse-buffer 'headline) 'headline
                            (lambda (headline)
                              (when (<= (org-element-property :level headline) level)
                                headline)))))
        (kill-buffer widened-buffer)))
    headlines))

(defun org-tree-view/draw-headline (headline)
  "Return a string of the headline to be printed, with the proper
face and its position in the base buffer encoded as the
`org-tree-view-headline-pos' text property."
  (let* ((title (org-element-property :raw-value headline))
         (level (org-element-property :level headline))
         (begin (org-element-property :begin headline))
         (end (org-element-property :end headline))
         (org-tree-view-level-face (intern
                                    (concat "org-tree-view/level-"
                                            (number-to-string level))))
         (text (concat (apply #'concat (make-list (* 2 (1- level)) " "))
                       "* "
                       (replace-regexp-in-string "\"" "" title nil t)))) ; remove \" from title

    ;; Text properties
    (put-text-property ; org-tree-view-level-N
     0 (length text)
     'font-lock-face org-tree-view-level-face
     text)
    (put-text-property ; encode headling position in base buffer
     0 (length text)
     'org-tree-view-headline-pos begin
     text)
    (setq text (org-tree-view/draw-string text))

    ;; Pad headline with spaces
    (let ((end (1- (length text))))
      (setq text (concat text
                         (apply #'concat (make-list (* 2 org-tree-view/width) " "))))
      ;; Add same properties to padding (a little repetitive)
      (put-text-property
       end (length text)
       'font-lock-face org-tree-view-level-face
       text)
      (put-text-property
       end (length text)
       'org-tree-view-headline-pos begin
       text))
    text))

(defun org-tree-view/draw-string (text)
  "Apply the appropriate faces on `text' according to Org markup
syntax and return the resulting string."
  (let* ((types '(link
                  italic
                  bold
                  strike-through
                  verbatim
                  code))
         (data (org-element-parse-secondary-string text types)))
    (org-element-map data types
      (lambda (object)
        (let* ((type (org-element-type object))
               (begin (1- (org-element-property :begin object)))
               (end (1- (org-element-property :end object)))
               (faces
                '(italic         italic
                  bold           bold
                  strike-through (:strike-through t)
                  verbatim       org-verbatim
                  code           org-code)))

          ;; Figure out real end of object
          (let ((substr (string-trim-right (substring text begin end))))
            (setq end (+ begin (length substr))))

          ;; Handle faces
          (when (member type faces)
            (let* ((face (plist-get faces type))
                   (existing-face (get-text-property begin 'font-lock-face text))
                   (new-face (if existing-face
                                 `(,face ,existing-face)
                               face)))
              (put-text-property ; add face for type
               begin end
               'font-lock-face new-face
               text))
            (put-text-property ; remove first piece of markup
             begin (1+ begin)
             'display ""
             text)
            (put-text-property ; remove second piece of markup
             (1- end) end
             'display ""
             text))

          ;; Handle links
          (when (equal 'link type)
            (let ((contents-begin (org-element-property :contents-begin object)))
              (if contents-begin ; has contents
                  (put-text-property
                   begin (1- contents-begin)
                   'display ""
                   text)
                (put-text-property ; only url
                 begin (+ begin 2)
                 'display ""
                 text))
              (put-text-property
               (- end 2) end
               'display ""
               text))))))
    text))

(defun org-tree-view/insert-headlines ()
  "Insert all drawn headlines at the current position."
  (let ((headlines (org-tree-view/get-headlines)))
    (cl-loop for headline in headlines
       do (insert (concat
                   (org-tree-view/draw-headline headline)
                   "\n")))
    ;; Delete final newline
    (backward-delete-char 1)))

(defun org-tree-view/refresh (&optional no-set-window-start)
  (let* ((orig-window-start (window-start))
         (orig-window-line  (+ (count-lines (window-start) (point))
                               (if (= (current-column) 0) 1 0)
                               -1))
         ;; ^ see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_478.html
         (refresh `(progn
                     (setq-local buffer-read-only nil)
                     (set-window-fringes (get-buffer-window) 8 1)
                     (erase-buffer)
                     (org-tree-view/insert-headlines)
                     (set-window-start (get-buffer-window) orig-window-start)
                     (goto-char (window-start))
                     (forward-line orig-window-line)
                     (move-beginning-of-line 1)
                     (setq-local buffer-read-only t))))
    (if (org-tree-view/is-tree-view)
        (eval (macroexpand refresh))
      (if (org-tree-view/has-tree-view)
          (with-current-buffer (org-tree-view/make-tree-view-buffer-name)
            (eval (macroexpand refresh)))))))


;;;;;;;;;;;;;;;;;;;;
;;; Interacting with the tree view
;;;;;;;;;;;;;;;;;;;;
(defun org-tree-view/open (&optional arg)
  ""
  (interactive "p")
  (let ((tree-view-buffer-name (org-tree-view/make-tree-view-buffer-name))
        (base-buffer (current-buffer))
        (tree-view-buffer))

    (if (and (org-tree-view/has-tree-view)
             (get-buffer-window tree-view-buffer-name))
        (progn
          (select-window (get-buffer-window tree-view-buffer-name))
          (setq tree-view-buffer (get-buffer tree-view-buffer-name)))
      (if (org-tree-view/has-tree-view)
          (kill-buffer (org-tree-view/make-tree-view-buffer-name)))
      (if (equal org-tree-view/side 'left)
          (split-window-right org-tree-view/width)
        (split-window-right (* -1 org-tree-view/width))
        (other-window 1))
      (setq tree-view-buffer (generate-new-buffer tree-view-buffer-name)))

    ;; Switch to tree view buffer
    (switch-to-buffer tree-view-buffer)
    (org-tree-view/setup)
    (org-tree-view-mode)))

(defun org-tree-view/close (&optional tree-view-buffer &optional base-buffer)
  "Close `tree-view-buffer' for `base-buffer'. Defaults to the current buffer."
  (interactive)
  (let* ((base-buffer
          (get-buffer (or base-buffer
                          (if (org-tree-view/is-tree-view)
                              (org-tree-view/make-base-buffer-name)
                            (current-buffer)))))
         (tree-view-buffer
          (get-buffer (or tree-view-buffer
                          (org-tree-view/make-tree-view-buffer-name base-buffer))))
         (tree-view-window))
    (if tree-view-buffer
        (progn (if (setq tree-view-window (get-buffer-window tree-view-buffer))
                   (delete-window tree-view-window))
               (kill-buffer tree-view-buffer)
               (org-tree-view/cleanup))
      (error "No tree view found!"))))

(defun org-tree-view/bind-close-after (&optional key)
  "Bind the key combination pressed to call
`org-tree-view/open-headline-at-point' to also close the tree
view, but only for 1 second."
  (let* ((key (or key (this-command-keys-vector)))
         (original-binding (local-key-binding key))
         (reset-key `(local-set-key ,key (quote ,original-binding)))
         (base-buffer))
    (if (org-tree-view/is-tree-view)
        (setq base-buffer (org-tree-view/make-base-buffer-name))
      (if (org-tree-view/has-tree-view)
          (setq base-buffer (current-buffer))))
    (when (and base-buffer (not (string-match-p "mouse" (key-description key))))
      (message "Press %s again to close the tree view." (key-description key))
      (eval
       (macroexpand
        `(progn
           (local-set-key ,key (lambda () (interactive)
                                 ,reset-key
                                 (org-tree-view/close)))
           (run-with-timer 1 nil (lambda ()
                                   (message " ") ; clear
                                   (with-current-buffer ,base-buffer
                                     ,reset-key)))))))))

(defun org-tree-view/open-headline-at-point (&optional bind-close-after)
  "From tree view, open headline at point in base buffer. If
`bind-close-after' is non-nil (default: t), the function will
also call `org-tree-view/bind-close-after' before finishing."
  (interactive)
  (let* ((bind-close-after (or bind-close-after t))
         (base-buffer (org-tree-view/make-base-buffer-name))
         (position (get-text-property (point) 'org-tree-view-headline-pos)))
    (condition-case nil
        (select-window (car (get-buffer-window-list base-buffer)))
      (error (other-window 1)
             (switch-to-buffer base-buffer)))
    (if position
        (progn (widen)
               (goto-char position)
               (outline-show-all)
               (org-cycle-hide-drawers 'all)
               (org-narrow-to-subtree)
               (if bind-close-after (org-tree-view/bind-close-after)))
      (error "No headline found!"))))

(defun org-tree-view/switch-to-base-buffer ()
  (interactive)
  (when (org-tree-view/is-tree-view)
    (if (get-buffer-window (org-tree-view/make-base-buffer-name))
        (select-window (get-buffer-window (org-tree-view/make-base-buffer-name)))
      (other-window 1)
      ;; FIXME: the following switch-to-buffer didn't work last I checked
      (switch-to-buffer (org-tree-view/make-base-buffer-name) nil :force-same-window))))

(defun org-tree-view/toggle-visibility (&optional arg)
  (interactive "P")
  (if (numberp arg)
      (setq-local org-tree-view/level arg)
    (if (/= org-tree-view/level (default-value 'org-tree-view/level))
        (setq-local org-tree-view/level (default-value 'org-tree-view/level))
      (setq-local org-tree-view/level 2)))
  (org-tree-view/refresh :no-set-window-start))

;;;;;;;;;;;;;;;;;;;;
;;; Searching the tree view
;;;;;;;;;;;;;;;;;;;;
(defun org-tree-view/self-insert-command (N)
  "Start an `isearch' with the character `N' as the first
character in the search."
  (interactive "p")
  (goto-char (point-min))
  (let* ((char (string-to-char (this-command-keys)))
         (unread-command-events (append unread-command-events (list char))))
    (isearch-forward)))

(defun org-tree-view/isearch-return ()
  "Open the matching headline and exit the isearch."
  (interactive)
  (when (org-tree-view/is-tree-view)
    (org-tree-view/open-headline-at-point)
    (run-with-timer 0 nil (lambda () (org-tree-view/bind-close-after (kbd "<S-return>"))))
    ;; ^ Timer needed because isearch-exit behaves weirdly otherwise.
    (let ((inhibit-message t))
      (isearch-exit))))


;;;;;;;;;;;;;;;;;;;;
;;; Helper functions
;;;;;;;;;;;;;;;;;;;;
(defun org-tree-view/setup ()
  (font-lock-mode)
  (org-tree-view/refresh)
  (setq-local buffer-read-only t)
  (setq-local scroll-margin 0)
  ;; If evil-mode is enabled, enable emacs state:
  (if (and (boundp 'evil-mode) evil-mode) (evil-emacs-state))
  (setq-local case-fold-search t) ; ignore case
  (define-key isearch-mode-map (kbd "<S-return>") #'org-tree-view/isearch-return))

(defun org-tree-view/cleanup ()
  (define-key isearch-mode-map (kbd "<S-return>") nil))

(defun org-tree-view/make-base-buffer-name (&optional tree-view-buffer)
  (let ((tree-view-buffer
         (get-buffer (or tree-view-buffer
                         (current-buffer)))))
    (string-remove-prefix "<tree>" (buffer-name tree-view-buffer))))

(defun org-tree-view/make-tree-view-buffer-name (&optional base-buffer)
  (let ((base-buffer
         (get-buffer (or base-buffer
                         (current-buffer)))))
    (concat "<tree>" (buffer-name base-buffer))))

(defun org-tree-view/is-tree-view (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (bufferp (get-buffer (org-tree-view/make-base-buffer-name buffer))))))

(defun org-tree-view/has-tree-view (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (bufferp (get-buffer (org-tree-view/make-tree-view-buffer-name buffer))))))


