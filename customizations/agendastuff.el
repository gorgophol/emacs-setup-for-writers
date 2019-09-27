;; Use org-agenda to create a Timeline for all headings with TODO "TIME" and sorted by timestamp
    (global-set-key (kbd "C-c a") `org-agenda)
    (setq org-agenda-custom-commands
   '(("L" "Timeline"
      ((todo
        "TIME"
        ((org-agenda-overriding-header "=== TIMELINE ===")
         (org-agenda-sorting-strategy '(timestamp-up))
    ))))))


;; SAVE THIS FOR LATER, when other agendas are needed
;; make agenda view for the last 30 years possbile, hide empty dates 
;;(setq org-agenda-start-day "-30y")
;;(setq org-agenda-span 12000)
;;(setq org-agenda-show-all-dates nil)
;;(setq org-agenda-sorting-strategy '(timestamp-up))

;;         (org-agenda-prefix-format '((agenda . " %1c %?-12t% s")))


