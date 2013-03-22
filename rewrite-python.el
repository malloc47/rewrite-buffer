;;; rewrite-python.el

(defvar rewrite-python-mode-hook nil)

;; minor mode definitions
(define-minor-mode rewrite-python-mode
 "Rewrite python display output"
 nil
 " rp"
 nil
 :group 'rewrite
 (if rewrite-python-mode 
   (rewrite-python-activate)
  (rewrite-python-deactivate)))

;; needed only if we're using put-text-property instead of
;; compose-region
(defun rewrite-pyton-advice () 
 (defadvice delete-char (around delete-char-display activate)
  (if (and (= (ad-get-arg 0) 1)
           (get-text-property (point) 'display))
    (progn
     (remove-text-properties (point) (next-property-change (point)) '(display nil))
     ad-do-it)
   ad-do-it))
 (defadvice delete-backward-char (around delete-backward-char-display activate)
  (if (and (= (ad-get-arg 0) 1) (> (point) (point-min))
           (get-text-property (- (point) 1) 'display))
    (progn
     (remove-text-properties (point)  
                             (previous-property-change (point)) '(display nil))
     ad-do-it)
   ad-do-it)))

(defun rewrite-python-activate () 
 (rewrite-pyton-advice)
 (rewrite-python-substitutions 'font-lock-add-keywords)
 (font-lock-fontify-buffer))

(defun rewrite-python-deactivate ()
 (ad-deactivate 'delete-char)
 (ad-deactivate 'delete-backward-char)
 (rewrite-python-substitutions 'font-lock-remove-keywords)
 (facemenu-remove-all (point-min) (point-max)))

;; compose region version

;; (font-lock-add-keywords nil
;; 			 `(("\\(^\\|[a-zA-Z0-9]\\)\\(_to_\\)"
;; 			    (0 (progn (compose-region (match-beginning 2) 
;;                                           (match-end 2)
;;                                           ,?-)
;; 				      nil)))))

(defun rewrite-python-substitutions (f)
 (funcall 
  f
  nil
  `(("\\(^\\|[[:alnum:]]\\)\\(_to_\\)"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "→")
               nil)))
    ("\\(\\.astype\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 1)
                (match-end 1) 'display "→")
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(\\array\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2)
                (match-end 2) 'display "⩩")
               nil)))
    ("\\(^\\|[[:space:]]\\)\\(in\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "∈")
               nil)))
    ("\\(^\\|[^[:alnum:]]\\)\\(for\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "∀")
               nil)))
    ("\\(^\\|[^[:alnum:]]\\)\\(if\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "∃")
               nil)))
    ("\\(^\\|[[:space:]]\\|\(\\)\\(not\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "¬")
               nil)))
    ("\\(^\\|[[:space:]]\\|\(\\)\\(or\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "∨")
               nil)))
    ("\\(^\\|[[:space:]]\\|\(\\)\\(and\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "∧")
               nil)))
    ("\\(^\\|[^[:alnum:]]\\)\\(True\\)[^a-zA-Z0-9]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "⊤")
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(False\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "⊥")
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(None\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "∅")
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(self\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "◯")
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(import\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "≺") ;just silly at this point
               nil)))
    (")\\(:\\)$"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display "▶")
               nil)))
    ("\\(\\\\\\)$"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display "…")
               nil)))
    ("^[[:space:]]*\\(def\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display "§")
               nil)))
    ("^[[:space:]]*\\(return\\)\\([^[:alnum:]]\\|$\\)"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display "◀")
               nil)))
    ("\\([[:alnum:]]\\|[[:space:]]\\)\\(==\\)\\([[:alnum:]]\\|[[:space:]]\\)"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display "≡")
               nil)))))
 ;; * as ×
 ;; ^ as ⊕
 ;; / as ÷
 nil)

(provide 'rewrite-python-mode)
