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

(defun trim-string (string)
(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun rewrite-python-font-lock-extend-region ()
 "If last char is a closing paren, then scroll back until mated"
 (condition-case nil
   (save-excursion
    (goto-char font-lock-end)
    (backward-list)
    (beginning-of-line)
    (when (< (point) font-lock-beg) ;;  (and 
     ;; (< (- font-lock-end (point)) 1000)
     ;; (< (point) font-lock-beg))
     (setq font-lock-beg (point))))
 (error nil)))


(defun rewrite-python-activate ()
 (rewrite-python-substitutions 'font-lock-add-keywords)
 (set (make-local-variable 'font-lock-extra-managed-props) '(display))
 (set (make-local-variable 'font-lock-multiline) t)
 (add-hook 'font-lock-extend-region-functions 'rewrite-python-font-lock-extend-region nil 'local)
 (font-lock-fontify-buffer))

(defun rewrite-python-deactivate ()
 (rewrite-python-substitutions 'font-lock-remove-keywords)
 (facemenu-remove-all (point-min) (point-max))
 (remove-hook 'font-lock-extend-region-functions 'rewrite-python-font-lock-extend-region 'local))

;; compose region version

;; (font-lock-add-keywords nil
;; 			 `(("\\(^\\|[a-zA-Z0-9]\\)\\(_to_\\)"
;; 			    (0 (progn (compose-region (match-beginning 2) 
;;                                           (match-end 2)
;;                                           ,?-)
;; 				      nil)))))


(defun rewrite-python-function-match (name)
 `(lambda (last)
   (cond ((re-search-forward 
           (concat "\\(^\\|[^[:alnum:]]\\)\\(" ,name "\\)")
           last t)
          (let* ((b1 (match-beginning 2))
                 (e1 (match-end 2))
                 (closing (or 
                           (condition-case nil
                             (scan-lists (- e1 1) 1 0)
                            (error nil))  ; nil if imbalanced
                           (buffer-end 1))))
           (cond ((and (<= closing last)  ; hasn't gone past last
                       ;; (<= closing (line-end-position 1)) ; limit to single line
                       (= (char-before e1)  ; is the same char
                          (or ; handle when its not in assoc list
                           (cadr (assq (char-before closing) '((?\) ?\()
                                                               (?\] ?\[)
                                                               (?\} ?\{)
                                                               (?\> ?\<))))
                           -1)))          ;never match char with -1
                  (set-match-data (list b1 closing b1 e1 (- closing 1) closing))
                  (backward-char)
                  t)
                 (t nil))))
         (t nil))))

(defun rewrite-python-substitutions (f)
 "TODO: borrow pretty-mode.el's pretty-keywords app"
 (funcall 
  f
  nil
  `(("\\(^\\|[[:alnum:]]\\)\\(_to_\\)"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "→"))
               nil)))
    ("\\(\\.astype\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 1)
                (match-end 1) 'display (concat "→"))
               nil)))
    ("[^[:alnum:]]\\([[:alnum:]]*\\)\\(\\.sum\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 1)
                (match-end 2) 'display (concat "Σ"))
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(\\array\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2)
                (match-end 2) 'display (concat "⩩"))
               nil)))
    ("[^-!<>=+*/^]\\(=\\)[^-!=<>+*/^]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "←"))
               nil)))
    ("[^-!<>=+*/^]\\(>=\\)[^-!=<>+*/^]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "≥"))
               nil)))
    ("[^-!<>=+*/^]\\(<=\\)[^-!=<>+*/^]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "≤"))
               nil)))
    ("[^-!<>=+*/^]\\(!=\\)[^-!=<>+*/^]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "≠"))
               nil)))
    ("[^-!<>=+*/^]\\(==\\)[^=]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "≡"))
               nil)))
    ;; ("\\([[:alnum:]]\\|[[:space:]]\\)\\(==\\)\\([[:alnum:]]\\|[[:space:]]\\)"
    ;;  (0 (progn (put-text-property 
    ;;             (match-beginning 2) 
    ;;             (match-end 2) 'display (concat "≡"))
    ;;            nil)))
    ("\\(^\\|[^[:alnum:]]\\)\\(for\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "∀"))
               nil)))
    ("\\(^\\|[^[:alnum:]]\\)\\(if\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "∃"))
               nil)))
    ("\\(^\\|[[:space:]]\\|\(\\)\\(not\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "¬"))
               nil)))
    ("\\(^\\|[[:space:]]\\|\(\\)\\(or\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "∨"))
               nil)))
    ("\\(^\\|[[:space:]]\\|\(\\)\\(and\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "∧"))
               nil)))
    ("\\(^\\|[^[:alnum:]]\\)\\(True\\)[^a-zA-Z0-9]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "⊤"))
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(False\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "⊥"))
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(None\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "∅"))
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(self\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "◯"))
               nil)))
    ("\\(^\\|[^a-zA-Z0-9]\\)\\(import\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "≺")) ;just silly at this point
               nil)))
    (")\\(:\\)$"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "▶"))
               nil)))
    ("\\(\\\\\\)$"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "…"))
               nil)))
    ("^[[:space:]]*\\(def\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "§"))
               nil)))
    ("^[[:space:]]*\\(class\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "⌘"))
               nil)))
    ("^[[:space:]]*\\(return\\)\\([^[:alnum:]]\\|$\\)"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "◀"))
               nil)))
    ("[[:alnum:]]\\(_\\)[[:alnum:]]"    ; my all time most hated wart in pyton
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "-"))
               nil)))
    ("[^[:alnum:]]\\(_\\)[[:alnum:]]*\\(_\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "⟨"))
               (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "⟩"))
               nil)))
    ("[^[:alnum:]]\\(__\\)[[:alnum:]]*\\(__\\)[^[:alnum:]]"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "⟪"))
               (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "⟫"))
               nil)))
    (,(rewrite-python-function-match "abs[[:space:]]*(\\|fabs[[:space:]]*(\\|len[[:space:]]*(") ;abs-match
     (0 (progn 
         (put-text-property 
          (match-beginning 1) 
          (match-end 1) 'display (concat "|"))
         (put-text-property 
          (match-beginning 2) 
          (match-end 2) 'display (concat "|"))
         (put-text-property 
          (match-beginning 0) 
          (match-end 0) 'font-lock-multiline t)
         nil)))
    (,(rewrite-python-function-match "floor[[:space:]]*(")
     (0 (progn 
         (put-text-property 
          (match-beginning 1) 
          (match-end 1) 'display (concat "⌊"))
         (put-text-property 
          (match-beginning 2) 
          (match-end 2) 'display (concat "⌋"))
         (put-text-property 
          (match-beginning 0) 
          (match-end 0) 'font-lock-multiline t)
         nil)))
    (,(rewrite-python-function-match "ceil[[:space:]]*(")
     (0 (progn 
         (put-text-property 
          (match-beginning 1) 
          (match-end 1) 'display (concat "⌈"))
         (put-text-property 
          (match-beginning 2) 
          (match-end 2) 'display (concat "⌉"))
         (put-text-property 
          (match-beginning 0) 
          (match-end 0) 'font-lock-multiline t)
         nil)))
))
 ;; * as ×
 ;; ^ as ⊕
 ;; / as ÷
 nil)

(provide 'rewrite-python-mode)
