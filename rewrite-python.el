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

(defun rewrite-python-font-lock-extend-region ()
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (setq font-lock-end (line-end-position 1))
  (setq font-lock-beg (line-beginning-position -1))
  t)


(defun rewrite-python-activate ()
 ;; (rewrite-pyton-advice)
 (rewrite-python-substitutions 'font-lock-add-keywords)
 ;; (add-hook 'before-change-functions 'rewrite-python-before-change nil 'local)
 (add-hook 'after-change-functions 'rewrite-python-after-change nil 'local)
 ;; (set (make-local-variable 'font-lock-multiline) t)
 ;; (add-hook 'font-lock-extend-region-functions 'rewrite-python-font-lock-extend-region 'local)
 ;; (set (make-local-variable 'font-lock-extend-after-change-region-function)
 ;;      'rewrite-python-extend-after-change)
 (font-lock-fontify-buffer))

(defun rewrite-python-deactivate ()
 ;; (ad-deactivate 'delete-char)
 ;; (ad-deactivate 'delete-backward-char)
 ;; (remove-hook 'before-change-functions 'rewrite-python-before-change 'local)
 (remove-hook 'after-change-functions 'rewrite-python-after-change 'local)
 ;; (remove-hook 'font-lock-extend-region-functions 'rewrite-python-font-lock-extend-region 'local)
 (rewrite-python-substitutions 'font-lock-remove-keywords)
 (facemenu-remove-all (point-min) (point-max)))

;; compose region version

;; (font-lock-add-keywords nil
;; 			 `(("\\(^\\|[a-zA-Z0-9]\\)\\(_to_\\)"
;; 			    (0 (progn (compose-region (match-beginning 2) 
;;                                           (match-end 2)
;;                                           ,?-)
;; 				      nil)))))


(defun rewrite-python-after-change (begin end length)
 "unset display on characters before and after changed region
  TODO: currently hacked so that it just clears the current line instead of
  intelligently finding the next next point"
 (when (text-property-not-all (line-beginning-position -1) (line-end-position 1) 'display nil)
  (let* ((l (remove-if
             'not
             (list 
              (- begin 2)
              (+ end 2)
              (line-beginning-position -1)
              (line-end-position 1)
              (condition-case nil
                (+ (scan-lists (point) 1 1) 1)   ; ( stuff ) <-
               (error nil))
              (condition-case nil
                (- (scan-lists (scan-lists (point) 1 1) -1 0) 1) ; -> ( stuff )
               (error nil))
              (previous-property-change (max (- begin 2) 1))
              (next-property-change (min (+ end 2) (buffer-end 1)))
              )))
         (b (reduce 'min l))
         (e (reduce 'max l)))
   ;; (message (concat "prev: " 
   ;;                  (number-to-string (condition-case nil
   ;;                                      (scan-lists (point) 1 1)   ; ( stuff ) <-
   ;;                                     (error 0))) 
   ;;                  " next: "
   ;;                  (number-to-string (condition-case nil
   ;;                                      (scan-lists (scan-lists (point) 1 1) -1 0) ; -> ( stuff )
   ;;                                     (error 0)))
   ;;                  ))
   (message (concat "begin: " (number-to-string b) " end: " (number-to-string e)))
   (remove-text-properties b e '(display nil))
   (save-excursion (font-lock-fontify-region b e))
   )
  )
 )

(defun rewrite-python-before-change (begin end)
 "unset display on characters before and after changed region
  TODO: currently hacked so that it just clears the current line instead of
  intelligently finding the next next point"
 (when (text-property-not-all (line-beginning-position -1) (line-end-position 1) 'display nil)
  (let* ((l (remove-if
             'not
             (list 
              (- begin 1)
              (+ end 1)
              (line-beginning-position)
              (line-end-position)
              (condition-case nil
                (scan-lists (point) 1 1)   ; ( stuff ) <-
               (error nil))
              (condition-case nil
                (scan-lists (scan-lists (point) 1 1) -1 0) ; -> ( stuff )
               (error nil))
              (previous-property-change (max (- begin 1) 1))
              (next-property-change (min (+ end 1) (buffer-end 1)))
              )))
         (b (reduce 'min l))
         (e (reduce 'max l)))
   ;; (message (concat "prev: " 
   ;;                  (number-to-string (condition-case nil
   ;;                                      (scan-lists (point) 1 1)   ; ( stuff ) <-
   ;;                                     (error 0))) 
   ;;                  " next: "
   ;;                  (number-to-string (condition-case nil
   ;;                                      (scan-lists (scan-lists (point) 1 1) -1 0) ; -> ( stuff )
   ;;                                     (error 0)))
   ;;                  ))
   (message (concat "begin: " (number-to-string b) " end: " (number-to-string e)))
   (remove-text-properties b e '(display nil))
   ;; (save-excursion (font-lock-fontify-region b e))
   )
  )
 )

(defun rewrite-python-extend-after-change (beg end &optional old-len)
 (cons (line-beginning-position -1) (line-end-position 1)))

(defun abs-match (last)
 (cond ((re-search-forward "\\(^\\|[^[:alnum:]]\\)\\(abs(\\)" last t)
        (let* ((b1 (match-beginning 2))
               (e1 (match-end 2))
               (closing (or 
                         (condition-case nil
                           (scan-lists (- e1 1) 1 0)
                          (error nil))  ; nil if imbalanced
                         (buffer-end 1))))
         ;; (message (concat (number-to-string b1) " , " (number-to-string e1) " , " (number-to-string closing)))
         (cond ((and (<= closing last)  ; hasn't gone past last
                     (= (char-before e1)  ; is the same char
                        (or ; handle when its not in assoc list
                         (cadr (assq (char-before closing) '((?\) ?\()
                                                             (?\] ?\[)
                                                             (?\} ?\{)
                                                             (?\> ?\<))))
                         -1)))          ;never match char with -1
                (set-match-data (list b1 closing b1 e1 (- closing 1) closing))
                t)
               (t nil))))
       (t nil)))

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
    ("\\(^\\|[[:space:]]\\)\\(in\\)[[:space:]]"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "∈"))
               nil)))
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
    ("^[[:space:]]*\\(return\\)\\([^[:alnum:]]\\|$\\)"
     (0 (progn (put-text-property 
                (match-beginning 1) 
                (match-end 1) 'display (concat "◀"))
               nil)))
    ("\\([[:alnum:]]\\|[[:space:]]\\)\\(==\\)\\([[:alnum:]]\\|[[:space:]]\\)"
     (0 (progn (put-text-property 
                (match-beginning 2) 
                (match-end 2) 'display (concat "≡"))
               nil)))
    ;; I was rather insane to even try this
    ;; ("\\(^\\|[^[:alnum:]]\\n\\)\\(abs(\\)\\([^\\\n)]*\\))"
    ;;  ;; "\\(^\\|[^[:alnum:]]\\)\\(abs(\\)\\([^)]*\\)\\()\\)"
    ;;  (0 (progn 
    ;;      (let ((closing (scan-lists (+ (match-beginning 2) 3) 1 0)))
    ;;       (when (< (or closing (line-end-position)) (line-end-position))
    ;;        (put-text-property 
    ;;         (match-beginning 2)
    ;;         (or closing (buffer-end arg))
    ;;         'display 
    ;;         (concat (concat "|") (buffer-substring 
    ;;                      (match-end 2) 
    ;;                      (- (or closing (buffer-end arg)) 1)
    ;;                      ) (concat "|")))
    ;;        ))nil)))
    (abs-match
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
))
 ;; * as ×
 ;; ^ as ⊕
 ;; / as ÷
 nil)

(provide 'rewrite-python-mode)
