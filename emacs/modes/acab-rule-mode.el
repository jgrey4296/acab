;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
(provide 'acab-rule-mode)

;; TODO a mode for authoring and editing a *singular* acab rule
;;
(defvar-local acab-rule-mode-map
  (make-sparse-keymap))

;; Setup prettify symbols
;;

;; insert - using acab-company
;; fold query/transform/action/tags
;; get precursors / consumers
;; open tests


;; Fontify:
;; Rule name / end
;; types (::a)
;; subsentences [a]
;; queries a?
;; transforms a -> b
;; operators λa
;; actions λa:
;; strings "a"
;; comments
;; variables $a

;; ensure indentation


;; List of '(regex (groupnum "face")+)
(defconst acab-rule-font-lock-keywords
  (list
   ;; Rule Name
   `(,(rx line-start (group (+ (or word ?? ?.)) ?:))
     (1 "org-list-dt"))
   ;; End
   '("^end$" (0 "default"))
   ;; Var Block / Transform arrow
   '("|\\|->" (0 "org-code"))
   ;; Query
   `(,(rx (+ (or word ?! ?. ?# ?( ?) ?: ?$ ?λ blank)) ??)
     (0 "org-headline-todo" nil t)
     )
   ;; Type assign
   `(,(rx ?( (group ?: ?: (*? any)) ?))
     (1 "org-list-dt" t))
   ;; Action
   `(,(rx ?λ (+ (or word ?! ?.)))
     (0 "org-drawer" t))
   ;; Sentence
   `(,(rx (+ (or word ?! ?.)))
     (0 "org-level-5"))
   ;; Variable
   `(,(rx ?$ (+ alnum)) (0 "org-formula" t))
   ;; Tags
   `(,(rx ?# (+ alnum)) (0 "org-column-title" t))
     )
  "Highlighting for acab-rule-mode"
  )

(defun acab-rule-indent-line ()
  ;; TODO
  )

(define-derived-mode acab-rule-mode fundamental-mode
  "acab-rule"
  "Mode for authoring Acab Rule Structures"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-rule-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list acab-rule-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-rule-syntactic-face-function)
  (set (make-local-variable 'indent-line-function) 'acab-rule-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "#")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-rule-mode-syntax-table)
  ;;
  (setq major-mode 'acab-rule-mode)
  (setq mode-name "acab-rule")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )
