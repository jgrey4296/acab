;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; TODO mode for authoring activity diagrams
;; Auto structure of subject - tool - object - outcome
;; plus easy expansion to rules - community - div of labour
;; duplication, replacement, grammar expansion

(provide 'acab-activity-mode)



(defvar-local acab-activity-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-activity-font-lock-keywords
  (list)
  "Highlighting for acab-activity-mode"
  )

(define-derived-mode acab-activity-mode fundamental-mode
  "acab-activity"
  "For Authoring Activity-Theoretic structures"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-activity-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-activity-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-activity-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-activity-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-activity-mode-syntax-table)
  ;;
  (setq major-mode 'acab-activity-mode)
  (setq mode-name "acab-activity")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )
