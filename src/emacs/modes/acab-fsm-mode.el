;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; acab mode for manipulating and testing fsms and protocols,
;; including rendering into plantuml

(provide 'acab-fsm-mode)

(defvar-local acab-fsm-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-fsm-font-lock-keywords
  (list)
  "Highlighting for acab-fsm-mode"
  )

(define-derived-mode acab-fsm-mode fundamental-mode
  "acab-fsm"
  "Mode for manipulating fsms and protocosl"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-fsm-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-fsm-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-fsm-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-fsm-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-fsm-mode-syntax-table)
  ;;
  (setq major-mode 'acab-fsm-mode)
  (setq mode-name "acab-fsm")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("" . acab-fsm-mode))
