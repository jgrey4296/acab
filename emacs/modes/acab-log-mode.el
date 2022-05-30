;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(provide 'acab-log-mode)

;; TODO for navigating and narrowing acab-py logs

;; Narrow by Layer, Agent, sentence, string expansion
;; timestep...

;; Move by ...

;; diff two layers, agent, sentences...

;; queries/transforms/actions/rules/operators
;; types, strings, rule/layer/agenda


(defvar-local acab-log-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-log-font-lock-keywords
  (list)
  "Highlighting for acab-log-mode"
  )

(define-derived-mode acab-log-mode fundamental-mode
  "acab-log"
  "Mode for navigating and narrowing acab-py logs"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-log-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-log-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-log-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-log-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-log-mode-syntax-table)
  ;;
  (setq major-mode 'acab-log-mode)
  (setq mode-name "acab-log")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
