;;; ../../../Volumes/documents/github/emacs_files/packages/acab-ide/modes/acab-test-mode.el -*- lexical-binding: t; -*-

;; Mode for authoring test frameworks of a rule/layer/pipeline etc

(defvar-local acab-test-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-test-font-lock-keywords
  (list)
  "Highlighting for acab-test-mode"
  )

(define-derived-mode acab-test-mode fundamental-mode
  "acab-test"
  "Major Mode for writing tests of rules/layers/pipelines/str expansions"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-test-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-test-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-test-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-test-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-test-mode-syntax-table)
  ;;
  (setq major-mode 'acab-test-mode)
  (setq mode-name "acab-test")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("" . acab-test-mode))

(provide 'acab-test-mode)
