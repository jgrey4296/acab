;;; ../../../Volumes/documents/github/emacs_files/packages/acab-ide/modes/acab-type-mode.el -*- lexical-binding: t; -*-


(provide 'acab-type-mode)

(defvar-local acab-type-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-type-font-lock-keywords
  (list)
  "Highlighting for acab-type-mode"
  )

(define-derived-mode acab-type-mode fundamental-mode
  "acab-type"
  "Major mode for editing and working with type structures"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-type-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-type-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-type-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-type-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-type-mode-syntax-table)
  ;;
  (setq major-mode 'acab-type-mode)
  (setq mode-name "acab-type")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("" . acab-type-mode))
