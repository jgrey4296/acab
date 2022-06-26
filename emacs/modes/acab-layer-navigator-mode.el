;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
(provide 'acab-layer-navigator-mode)

;; TODO a rule for visualising and navigating between layers of rules
;; as a window ring
;;
;;VISUAL
(defun acab-ide/decrement-priors-layer ()
  (interactive)
  ;;TODO decrement priors layer
  )
(defun acab-ide/increment-priors-layer ()
  (interactive)
  ;;TODO increment priors layer
  )
(defun acab-ide/decrement-posts-layer ()
  (interactive)
  ;;TODO decrement post layer
  )
(defun acab-ide/increment-posts-layer ()
  (interactive)
  ;;TODO increment post layer
  )

(defvar-local acab-layer-navigator-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-layer-navigator-font-lock-keywords
  (list)
  "Highlighting for acab-layer-navigator-mode"
  )

(define-derived-mode acab-layer-navigator-mode fundamental-mode
  "acab-layer-navigator"
  "Mode for manipulating acab layers"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-layer-navigator-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-layer-navigator-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-layer-navigator-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-layer-navigator-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-layer-navigator-mode-syntax-table)
  ;;
  (setq major-mode 'acab-layer-navigator-mode)
  (setq mode-name "acab-layer-navigator")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
