;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; TODO A high level institution authoring mode
;; Auto structure of participants, activity sets,
;; values, IGU, roles, induction, artifacts...

(provide 'acab-inst-mode)

(defvar-local acab-inst-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-inst-font-lock-keywords
  (list)
  "Highlighting for acab-inst-mode"
  )

(define-derived-mode acab-inst-mode fundamental-mode
  "acab-inst"
  "Mode for authoring Institution Structure Descriptions"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-inst-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-inst-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-inst-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-inst-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-inst-mode-syntax-table)
  ;;
  (setq major-mode 'acab-inst-mode)
  (setq mode-name "acab-inst")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
