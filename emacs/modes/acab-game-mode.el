;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;

;; TODO a mode for authoring game theoretic structures
;; providing definition of game structure,
;; and ascii rendering for review
;; insertion/deletion of moves/payoffs etc

(provide 'acab-game-mode)

(defvar-local acab-game-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst acab-game-font-lock-keywords
  (list)
  "Highlighting for acab-game-mode"
  )

(define-derived-mode acab-game-mode fundamental-mode
  "acab-game"
  "Mode for Authoring Game-Theoretic Structures"
  (interactive)
  (kill-all-local-variables)
  (use-local-map acab-game-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list acab-game-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'acab-game-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'acab-game-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table acab-game-mode-syntax-table)
  ;;
  (setq major-mode 'acab-game-mode)
  (setq mode-name "acab-game")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
