;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'dash)
(require 'trie-data)
(require 'trie-explore-funcs)

(provide 'trie-explore-mode)

;; Major mode for navigating through acab-database
;; TODO shift from current data model to use acab database

(defgroup trie-explore-mode '() "Trie Exploration Mode Customizations"
  :group 'acab-ide)
(defcustom trie-explore/overlay-max 20 "Maximum number of overlays for trie-explore-mode" :type '(integer))
(defcustom trie-explore/movement-regexp-guard "[[:alnum:]-]" "Regexp that guards layer movement" :type '(regexp))

(defstruct trie-explore/tree-data name
           root
           (indents '())
           (curr-path '())
           (max-lines 0)
           (start-pos (make-marker))
           (path-pos (make-marker))
           (overlays (make-hash-table)))

(defvar-local trie-explore/current-markers (list (make-marker) (make-marker)))
(defvar-local trie-explore/next-heading (make-marker))
(defvar-local trie-explore/current-layer 0)
(defvar-local trie-explore/current-data nil)
;;--------------------------------------------------
(defvar-local trie-explore-mode-map
  (let ((map (make-keymap)))
    ;; (define-key map "RET" 'trie-explore/expand-entry)
    ;;any potential functions? add particular structures?
    ;;TODO: M-RET to prev-line
    map)
  "Basic Keymap for Trie-Explore Mode"
  )

(defconst trie-explore-font-lock-keywords
  (list
   `("^\\(**\\) \\([[:alnum:]]+\\)"
     (0 "font-lock-keyword-face")
     (1 "font-lock-constant-face"))
   )
  "Highlighting of Trie-Explore entry titles"
  )

(defvar trie-explore-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?- "." st)
    st)
  "Syntax table for Trie-Explore-mode"
  )

(define-derived-mode trie-explore-mode fundamental-mode "Trie-Explore Mode"
  "Major Mode for exploring Trees"
  (interactive)
  (kill-all-local-variables)
  (use-local-map trie-explore-mode-map)
  (set (make-local-variable 'font-lock-defaults) (list trie-explore-font-lock-keywords nil))
  (set-syntax-table trie-explore-mode-syntax-table)
  (setq major-mode 'trie-explore-mode
        mode-name "TRIE-EXPLORE")
  (set-marker trie-explore/next-heading (point-max))
  (outline-minor-mode)
  (run-mode-hooks)
  )
