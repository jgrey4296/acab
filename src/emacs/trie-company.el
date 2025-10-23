;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; https://github.com/company-mode/company-mode/wiki/Writing-backends
;;
(require 'cl-lib)
(require 'company)
(provide 'trie-company)

(defvar trie-company/completion-trie nil)

(defun trie-company/get-sentence ()
  ;; Get the sentence from the current position in the working buffer
  ;; possibly look back past line to get run on sentences
  (buffer-substring (line-beginning-position) (point))
  )

(defun trie-company/search (sentence)
  ;; Return completion options from the acab-ide trie-database
  ;;for the given sentence
  (mapcar (lambda (x) (concat sentence x))'("a" "b" "c" "c" "e"))
  )

(defun trie-company/backend (cmd &rest args)
  (interactive (list 'interactive))
  (message "Trie Company Backend: %s : %s" cmd args)
  (cl-case cmd
    (init            nil)
    ;; For Trie Prefix: get the sentence substring of the line, must end in DOT or BANG
    (prefix          (trie-company/get-sentence))
    ;; For Trie Candidates: navigate the trie db, return existing children
    (candidates      (trie-company/search (car args)))
    ;; Defaults
    (sorted          t)
    (duplicates      t)
    (ignore-case     t)
    (no-cache        nil)
    ;; Add data in completion window
    (annotation      nil)
    ;; Add data in echo
    (meta            nil)
    (location        nil)
    (post-completion nil)
    ;; For easy use of backend:
    (interactive     (company-begin-backend 'trie-company/backend))
    ;; Difference between usage / creation:
    (require-match   nil)

    (t               nil)
    )
  )

(define-minor-mode trie-company-minor-mode
 "This minor mode enables completion of trie sentences using company."
  :init-value nil
  :global t
  (cond
   (trie-company-minor-mode
    (setq company-backends
          (add-to-list 'company-backends 'trie-company/backend))
    (setq company-selection-default nil))
   (t
    (setq company-backends
          (delete 'trie-company/backend company-backends))
    (setq company-selection-default 0))))
