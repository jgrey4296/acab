;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;; TODO context aware company completion of rule names, layers,
;; operators etc
(require 'cl-lib)
(require 'company)

(provide 'acab-company)
;; Applicable Contexts:
;; Rule/Layer/Pipeline/Ageda names
;;
;; Variables marked with $
;; Operators marked with λ
;; TYPES marked with ::
;; TAGS marked with #
;; The above ↑ could all be linked to acab config setup
;;
;; For a separate mode: FSM States, Game States, Agent Names,
;; String expansions...

(defun acab-company/get-sentence ()

  )

(defun acab-company/search (sentence)

  )

(defun acab-company/backend (cmd &rest args)
  (cl-case cmd
    (init            nil)
    ;; Prefix Acab Company: check context, get line substring
    (prefix          (acab-company/get-sentence))
    ;; Navigate down context db, return next children
    (candidates      (acab-company/search (car args)))
    ;; Settings
    (sorted          t)
    (duplicates      t)
    (ignore-case     t)
    (no-cache        nil)
    (annotation      nil)
    (meta            nil)
    (location        nil)
    (post-completion nil)
    (require-match   nil)
    (t               nil)
    )
  )

(define-minor-mode acab-company-minor-mode
  " Minor Mode for Acab Company completion "
  :lighter "acab-company"
  :global t
  (cond
   (acab-company-minor-mode
    (setq company-backends
          (add-to-list 'company-backends 'acab-company/backend))
    (setq company-selection-default nil))
   (t
    (setq company-backends
          (delete 'acab-company/backend company-backends))
    (setq company-selection-default 0))))
