;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;based On https://www.emacswiki.org/emacs/ModeTutorial
(require 'acab-ide-management)
(require 'acab-comint)
(require 'acab-company)
(require 'acab-window-manager)
(require 'trie-company)

(provide 'acab-ide-minor-mode)

;; startup/shut down acab
;; switch modes
;; popup logs

(defgroup acab-ide '() "Acab Mode Customizations")
;;--------------------
;; Mode Variables
;;--------------------
(defcustom acab-ide-minor-mode-hook nil "Basic Hook For Acab Mode" :type '(hook))

;;--------------------
;;Utilities
;;--------------------
(defun acab-ide/no-op ()
  (interactive)
  )

;;--------------------
;;Key bindings.
;;use sparse-keymap if only a few bindings
;;--------------------
(defvar acab-ide-minor-mode-map
  (make-sparse-keymap)
  "Keymap for Acab ide mode")

;; --------------------
;;Entry Function
;;--------------------
(define-minor-mode acab-ide-minor-mode
  "Major Mode for creating rules using Acab"
  :init-value nil
  :lighter "Acab-IDE"
  :keymap acab-ide-minor-mode-map
  :global t
  (if acab-ide-minor-mode
      (progn
        (message "Acab IDE: Enabled")
        ;; Init management variables
        (acab-ide/init)
        ;; start acab-window-manager
        (acab-wm/init)
        ;; start up acab-comint
        (acab-comint/init)
        ;; Setup acab-company
        (acab-company-minor-mode)
        ;; Setup trie-company
        (trie-company-minor-mode)
        )
    (progn
      (message "Acab IDE: Disabled")
      ;; Cleanup the IDE
      ))
 )
