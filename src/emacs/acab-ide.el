;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; The master Acab-IDE access point

;; Components
(require 'acab-comint)
(require 'acab-faces)
(require 'acab-hydras)

;; Modes
(require 'acab-log-mode)
(require 'acab-inst-mode)
(require 'acab-rule-mode)
(require 'acab-sequence-mode)
(require 'acab-ide-minor-mode)

;; trie Specific
(require 'trie-explore-mode)
(require 'trie-data )
(require 'trie-company)

;; Autoloads
(require 'acab-autoload)

(provide 'acab-ide)

;; TODO add a parse-checker
