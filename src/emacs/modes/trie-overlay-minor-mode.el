;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;based On https://www.emacswiki.org/emacs/ModeTutorial
;;For allowing code to run when the mode is run:
(require 'dash)
(require 'acab-faces)
(require 'trie-management)

(provide 'trie-overlay-minor-mode)


(define-minor-mode trie-overlay-minor-mode
  "A Minor Mode for highlighting trie sentences using an overlay "
  :lighter "TrieMM"
  :keymap (make-sparse-keymap)
 )
