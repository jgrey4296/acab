;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(provide 'acab-string-expansion-mode)


;; TODO a tracery-inspired string expansion helper for authoring grammars
;; on a string, be able to call expansion through acab-py and generate multiple variables
;;
(define-minor-mode acab-string-expansion-minor-mode
  " Mode for manipulating string possibility grammars "
  :lighter "acab-string-expansion"
  :global t
  :keymap nil
  )
