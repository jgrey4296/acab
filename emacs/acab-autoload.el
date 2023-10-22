;;;acab-ide/acab-autoload.el -*- lexical-binding: t; -*-

(provide 'acab-autoload)

(add-to-list 'auto-mode-alist '("\\.acab\\'"   . acab-ide-minor-mode))
(add-to-list 'auto-mode-alist '("\\.trie\\'"   . trie-mode))
(add-to-list 'auto-mode-alist '("\\.acab.log"  . acab-log-mode))
(add-to-list 'auto-mode-alist '("\\.rule"      . acab-rule-mode))
(add-to-list 'auto-mode-alist '("\\.inst"      . acab-inst-mode))
(add-to-list 'auto-mode-alist '("\\.layer"     . acab-layer-navigator-mode))
(add-to-list 'auto-mode-alist '("\\.game"      . acab-game-mode))
(add-to-list 'auto-mode-alist '("\\.seq\\'"    . acab-sequence-mode))
(add-to-list 'auto-mode-alist '("\\.activity"  . acab-activity-mode))
