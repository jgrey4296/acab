;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'hydra)
(provide 'acab-hydras)

(defhydra trie-help-hydra (:color pink)
  "
   | General           ^^|
   |-------------------^^+
   | [_q_] Quit          |
   |                   ^^|
   |                   ^^|
   |                   ^^|
  "

  ("q" nil :exit t)
  )

(defhydra trie-sequence-transient ()
  "
   | General           ^^| Change                    ^^| Motion             ^^| Remove              ^^| Sort                         ^^|
   |-------------------^^+---------------------------^^+--------------------^^+---------------------^^+------------------------------^^|
   | [_q_] Quit          | [_i_] Insert Rule           |                    ^^| [_d_] Delete Value    | [_s_] Sort Table Alpha         |
   | [_n_] New Table     |                           ^^|                    ^^| [_D_] Delete Column   |                              ^^|
   | [_v_] Table Inspect | [_r_] Rename Column         | [_c_] Centre Column  | [_m_] Merge Column    |                              ^^|
   | [_b_] Set Right Tab | [_t_] Insert Terminal       |                    ^^|                     ^^|                              ^^|
  "
  ("q" nil :exit t)
  ("n" #'trie-sequence/new-table ) ;; org create table, insert
  ("v" #'trie-sequence/inspect-table) ;; create a left temp buffer that shows selected column's values (plus highlights active ones)
  ("b" nil ) ;; create a right temp buffer that shows selected column's values (plus highlights active ones)
  ("i" #'trie-sequence/insert-rule) ;; specify LHS and RHS, insert into factbase, insert into appropriate columns
  ("r" #'trie-sequence/rename-column) ;; Rename the column from default
  ("t" #'trie-sequence/insert-terminal) ;; Insert an Input terminal
  ("c" #'trie-sequence/centre-column) ;; Centre the current column
  ("d" #'trie-sequence/delete-value) ;; Delete the value at point from the table
  ("D" #'trie-sequence/delete-column) ;; Delete the column from the table
  ("m" nil ) ;; Merge the left connections and the right connections
  ("s" #'trie-sequence/sort-table) ;; sort all columns alphabetically
  )

(defhydra trie-explore-transient ()
  "
   | General           ^^|
   |-------------------^^+
   | [_q_] Quit          |
  "
  ("q" nil :exit t)
  )
