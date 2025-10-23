;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'dash)
(provide 'trie-data)
;;Example:
;; (setq root (make-trie-data/node :name "__root"))
;; (trie-data/tree-add root '("a" "b" "c") "value")
;; (trie-data/tree-add root '("a" "b" "d") "value")
;; (message "%s" (trie-data/node-to-string root))
;; (trie-data/tree-children root '("periostea" "disquisitional" "unimplicitly" "Champlain"))
;; (trie-data/generate-tree root 3 4)
;; (with-temp-buffer
;;   (insert-file-contents "/usr/share/dict/words")
;;   (mapcar (lambda (x)
;;             (goto-char (random (point-max)))
;;             (buffer-substring (line-beginning-position)
;;                               (line-end-position)))
;;           (make-list 5 "_")))

(cl-defstruct trie-data/node name value
              (children (make-hash-table :test 'equal)))

;; Tree Operations
(defun trie-data/generate-tree (node n-children layers)
  " Create a tree of n-children and layers, using random dictionary words as nodes "
  (if (> layers 0)
      (with-temp-buffer
        (insert-file-contents "/usr/share/dict/words")
        (let* ((words (mapcar (lambda (x)
                                (goto-char (random (+ 1 (point-max))))
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                              (make-list (+ 1 (random n-children)) "_")))
               (new-nodes (mapcar (lambda (x) (trie-data/node-add-child node x))
                                  words))
               )
          (mapcar (lambda (x) (trie-data/generate-tree x n-children (- layers 1))) new-nodes)
          )
        )
    )
  )
(defun trie-data/tree-add (rootnode path val)
  " Add a node with a value as a leaf of path from rootnode, creating
intermediate nodes if necessary "
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (setq curr-node (if (trie-data/node-has-child curr-node curr-path)
                          (trie-data/node-get-child curr-node curr-path)
                        (trie-data/node-add-child curr-node curr-path))
            curr-path (pop path))
      )
    )
  )
(defun trie-data/tree-children (rootnode path)
  " Get the children of a node, using a path from the root "
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (trie-data/node-has-child curr-node curr-path)
          (setq curr-node (trie-data/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        (hash-table-keys (trie-data/node-children curr-node))
      '())
  )
)
(defun trie-data/tree-get (rootnode path)
  " Get a node using a path from the rootnode "
  (let* ((curr-node rootnode)
         (curr-path (pop path))
         )
    (while curr-path
      (if (trie-data/node-has-child curr-node curr-path)
          (setq curr-node (trie-data/node-get-child curr-node curr-path)
                curr-path (pop path))
        (setq curr-path nil))
      )
    (if curr-node
        curr-node
      nil)
    )
  )
(defun trie-data/tree-remove (rootnode path child)
  " Remove the leaf of a path from the tree "
  (message "Removing %s from %s " child path)
  (let ((node (trie-data/tree-get rootnode path)))
    (if (trie-data/node-has-child node child)
        (remhash child (trie-data/node-children node))
        )
    )
  )
(defun trie-data/dfs-tree (n pred)
  " Apply Pred to each node, and return the nodes that pass "
  (if (hash-table-empty-p (trie-data/node-children n))
      ;;base case
      (if (apply pred n)
          n
        nil
        )
      ;;recursive case
    (-flatten
     (cons (if (apply pred n) n nil)
           (mapcar 'trie-data/dfs-tree
                   (hash-table-values (trie-data/node-children n)))))
    )
)

;;Node Operations
(defun trie-data/node-to-string (n)
  " Convert a Tree Node to a User Understandable String "
  (format "Trie-Data/Node: %s %s (%s)" (trie-data/node-name n)
          (trie-data/node-value n)
          (string-join (hash-table-keys (trie-data/node-children n)) ", "))
  )
(defun trie-data/node-add-child (n childname)
  " Add a new node of childname to node n "
  (puthash childname (make-trie-data/node :name childname)
           (trie-data/node-children n)))
(defun trie-data/node-has-child (n childname)
  " Check if a node has a child by the name of childname "
  (-contains? (hash-table-keys (trie-data/node-children n)) childname))
(defun trie-data/node-get-child (n childname)
  " Get the child of a node "
  (gethash childname (trie-data/node-children n)))
