;; org-roam-node-read+.el

(require 'org-roam-node)
(require 'org-roam-db-nodes-view)

(defvar org-roam-node-struct--slots
  '(file file-title file-hash file-atime file-mtime
	 id level point todo priority scheduled deadline title properties olp
	 tags aliases refs)
  "Define the list of slots to be used in the BOA Constructor
+org-roam-node-create.

Do NOT set this variable directly - instead use
`org-roam-node-struct-set-slots' to set this variable as well as
create the node struct appropriately!")

(defvar org-roam-node-struct-db-mapping
  '((file . "nodes_view.file")
    (file-title . "files.title")
    (file-hash . "files.hash")
    (file-atime . "files.atime")
    (file-mtime . "files.mtime")
    (id . "nodes_view.nview_id")
    (level . "nodes_view.\"level\"")
    (point . "nodes_view.pos")
    (todo . "nodes_view.todo")
    (priority . "nodes_view.priority")
    (scheduled . "nodes_view.scheduled")
    (deadline . "nodes_view.deadline")
    (title . "nodes_view.title")
    (properties . "nodes_view.properties")
    (olp . "nodes_view.olp")
    (tags . "nodes_view.tag")
    (aliases . "nodes_view.alias")
    (refs . "nodes_view.type_ref"))

  "Alist of mapping between structure slots and corresponding database column names.")

(defvar org-roam-directory-name (file-name-nondirectory (directory-file-name org-roam-directory))
  "The name of the org-roam root directory.")

(defvar org-roam-node-list-filter
  "where
   nodes_view.file not like '%%%%/references/%%%%'

  -- AND nodes_view.file not like '%%%%/folder/%%%%
  -- add additional folders like above."

  "Define default FILTER of `org-roam-node-list',
folders mentioned here are excluded from the list.")

(defvar org-roam-node-list-sort
  "order by
   length(nodes_view.file) - length(replace(nodes_view.file, '/', '')),
   case when nodes_view.file like '%%%%guides%%%%' then files.mtime end desc,
   files.mtime desc,
   nodes_view.\"level\" desc

  -- Keep root files in the beginning
  -- then anything in the guides folder (sort this by modification time)
  -- sort all other categories by modification time
  -- also keep headline nodes before file nodes."

  "Define default SORT of `org-roam-node-list'.")

(defvar org-roam-node-list-differentiate-aliases nil
  "Whether to differentiate each alias as a node in org-roam-node-list")

(defun org-roam-node-struct-create (&optional slots)
  "Dynamically create the `org-roam-node` struct with a BOA CONSTRUCTOR.
BOA stands for By Order of Arguments - I'm not making this up,

The slot list is defined in `org-roam-node-struct--slots'."
  (let ((constructor-args (or slots
			      org-roam-node-struct--slots)))
    (eval `(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
					(:constructor +org-roam-node-create ,constructor-args)
					(:copier nil))
	     "A heading or top level file with an assigned ID property."
	     file file-title file-hash file-atime file-mtime
	     id level point todo priority scheduled deadline title properties olp
	     tags aliases refs))))

(defun org-roam-node-struct-set-slots (&optional slots)
  (let ((slots (or slots
		   org-roam-node-struct--slots)))
    (setq org-roam-node-struct--slots slots)
    (org-roam-node-struct-create slots)))

;; Evaluate the function to create the struct
(org-roam-node-struct-set-slots '(id &optional file file-title level point olp file-mtime title))

;; (org-roam-node-struct-set-slots '(id &optional file file-title level todo point priority scheduled deadline ;; 0 - 8
				     ;; properties olp file-atime file-mtime tags refs title aliases))         ;; 9 - 16

(defun +org-roam-node-list (&optional filter sort)
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)  ; let users reuse db-gc threshold here - set it to
	                                               ; (* 2 8 1024 1024) 16mb, very marginal returns after this.
	 (slots org-roam-node-struct--slots)
	 (columns (cl-remove-if 'null
				(mapcar (lambda (slot)
					  (cdr (assoc slot org-roam-node-struct-db-mapping)))
					slots)))
	 (column-names (mapconcat 'identity columns ", "))
	 (rows (org-roam-db-query
		(format
		 "select %s
		  from nodes_view left join files on nodes_view.file = files.file
	           %s
        	   %s;"
		 column-names
		 (or filter org-roam-node-list-filter "")
		 (or sort org-roam-node-list-sort "")))))

    (if org-roam-node-list-differentiate-aliases
	;; then
	(progn
	  ;; calculate common value from example row
	  (let* ((ex-row (car rows))                                       ; Take the first row to determine lengths
		 (total-length (length ex-row)))                           ; length of a row
	    
	    ;; (common-attrs (seq-subseq row 0 15)) ; Combine slots 0 through 14
	    ;; (title (nth 15 row))                 ; Title at index 15
	    ;; (aliases (nth 16 row))               ; Aliases at index 16
	    
	    (cl-loop for row in rows
		     append (let ((common-attrs (seq-subseq row 0 (- total-length 2)))  ; Extract common attributes
				  (title (nth (- total-length 2) row))                  ; Title at index 15
				  (aliases (nth (- total-length 1) row)))               ; Aliases at index 16
		     (mapcar (lambda (temp-title)
			       (apply '+org-roam-node-create 
				      (append common-attrs 
					      (list temp-title aliases))))
			     (cons title aliases))))))
      ;; else
      (cl-loop for row in rows
	       collect (apply #'+org-roam-node-create row))))) 

(defun +org-roam-node-read--completions (&optional filter sort)
  "Modfied `org-roam-node-read--completions'
Filtering and Sorting made redundant.

Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-node'.

The displayed title is formatted according to `+org-roam-node-display-template'."
  (mapcar (lambda (node)
	    (let ((display-candidate (+org-roam-node-display-template node)))
	      (cons display-candidate node)))
	  (+org-roam-node-list filter sort)))

;; Customise appearance of nodes in `org-roam-find' & `org-roam-insert'
(defun +org-roam-node-display-template (node)
  (let* ((level (org-roam-node-level node))
	 (file (org-roam-node-file node))
	 (dir (if (string-match
		   (format "/%s/\\([^/]+\\)/" org-roam-directory-name)
		   file)
		  (match-string 1 file)
		"/"))
	 (formatted-dir (format "(%s)" dir))
	 (dir-width 10)  ;; Set fixed width for the directory component
	 (padded-dir (truncate-string-to-width (format "%-10s" formatted-dir) dir-width)))
    (concat
     padded-dir " "
     (when (> level 0) (concat (propertize (org-roam-node-file-title node) 'face 'italic) " / "))
     (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
     (propertize (org-roam-node-title node) 'face 'bold))))

;; Initialise the new read protocol
(advice-add 'org-roam-node-read--completions :override #'+org-roam-node-read--completions)

(provide 'org-roam-node-read+)
