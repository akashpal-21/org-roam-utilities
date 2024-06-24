;; org-roam-node-read+.el

(require 'org-roam-node)
(require 'org-roam-db-nodes-view)


;; HHHH---------------------------------------------------
;; Variables

(defconst org-roam-node-struct--slots
  '(id file file-title level todo point priority scheduled deadline
       properties olp file-atime file-mtime tags refs title aliases)

  "Define the list (&order) of slots used in the BOA Constructor
`+org-roam-node-create'.")

(defvar org-roam-node-list--query-subset
  "nodes_view.nview_id,
 nodes_view.file,
 files.title,
 nodes_view.\"level\",
 nodes_view.todo,
 nodes_view.pos,
 nodes_view.priority,
 nodes_view.scheduled,
 nodes_view.deadline,
 nodes_view.properties,
 nodes_view.olp,
 files.atime,
 files.mtime,
 nodes_view.tag,
 nodes_view.type_ref,
 nodes_view.title,
 nodes_view.alias"

  "Stores the column names of the subset of `org-roam-node-struct--slots'
that is used in `+org-roam-node-list' for querying from the db

Be careful when setting this variable directly,
use `org-roam-node-struct-set-slots' to set this variable appropriately.

 Evaluate the function to generate a subset for querying the db
 Example: (org-roam-node-struct-set-slots '(id file file-title level point olp file-mtime title))
 Default: (org-roam-node-struct-set-slots org-roam-node-struct--slots)")

(defconst org-roam-node-struct-db-mapping
  '((nil . "null")
    (file . "nodes_view.file")
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

  -- AND nodes_view.file not like '%%%%/folder/%%%%'
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

  "Define default SORT of `+org-roam-node-list'.")

(defvar org-roam-node-list-differentiate-aliases t
  "Whether to differentiate each alias as a node in org-roam-node-list")


;; HHHH---------------------------------------------------
;; Definitions

;; Create the `org-roam-node` struct with a BOA CONSTRUCTOR `+org-roam-node-create'.
;; BOA stands for By Order of Arguments - I'm not making this up
(cl-defstruct (org-roam-node (:constructor org-roam-node-create)
			     (:constructor +org-roam-node-create
					   (id &optional
					       file file-title level todo point priority scheduled deadline
					       properties olp file-atime file-mtime tags refs title aliases))
			     (:copier nil))
  "A heading or top level file with an assigned ID property."
  file file-title file-hash file-atime file-mtime
  id level point todo priority scheduled deadline title properties olp
  tags aliases refs)

(defun org-roam-node-struct-set-slots (arg-list)
  "Create a subset of `org-roam-node-struct--slots'
with appropriate conversions to column names in the db
to be used in `+org-roam-node-list' for querying the database.

This sets the variable `org-roam-node-list--query-subset'

Arguments may be provided in any order."
  (let (subset)

    (dolist (arg arg-list)
      (unless (member arg org-roam-node-struct--slots)
	(warn "Invalid argument %s provided! Ignored!" arg)))

    (dolist (slot org-roam-node-struct--slots)
      (if (member slot arg-list)
	  (setq subset (append subset (list slot)))
	(setq subset (append subset (list nil)))))

    (setq org-roam-node-list--query-subset
	  (mapconcat (lambda (column)
		       (cdr (assoc column org-roam-node-struct-db-mapping)))
		     subset
		     ", "))))

(defun +org-roam-node-list (&optional filter sort)
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
	 ;; let users reuse db-gc threshold here - set it to
	 ;; (* 2 8 1024 1024) 16mb, very marginal returns after this.
	 (rows (org-roam-db-query
		(format
		 "select %s
		  from nodes_view join files using (file)
		   %s
		   %s;"
		 org-roam-node-list--query-subset
		 (or filter org-roam-node-list-filter "")
		 (or sort org-roam-node-list-sort "")))))

    (if org-roam-node-list-differentiate-aliases
	;; then
	(progn
	  (cl-loop for row in rows
		   append (let ((common-attrs (seq-subseq row 0 15)) ; Combine slots 0 through 14
				(title (nth 15 row))                 ; Title at index 15
				(aliases (nth 16 row)))              ; Aliases at index 16
			    (mapcar (lambda (temp-title)
				      (apply #'+org-roam-node-create
					     (append common-attrs
						     (list temp-title aliases))))
				    (cons title aliases)))))
    ;; else
    (cl-loop for row in rows
	     collect (apply #'+org-roam-node-create row)))))

(defun +org-roam-node-read--completions (&optional filter sort)
  "Modfied `org-roam-node-read--completions'
Filtering and Sorting is delegated to `+org-roam-node-list'

Return an alist for node completion.
The car is the displayed title or alias for the node, and the cdr
is the `org-roam-node'.

The displayed title is formatted according to `+org-roam-node-display-template'."
  (mapcar (lambda (node)
	    (let ((display-candidate (+org-roam-node-display-template node)))
	      (cons display-candidate node)))
	  (+org-roam-node-list filter sort)))

;; HHHH---------------------------------------------------
;; Customisations

;; Customise appearance of nodes in `org-roam-find' & `org-roam-insert'
;; Redefine this function with the required display template.
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

;; HHHH---------------------------------------------------

;; Initialise the new read protocol
(advice-add 'org-roam-node-read--completions :override #'+org-roam-node-read--completions)


;; End
(provide 'org-roam-node-read+)
