;; org-roam-link-utils.el -*- Some utilities for managing org-roam links ("roam:")


;; HHHH---------------------------------------------------
;; Customisations

;; Do not auto transform "roam:" links to "id:" links on save.
(setq org-roam-link-auto-replace nil)

(defun org-roam-link-replace-all--export (backend)
  "Preprocess the buffer to replace \"roam:\" links with \"id:\" links."
  (org-roam-link-replace-all))
(add-hook 'org-export-before-processing-functions #'org-roam-link-replace-all--export)

;; Customise appearance of [[roam:]] links
(org-link-set-parameters "roam" :follow #'org-roam-link-follow-link :face 'nobreak-space)


;; HHHH---------------------------------------------------
;; Helper Function

(defun +org-roam-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer.
Like `org-id-goto', but additionally uses the Org-roam database"
  (interactive "sID: ")
  (let ((m (org-roam-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-fold-show-context)))


;; HHHH---------------------------------------------------
;; Definitions

(defun org-roam-link-find ()
  "Find \"roam:\" links in Org-Roam"
  (interactive)
  (let* ((link-type "roam")
	 (query "select
		 files.title, links.source, links.pos,
			      links.dest, links.properties
		 from links
		 inner join nodes on links.source = nodes.id
		 inner join files on nodes.file = files.file
		 where links.type = $s1")
	 (links (org-roam-db-query query link-type))
	 (choices (mapcar (lambda (link)
			    (let* ((file-title (nth 0 link))
				   (pos (nth 2 link))
				   (dest (nth 3 link))
				   (outline (mapconcat #'identity
						       (plist-get (nth 4 link) :outline)
						       " > "))
				   (description (format "%s: %s [%d] %s"
							dest
							file-title
							pos
							(if (not (string= "" outline))
							    (concat "> " outline)
							  ""))))
			      (cons description (list pos (nth 1 link)))))
			  links))
	 (selection (completing-read "Select a roam link: " (mapcar #'car choices)))
	 (selected-data (cdr (assoc selection choices)))
	 (pos (nth 0 selected-data))
	 (id (nth 1 selected-data)))
    (org-roam-id-open id nil)
    (goto-char pos)))

(defun org-roam-link-report-dangling ()
  "Create a report of dangling links (broken links)
in Org-Roam,

A table containing the sources and the links themselves are presented."
  (interactive)
  (let ((buffer (generate-new-buffer "*Org-Roam Dangling Links*"))
	(query (org-roam-db-query
		"select
			'\"id:' || ltrim(links.source, '\"'),
		       '(' || group_concat(rtrim(links.type, '\"') || ':' || ltrim(links.dest, '\"'), ' \"\n||\" ') || ')'
		   from links
		   where links.type in ('\"roam\"', '\"id\"')
		     and (rtrim(links.type, '\"') || ':' || ltrim(links.dest, '\"'))
			 not in
			       (select '\"id:' || ltrim(nodes.id, '\"') from nodes
			  union select '\"roam:' || ltrim(nodes.title, '\"') from nodes
			  union select '\"roam:' || ltrim(aliases.alias, '\"') from aliases)
		   group by links.source;")))
    (with-current-buffer buffer
      (switch-to-buffer buffer)
      (org-mode)
      (insert "#+TITLE: Dangling Links Report\n\n")
      (insert "* Dangling Links\n\n")
      (insert "| Source | Broken Links \n")
      (insert "|")
      (org-table-align)
      (org-table-insert-hline)
      (forward-line 2)
      (dolist (row query)
	(insert "||\n")
	(insert (format "| %s | %s\n" (car row) (cadr row)))
	(org-table-align))
      (goto-char (point-min)))))

(cl-defun org-roam-link-query-backlinks (id title
					 &optional
					 no-id-links
					 no-roam-title-links
					 no-roam-aliases-links
					 &key
					 aliases)
  "Query the SOURCEs where forward links have been defined,
along with the type of links (ID / ROAM).

nth 0 is the SOURCE ID where forward links exist,
nth 1 to 3 are the associated link types in the SOURCE

nth 1 is non-nil if ID links exists in SOURCE
      and returns the node ID
nth 2 is non nil if \"roam:TITLE\" links exists in SOURCE
      and returns the node TITLE
nth 3 is non nil if \"roam:ALIASES\" links exists in SOURCE
      and returns the list of node ALIASES used.

Optional arguments:

When optional NO-ID-LINKS is NON NIL
	      do not query for ID backlinks

When optional NO-ROAM-TITLE-LINKS is NON NIL
	      do not query for \"roam:TITLE\" backlinks

When optional NO-ROAM-ALIASES-LINKS is NON NIL
	      do not query for \"roam:ALIASES\" backlinks.

KEYWORD ARGUMENTS:

- ALIASES: a list of aliases to query for.
"
  (org-roam-db-query
   (format  "
  with
      alias_links as
		    (select links.source as source, '(' || group_concat(alias, ' ') || ')' as aliases
		     from  aliases inner join links on aliases.alias = links.dest
		     where node_id = '\"%s\"' and %s
		     group by links.source),

      title_links as
		    (select  links.source as source, '(' || links.dest || ')' as title_link
		     from links
		     where links.dest = '\"%s\"'
		     group by links.source),

      id_links as
		   (select links.source as source, '(' || links.dest || ')' as id_link
		    from links
		    where links.dest = '\"%s\"'
		    group by links.source)

  select source, max(id_link) as id_link, max(title_link) as title_link, max(aliases) as aliases
  from
  (select source, aliases, null as title_link, null as id_link from alias_links
  union all
  select source, null as aliases, title_link, null as id_link from title_links
  union all
  select source, null as aliases, null as title_link, id_link from id_links) as combined_data

  group by source;"
	    (unless no-roam-aliases-links id)
	    (if aliases (format "links.dest IN (%s)"
				(mapconcat (lambda (alias) (format "'\"%s\"'" alias)) aliases ", "))
	      "links.dest = aliases.alias")
	    (unless no-roam-title-links title)
	    (unless no-id-links id))))

(defun org-roam-link-replace-all-backlinks ()
  "For all \"roam:\" links referencing current node,
Convert to an id link &,
Convert every raw id link - if any - [[id:uuid]] to
[[id:uuid][description]] where description is node-title."
  (interactive)
  (when-let* ((original-buffer (current-buffer))
	      (node (org-roam-node-at-point))
	      (title (org-roam-node-title node))
	      (id (org-roam-node-id node))
	      (links (org-roam-link-query-backlinks id title)))
    (mapc (lambda (link)
	    (let ((ids (nth 0 link))
		  (roam-re (append (nth 2 link) (nth 3 link))))
	      (+org-roam-id-goto ids)
	      (org-with-point-at 1
	      (while (re-search-forward org-link-bracket-re nil t)
		(cond ((and (seq-some
			     (lambda (re)
			       (string-match-p (concat "^roam:" re "$") (match-string 1)))
			     roam-re)
			(y-or-n-p (format "Convert %s?" (match-string 1))))
		       (org-roam-link-replace-at-point))
		      ((and (string-match-p (concat "^id:" id "$") (match-string 1))
			    (not (match-string 2))
			    (y-or-n-p (format "Update description of %s?" (match-string 1))))
		       (goto-char (match-end 1))
		       (forward-char 1)
		       (insert (format "[%s]" title))))))
	    (write-file (buffer-file-name))))
	  links)
    ;; switch to our orignal-buffer
    (switch-to-buffer original-buffer)))

(defun org-roam-link-rename-all-backlinks (&optional alias-rename)
  "Rename the current node title and propagate changes
to links referencing current node.
1. Propagates changes to \"roam:\" links by updating the destination, and
2. For \"id:\" links -
If it is a raw ID link `[[id:uuid]]' add a description with the new node title
[[id:uuid][new-title]],
whereas for any standard ID link `[[id:uuid][old-title]]',
update to `[[id:uuid][new-title]],'
does not affect the description of non-standard IDs,
`[[id:uuid][custom-description]]'

When optional ALIAS-RENAME is NON-NIL,
rename an alias and propagate to \"roam:old-alias\" backlinks.
"
  (interactive "P")
  (when-let* ((original-buffer (current-buffer))
	      (node (org-roam-node-at-point))
	      (title (org-roam-node-title node))
	      (id (org-roam-node-id node))
	      (level (org-roam-node-level node))
	      (pos (org-roam-node-point node)))

    (let* (new-title
	   links
	   alias-pairs)  ; init a few local variables to be used downstream

	 (if alias-rename (progn
			 (let* ((aliases (or (org-entry-get pos "ROAM_ALIASES") ""))
				(alias-list (split-string aliases))
				(chosen-aliases (completing-read-multiple "Select aliases to rename (separate by comma): " alias-list))
				(updated-aliases aliases))
			   (setq alias-pairs (mapcar (lambda (old-alias)
						       (cons old-alias (read-from-minibuffer (format "Rename alias \"%s\" with: " old-alias))))
						     chosen-aliases))
			   (cond
			    ((string-empty-p aliases) (message "No aliases found for renaming."))
			    ((not (cl-every (lambda (alias) (member alias alias-list)) chosen-aliases))
			     (message "One or more chosen aliases not found in the list."))
			    (t (dolist (pair alias-pairs)
				 (let ((old-alias (car pair))
				       (new-alias (cdr pair)))
				   (setq updated-aliases
					 (replace-regexp-in-string (format "\\b%s\\b" (regexp-quote old-alias)) "" updated-aliases))
				   (when (string-empty-p new-alias)
				     (unless (y-or-n-p (format "You have chosen to delete alias %s. Continue?" old-alias))
				       (user-error "Aborted!")))
				   (setq updated-aliases (concat updated-aliases " " new-alias))))
			       (with-undo-amalgamate (org-entry-put pos "ROAM_ALIASES" (string-trim updated-aliases)))))
			   (setq links (org-roam-link-query-backlinks id title t t nil :aliases chosen-aliases))))

      (progn
	(setq new-title (read-from-minibuffer (format "Enter new node-title \n
[Currently \"%s\"]: " title)))
	(when (string= "" new-title)
	  (user-error "Warning! You have decided to delete current node-title and propagate changes.
Aborting! This is not allowed."))
	(if (= level 0)
	    (org-roam-set-keyword "TITLE" new-title)
	  (org-with-point-at pos
	    (org-edit-headline new-title)))
	(setq links (org-roam-link-query-backlinks id title nil nil t))))

    (write-file (buffer-file-name))

      (mapc (lambda (link)
	      (let ((ids (nth 0 link))
		    (roam-re (if alias-rename
				 (nth 3 link)
			       (nth 2 link))))
		(+org-roam-id-goto ids)
		(org-with-point-at 1
		  (while (re-search-forward org-link-bracket-re nil t)
		    (cond

		     ((and alias-rename
			   (seq-some
			    (lambda (re)
			      (string-match-p (concat "^roam:" re "$") (match-string 1)))
			    roam-re)
			   (y-or-n-p (format "Update %s? (Warning! answering no may disjoint this link!)" (match-string 1))))
		      (let* ((old-alias (substring (match-string 1) 5))  ;; Skip "roam:"
			     (new-alias (cdr (assoc old-alias alias-pairs))))
			(goto-char (match-beginning 1))
			(delete-region (match-beginning 1) (match-end 1))
			(insert (format "roam:%s" new-alias))))

		     ((and (not alias-rename)
			   (seq-some
			    (lambda (re)
			      (string-match-p (concat "^roam:" re "$") (match-string 1)))
			    roam-re)
			   (y-or-n-p (format "Update %s? (Warning! answering no may disjoint this link!)" (match-string 1))))
		      (goto-char (match-beginning 1))
		      (delete-region (match-beginning 1) (match-end 1))
		      (insert (format "roam:%s" new-title)))

		     ((and (not alias-rename)
			   (string-match-p (concat "^id:" id "$") (match-string 1)))
		      (when (and (not (match-string 2))
				 (y-or-n-p (format "Update Description of %s?" (match-string 1))))
			(goto-char (match-end 1))
			(forward-char 1)
			(insert (format "[%s]" new-title))
			(goto-char (match-beginning 0))
			(re-search-forward org-link-bracket-re nil t))
		      (when (and (match-string 2)
				 (string-match-p (concat "^" title "$") (match-string 2))
				 (y-or-n-p (format "Update Description of %s?" (match-string 1))))
			(goto-char (match-beginning 2))
			(delete-region (match-beginning 2) (match-end 2))
			(insert new-title))))))
		(write-file (buffer-file-name))))
	    links)
      ;; switch to our orignal-buffer
      (switch-to-buffer original-buffer))))

;; HHHH---------------------------------------------------


;; End
(provide 'org-roam-link-utils)
