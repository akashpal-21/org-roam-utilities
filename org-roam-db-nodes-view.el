;; org-roam-db-nodes.view.el

(require 'org-roam-db)

(setq org-roam-db-gc-threshold (* 2 8 1024 1024))

(defconst org-roam-db--nodes-view-schemata
  '((nodes-view
     [(nview-id :primary-key)
     (file :not-null)
     level
     pos
     todo
     priority
     scheduled
     deadline
     title
     properties
     olp
     tag
     alias
     type_ref]
     (:foreign-key [file] :references files [file] :on-delete :cascade))))

(defconst org-roam-db--nodes-view-index
  '((nodes-view-file nodes-view [file])))

(defconst org-roam-db--nodes-view-populate
  "insert into nodes_view (
 nview_id, file, level, pos, todo,
 priority, scheduled, deadline, title, properties,
 olp, tag, alias, type_ref)

 select
  id, file, \"level\", pos, todo,
  priority, scheduled, deadline, title, properties,
  olp, '(' || group_concat(tags, ' ') || ')' as tags, aliases, refs

  from
  (select
   id, file, \"level\", pos, todo,
   priority , scheduled , deadline , title, properties,
   olp, tags, '(' || group_concat(aliases, ' ') || ')' as aliases, refs

   from
   (select
    nodes.id as id, nodes.file as file, nodes.\"level\" as \"level\", nodes.pos as pos, nodes.todo as todo,
    nodes.priority as priority, nodes.scheduled as scheduled, nodes.deadline as deadline, nodes.title as title, nodes.properties as properties,
    nodes.olp as olp, tags.tag as tags, aliases.alias as aliases, '(' || group_concat(rtrim (refs.\"type\", '\"') || ':' || ltrim(refs.ref, '\"'), ' ') || ')' as refs

    from nodes
    left join tags on tags.node_id = nodes.id
    left join aliases on aliases.node_id = nodes.id
    left join refs on refs.node_id = nodes.id
    group by nodes.id, tags.tag, aliases.alias)

   group by id, tags)
 group by id;")

(defconst org-roam-db--nodes-view-triggers
  '((nodes
     (nodes-create
      "create trigger insert_node_trigger
	after insert on nodes
	begin
	insert into nodes_view (nview_id, file, level, pos, todo, priority, scheduled, deadline, title, properties, olp)
	values (new.id, new.file, new.level, new.pos, new.todo, new.priority, new.scheduled, new.deadline, new.title, new.properties, new.olp);
	end;")
     (nodes-update
      "create trigger update_node_trigger
	after update on nodes
	begin
	update nodes_view
	set file = new.file,
	    level = new.level,
	    pos = new.pos,
	    todo = new.todo,
	    priority = new.priority,
	    scheduled = new.scheduled,
	    deadline = new.deadline,
	    title = new.title,
	    properties = new.properties,
	    olp = new.olp
	where nview_id = old.id;
	end;")
     (nodes-delete
      "create trigger delete_node_trigger
	after delete on nodes
	begin
	delete from nodes_view
	where nview_id = old.id;
	end;"))
    (tags
     (tags-insert
      "create trigger insert_tag_trigger
	after insert on tags
	begin
	update nodes_view
	set tag = (select '(' || group_concat(tags.tag, ' ') || ')' from tags where node_id = new.node_id)
	where nview_id = new.node_id;
	end;")
     (tags-update
      "create trigger update_tag_trigger
	after update on tags
	begin
	update nodes_view
	set tag = (select '(' || group_concat(tags.tag, ' ') || ')' from tags where node_id = old.node_id)
	where nview_id = old.node_id;
	end;")
     (tags-delete
      "create trigger delete_tag_trigger
	after delete on tags
	begin
	update nodes_view
	set tag = (select '(' || group_concat(tags.tag, ' ') || ')' from tags where node_id = old.node_id)
	where nview_id = old.node_id;
	end;"))
    (aliases
     (aliases-insert
      "create trigger insert_alias_trigger
	after insert on aliases
	begin
	update nodes_view
	set alias = (select '(' || group_concat(aliases.alias, ' ') || ')' from aliases where node_id = new.node_id)
	where nview_id = new.node_id;
	end;")
     (aliases-update
      "create trigger update_alias_trigger
	after update on aliases
	begin
	update nodes_view
	set alias = (select '(' || group_concat(aliases.alias, ' ') || ')' from aliases where node_id = old.node_id)
	where nview_id = old.node_id;
	end;")
     (aliases-delete
      "create trigger delete_alias_trigger
	after delete on aliases
	begin
	update nodes_view
	set alias = (select '(' || group_concat(aliases.alias, ' ') || ')' from aliases where node_id = old.node_id)
	where nview_id = old.node_id;
	end;"))
    (refs
     (refs-insert
      "create trigger insert_ref_trigger
	after insert on refs
	begin
	update nodes_view
	set type_ref = (select '(' || group_concat(rtrim (refs.\"type\", '\"') || ':' || ltrim(refs.ref, '\"'), ' ') || ')' from refs where node_id = new.node_id)
	where nview_id = new.node_id;
	end;")
     (refs-update
      "create trigger update_ref_trigger
	after update on refs
	begin
	update nodes_view
	set type_ref = (select '(' || group_concat(rtrim (refs.\"type\", '\"') || ':' || ltrim(refs.ref, '\"'), ' ') || ')' from refs where node_id = old.node_id)
	where nview_id = old.node_id;
	end;")
     (refs-delete
      "create trigger delete_ref_trigger
	after delete on refs
	begin
	update nodes_view
	set type_ref = (select '(' || group_concat(rtrim (refs.\"type\", '\"') || ':' || ltrim(refs.ref, '\"'), ' ') || ')' from refs where node_id = old.node_id)
	where nview_id = old.node_id;
	end;")))
  "Mapping of database tables to their corresponding SQL trigger definitions.")

(defun org-roam-db--init-nodes-view (db)
  (emacsql-with-transaction db
    (pcase-dolist (`(,table ,schema) org-roam-db--nodes-view-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (pcase-dolist (`(,index-name ,table ,columns) org-roam-db--nodes-view-index)
      (emacsql db [:create-index $i1 :on $i2 $S3] index-name table columns))
    (emacsql db org-roam-db--nodes-view-populate)
    (pcase-dolist (`(,table . ,triggers) org-roam-db--nodes-view-triggers)
      (dolist (trigger triggers)
	(emacsql (org-roam-db) (cadr trigger)))))
  ;; defrag the db
  (emacsql db "vacuum;"))

;; Initialise org-roam-db-nodes-view after initialising org-roam.db
(advice-add 'org-roam-db--init :after #'org-roam-db--init-nodes-view)

(provide 'org-roam-db-nodes-view)
