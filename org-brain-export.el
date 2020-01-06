;;; org-brain-export.el --- Export org-brain to other formats  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain-export
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25.1") (org-brain "0.9") (a "0.1.1") (xmlgen "0.5"))
;; Version: 0.1

;;; Commentary:

;; Package for exporting `org-brain' data to other formats.

;;; Code:

(require 'org-brain)
(require 'a)
(require 'xmlgen)

;;; General

(defvar org-brain-export-directory (expand-file-name "export" org-brain-path)
  "Directory where exported files will end up.")

(defun org-brain-export--relation-data (from-entry to-entry)
  "Get relationship data FROM-ENTRY TO-ENTRY.
TO-ENTRY is the main entry, while TO-ENTRY is the relationship entry."
  (a-list :id (org-brain-entry-identifier to-entry)
          :annotation (org-brain-get-edge-annotation from-entry to-entry)
          :title (org-brain-title to-entry)))

(defun org-brain-export-generate-data (entry)
  "Generate data representation of `org-brain' ENTRY.
Represented as an alist."
  (a-list
   :id (org-brain-entry-identifier entry)
   :type (if (org-brain-filep entry) 'file 'headline)
   :title (org-brain-title entry)
   :text (org-brain-text entry)
   :children (mapcar (lambda (child)
                       (org-brain-export--relation-data entry child))
                     (org-brain-children entry))
   :parents (mapcar (lambda (parent)
                      (org-brain-export--relation-data entry parent))
                    (org-brain-parents entry))
   :friends (mapcar (lambda (friend)
                      (org-brain-export--relation-data entry friend))
                    (org-brain-friends entry))))

;;; GraphViz

(defvar org-brain-export-dot-file (expand-file-name "brain.dot" org-brain-export-directory)
  "Where the output of `org-brain-export-dot' goes.")

(defun org-brain-export--dot-id (ob-data)
  "Convert id of OB-DATA into a dot readable format."
  (concat "ob"
          (replace-regexp-in-string
           "[-/]" "" (alist-get :id ob-data))))

(defun org-brain-export--dot-node-def (ob-data)
  "Get node entry line (a string) of OB-DATA."
  (format "%s [label=\"%s\"];\n"
          (org-brain-export--dot-id ob-data)
          (alist-get :title ob-data)))

(defun org-brain-export--dot-children (ob-data)
  "Get children relations in OB-DATA as a dot string."
  (mapconcat
   (lambda (child)
     (format "%s -> %s;\n"
             (org-brain-export--dot-id ob-data)
             (org-brain-export--dot-id child)))
   (alist-get :children ob-data)
   ""))

(defun org-brain-export--dot-friends (ob-data)
  "Get friend relations in OB-DATA as a dot string."
  (mapconcat
   (lambda (child)
     (format "%s -> %s [dir=none color=\"blue\"];\n"
             (org-brain-export--dot-id ob-data)
             (org-brain-export--dot-id child)))
   (alist-get :friends ob-data)
   ""))

(defun org-brain-export-dot ()
  "Export your `org-brain' to Graphviz dot format.
Saved at `org-brain-export-dot-file'."
  (interactive)
  (make-directory (file-name-directory org-brain-export-dot-file) t)
  (message "Starting dot export...")
  (let ((data (mapcar #'org-brain-export-generate-data
                      (append (org-brain-files t)
                              (org-brain-headline-entries)))))
    (with-temp-file org-brain-export-dot-file
      (insert "digraph G {\nconcentrate=true;\n")
      (mapc (lambda (x) (insert (org-brain-export--dot-node-def x)))
            data)
      (mapc (lambda (x) (insert (org-brain-export--dot-children x)))
            data)
      (mapc (lambda (x) (insert (org-brain-export--dot-friends x)))
            data)
      (insert "}")))
  (message "Dot export finished!"))

;;; HTML

(defvar org-brain-export-html-file (expand-file-name "brain.html" org-brain-export-directory)
  "Where the output of `org-brain-export-html' goes.")

(defun org-brain-export--html-relation-list-item (relation)
  "Generate the HTML <li> to RELATION."
  `(li ,@(remove nil (list
                      `(a :href ,(concat "#" (alist-get :id relation)) ,(alist-get :title relation))
                      (when-let ((annotation (alist-get :annotation relation)))
                        '(span :class "ob-edge" annotation))))))

(defun org-brain-export--html-parents (ob-data)
  "Generate HTML representation of parents in OB-DATA."
  (when-let ((parents (alist-get :parents ob-data)))
    `(section :class "ob-parents"
       (h2 "Parents")
       (ul :class "ob-parent-list"
         ,@(mapcar #'org-brain-export--html-relation-list-item parents)))))

(defun org-brain-export--html-friends (ob-data)
  "Generate HTML representation of parents in OB-DATA."
  (when-let ((friends (alist-get :friends ob-data)))
    `(section :class "ob-friends"
       (h2 "Friends")
       (ul :class "ob-friend-list"
         ,@(mapcar #'org-brain-export--html-relation-list-item friends)))))

(defun org-brain-export--html-children (ob-data)
  "Generate HTML representation of children in OB-DATA."
  (when-let ((children (alist-get :children ob-data)))
    `(section :class "ob-children"
       (h2 "Children")
       (ul :class "ob-children-list"
         ,@(mapcar #'org-brain-export--html-relation-list-item children)))))

(defun org-brain-export--generate-html (ob-data)
  "Generate HTML representation of OB-DATA."
  `(article :class "ob-entry" :id ,(alist-get :id ob-data)
     ,@(remove nil (list
                    `(h1 ,(alist-get :title ob-data))
                    (org-brain-export--html-parents ob-data)
                    (org-brain-export--html-children ob-data)
                    (org-brain-export--html-friends ob-data)
                    ;; The %s here is sent to format during
                    ;; org-brain-export-html, since raw HTML doesn't play well
                    ;; with xmlgen
                    `(section :class "ob-entry-text" "%s")))))

(defun org-brain-export-html ()
  "Export your `org-brain' to HTML."
  (interactive)
  (make-directory (file-name-directory org-brain-export-html-file) t)
  (message "Starting HTML export...")
  (let ((data-rep (mapcar #'org-brain-export-generate-data
                          (append (org-brain-files t)
                                  (org-brain-headline-entries)))))
    (with-temp-file org-brain-export-html-file
      (insert
       (apply #'format
              (xmlgen
               `(html
                 (head)
                 (body
                  ,@(mapcar #'org-brain-export--generate-html data-rep))))
              (mapcar (lambda (x)
                        (or (ignore-errors (org-export-string-as (alist-get :text x) 'html t))
                            "<i>Error during parsing of entry text...</i>"))
                      data-rep)))))
  (message "HTML export finished!"))

(provide 'org-brain-export)
;;; org-brain-export.el ends here
