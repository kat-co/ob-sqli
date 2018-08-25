(require 'ob)
(require 'ob-sql)
(require 'ob-sqlite)

;; Functions defined in org-mode
(declare-function org-fill-template "org" (template alist))
(declare-function org-table-convert-region "org-table"
		  (beg0 end0 &optional separator))
(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))


(defvar org-babel-default-header-args:sqli '())

(defvar org-babel-header-args:sqli
  '((sqli-buffer . :any)))

(defun org-babel-expand-body:sqli (body params)
  "Expand `body' according to the values of `params'."
  )

(defun org-babel-execute:sqli (body params)
  "Executes a block of SQL in the given SQLi buffer.

This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
        (sqli-buffer-name (cdr (assq :sqli-buffer params)))
        (separator (cdr (assq :separator params)))
        (headers-p (equal "yes" (cdr (assq :colnames params))))
        (others (delq nil (mapcar
			   (lambda (arg) (car (assq arg params)))
			   (list :header :echo :bail :column
				 :csv :html :line :list)))))
    (unless sqli-buffer-name (error "ob-sqli: cannot evaluate without a SQLi buffer"))
    (let ((sqli-buffer (get-buffer sqli-buffer-name)))
      (unless sqli-buffer (error "ob-sqli: cannot find the given SQLi buffer: %s" sqli-buffer-name))
      ;; Attempt #3
      (with-temp-buffer
        (sql-redirect sqli-buffer
                      ;; `sql-redirect' does not seem to understand
                      ;; how to redirect a multi-line BODY. Strip out
                      ;; the newlines to make its job a bit easier.
                      (org-babel-expand-body:sql (replace-regexp-in-string "\n" " " body) params)
                      (buffer-name (current-buffer)))
        (org-babel-result-cond result-params
          (buffer-string)
          ;;
          (unless (string= "" (buffer-string))
	    (org-table-convert-region (point-min) (point-max)
				      (if (or (member :csv others)
					      (member :column others)
					      (member :line others)
					      (member :list others)
					      (member :html others)
                                              separator)
					  separator
				        '(4)))
	    (org-babel-sqlite-table-or-scalar
	     (org-babel-sqlite-offset-colnames
	      (org-table-to-lisp) headers-p))))))))

(defun ob-sqli--sqli-block-p (params)
  "Determines whether a SQL code block is intended to be run by SQLi."
  (assq :sqli-buffer params))

(defun ob-sqli--sqli-sentinel (body params)
  "Determines whether to handle org-babel execution, or forward the call to ob-sql.

This function is called by `org-babel-execute-src-block' when C-c
C-c is called on a SQL code block. This is because of the call to
`advice-add' below. This function will decide whether the SQL
block is intended to be run by ob-sql or ob-sqli, and forward the call on accordingly."
  (if (ob-sqli--sqli-block-p params)
      (org-babel-execute:sqli body params)
    (org-babel-execute:sql body params)))

;; We want code blocks intended to be run in SQLi buffers to be syntax highlighted, so we don't want to declare our own sqli mode
(advice-add #'org-babel-execute:sql :override #'ob-sqli--sqli-sentinel)

(provide 'ob-sqli)
