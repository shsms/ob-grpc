;;; ob-grpc.el --- Grpc evaluation functions for org-babel

;;; Code:
(require 'org)
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'js)

(add-to-list 'org-babel-tangle-lang-exts '("json"))

;;;###autoload
(defun ob-grpc-init ()
  "Initialize org file with the necessary parameters as properties."
  (interactive)

  (when (not (equal major-mode 'org-mode))
    (error "Must be called from org-mode"))

  (setq-local prop-defaults '(("GRPC-ENDPOINT" . "[::1]:8080")
			      ("PROTO-FILE" . "./proto/service.proto")
			      ("PROTO-IMPORT-PATH" . "proto")
			      ("PLAIN-TEXT" . "yes")
                              ("GRPC-BLOCK-PREFIX" . "* TODO ${method}\\n\\n# ${decl}\\n")))
  (let ((props (org-buffer-property-keys)))
    (mapcar
     (lambda (kvp)
       (let ((key (car kvp))
	     (value (cdr kvp)))
	 (unless (member key props)
	   (org-set-property key value))))
     prop-defaults)))


(defun ob-grpc--split-lines (string)
  "Split STRING at newline into a list, discard empty lines."
  (delete "" (split-string string "\n")))


(defun ob-grpc--concat-imports (import-paths)
  "Take a list of IMPORT-PATHS and return string of grpcurl cli arguments."
  (let (result)
    (dolist (item import-paths result)
      (setq result (concat " -import-path " item result)))
    result))


(defun ob-grpc--grpcurl-list (proto-file import-paths &optional service)
  "When no SERVICE is provided, return a list of grpc services defined \
in PROTO-FILE and IMPORT-PATHS.  Else return methods under SERVICE."
  (let ((command (message "grpcurl %s -proto %s list %s"
			  (ob-grpc--concat-imports import-paths)
			  proto-file
			  (if service service ""))))
    ;;TODO: update `shell-command-to-string' to check errno.
    (ob-grpc--split-lines (message (shell-command-to-string command)))))


(defun ob-grpc--grpcurl-describe (proto-file import-paths name &optional msg-template)
  "Return a `grpcurl describe' for NAME, defined in PROTO-FILE and IMPORT-PATHS."
  (let ((command (message "grpcurl %s %s -proto %s describe %s"
			  (ob-grpc--concat-imports import-paths)
			  (if msg-template " --msg-template " "")
			  proto-file
			  name)))
    (message (shell-command-to-string command))))


(defun ob-grpc--get-method-names ()
  "Return a list of grpc method names from grpcurl."
  (let* ((proto-file (org-entry-get nil "PROTO-FILE" t))
	 (import-paths (split-string (org-entry-get nil "PROTO-IMPORT-PATH" t) " "))
	 (service-names (ob-grpc--grpcurl-list proto-file import-paths))
	 (methods))
    (message "services: %s" service-names)
    (dolist (service-name service-names methods)
      (let (svc-methods)
	(dolist (method-name
		 (ob-grpc--grpcurl-list proto-file import-paths service-name)
		 svc-methods)
	  (setq svc-methods (cons method-name svc-methods)))
	(setq methods (append svc-methods methods))))
    methods))


(defun ob-grpc--grpcurl-msg-template (name)
  "Return the method signature and request msg template for method NAME, using grpcurl."
  (let* ((proto-file (org-entry-get nil "PROTO-FILE" t))
	 (import-paths (split-string (org-entry-get nil "PROTO-IMPORT-PATH" t) " "))
	 (method-description (ob-grpc--grpcurl-describe proto-file import-paths name))
	 (msg-template "uninitialized")
         (block-prefix nil))
    (message "%s" method-description)
    (save-match-data
      (and (string-match
	    "^rpc \\([A-Za-z_0-9]+\\) (\\( stream\\)? \\([a-zA-Z._0-9]+\\) ) returns (\\( stream\\)? \\([a-zA-Z._0-9]+\\) )"
	    method-description)
           (let ((decl (match-string 0 method-description))
                 (method-short (match-string 1 method-description))
		 (req-stream (match-string 2 method-description))
		 (req-message (match-string 3 method-description))
		 (resp-stream (match-string 4 method-description))
		 (resp-message (match-string 5 method-description))
                 (grpc-block-prefix (org-entry-get nil "GRPC-BLOCK-PREFIX" t)))
             (when grpc-block-prefix
               (setq block-prefix (replace-regexp-in-string
                                   (regexp-quote "${method-full}") name
                                   (replace-regexp-in-string
                                    (regexp-quote "${method}") method-short
                                    (replace-regexp-in-string
                                     (regexp-quote "${req-type}") (if req-stream
                                                                      (concat req-stream " " req-message)
                                                                    req-message)
                                     (replace-regexp-in-string
                                      (regexp-quote "${resp-type}") (if resp-stream
                                                                        (concat resp-stream " " resp-message)
                                                                      resp-message)
                                      (replace-regexp-in-string
                                       (regexp-quote "${decl}") decl
                                       (replace-regexp-in-string
                                        (regexp-quote "\\n") "\n"
                                        grpc-block-prefix))))))))
             (let ((msg-desc (ob-grpc--grpcurl-describe proto-file import-paths req-message t)))
	       (and (string-match "Message template:\n\\(.*\\(?:\n.*\\)*?\\)\\(?:\n\\'\\)"
				  msg-desc)
                    (setq msg-template (match-string 1 msg-desc)))))))
    (list block-prefix msg-template)))


;;;###autoload
(defun ob-grpc-insert-block (name)
  "Create a org babel source block for a method with the given NAME."
  (interactive
   (let* ((methods (ob-grpc--get-method-names)))
     (list (completing-read (format-prompt "Choose a method" (car methods))
			    methods nil nil nil nil methods))))
  (let ((msg-template (ob-grpc--grpcurl-msg-template name)))
    (org-insert-structure-template "src")
    (when (nth 0 msg-template)
      (save-excursion
        (beginning-of-line)
        (open-line 1)
        (insert (nth 0 msg-template))))
    (end-of-line)
    (insert (concat "grpc :method " name "\n"
		    (nth 1 msg-template)))))


;;;###autoload
(defun org-babel-execute:grpc (body params)
  "Execute a grpc call with BODY as request, using grpcurl, with method and endpoint taken from PARAMS."
  (let* ((proto-file (org-entry-get nil "PROTO-FILE" t))
	 (import-paths (split-string (org-entry-get nil "PROTO-IMPORT-PATH" t) " "))
	 (grpc-endpoint (org-entry-get nil "GRPC-ENDPOINT" t))
	 (plain-text (org-entry-get nil "PLAIN-TEXT" t))
         (method (alist-get :method params)))
    (shell-command-to-string
     (message "grpcurl %s -proto %s %s -d %s \"%s\" \"%s\""
	      (ob-grpc--concat-imports import-paths)
	      proto-file
	      (if (equal plain-text "no") "" "-plaintext")
	      (prin1-to-string body)
	      grpc-endpoint
	      method
	      ))))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("grpc" . "js")))

(provide 'ob-grpc)
;;; ob-grpc.el ends here
