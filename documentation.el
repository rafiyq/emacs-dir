(defvar init-file-loaded-p nil)

(defvar minimum-emacs-version "25.2")

(defvar finalize-init-hook nil)

(defmacro protect-macros (&rest body)
  (declare (indent 0))
  `(eval '(progn ,@body)))

(defmacro flet (bindings &rest body)
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
		 (lambda (binding)
		   (when (memq (car binding) '(defun lambda))
		     (setq binding (cdr binding)))
		   (cl-destructuring-bind (name arglist &rest body) binding
		     (list
		      `(,name (symbol-function #',name))
		      `((symbol-function #',name)
			(lambda ,arglist
			  ,@body)))))
		 bindings))
     ,@body))

(defmacro defadvice (name arglist where place docstring &rest body)
  (declare (indent 2)
	   (doc-string 5))
  (unless (stringp docstring)
    (error "Advice `%S' not documented'" name))
  (unless (and (listp place)
	       (= 2 (length place))
	       (eq (nth 0 place) 'function)
	       (symbolp (nth 1 place)))
    (error "Advice `%S' does not sharp-quote place `%S'" name place))
  `(progn

(defun ,name ,arglist
  ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
		      "an"
		    "a")))
     (format "%s\n\nThis is %s `%S' advice for `%S'."
	     docstring article where
	     (if (and (listp place)
		      (memq (car place) ''function))
		 (cadr place)
	       place)))
  ,@body)
(eval-when-compile
  (declare-function ,name nil))
(advice-add ,place ',where #',name)
',name))

(defmacro defhook (name arglist hooks docstring &rest body)
  (declare (indent 2)
	   (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "No docstring provided for `defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
	 ,(format "%s\n\nThis function is for use in %s."
		  docstring hooks-str)
	 ,@body)
       (dolist (hook ',hooks)
	 (add-hook hook ',name)))))

(defmacro radian-operating-system-p (os)
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
			'(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro radian-with-operating-system (os &rest body)
  (declare (indent 1))
  `(when (operating-system-p ,os)
     ,@body))

(defmacro if-compile-time (cond then else)
  (declare (indent 2))
  (if (eval cond)
      then
    else))

(defmacro radian-when-compiletime (cond &rest body)
  (declare (indent 1))
  (when (eval cond)
    `(progn ,@body)))

(defun radian-managed-p (filename)
  (let ((truename (file-truename filename)))
    (string-prefix-p radian-directory truename
		     (when (if (fboundp 'file-name-case-insensitive-p)
			       (file-name-case-insensitive-p truename)
			     (with-operating-system macOS
			       t))
		       'ignore-case))))

(defmacro with-silent-load (&rest body)
  (declare (indent 0))
  `(flet ((defun load (file &optional noerror _nomessage &rest args)
	    (apply load file noerror 'nomessage args)))
	 ,@body))

(defmacro with-silent-write (&rest body)
  (declare (indent 0))
  `(flet ((defun write-region
	      (start end filename &optional append visit lockname
		     mustbenew)
	    (funcall write-region start end filename append 0
		     lockname mustbenew)
	    (when (or (stringp visit) (eq visit t))
	      (setq buffer-file-name
		    (if (stringp visit)
			visit
		      filename))
	      (set-visited-file-modtime)
	      (set-buffer-modified-p nil))))
	 (cl-letf (((symbol-function #'message) #'ignore))
	   ,@body)))

(defmacro with-silent-message (regexps &rest body)
  (declare (indent 1))
  (let ((regexps-sym (cl-gensym "regexps")))
    `(let ((,regexps-sym ,regexps))
       (when (stringp ,regexps-sym)
	 (setq ,regexps-sym (list ,regexps-sym)))
       (radian-flet ((defun message (format &rest args)
		       (let ((str (apply #'format format args)))

(cl-block done
		  (dolist (regexp ,regexps-sym)
		    (when (or (null regexp)
			      (string-match-p regexp str))
		      (cl-return-from done)))
		  (funcall message "%s" str)))))
,@body))))

(defun advice-silence-messages (func &rest args)
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))

(defun random-string ()
  (md5 (format "%s%s%s%s"
	       (system-name) (emacs-pid) (current-time) (random))))

(defun list-of-strings-p (obj)
  (and (listp obj)
       (cl-every #'stringp obj)))

(defun radian--path-join (path &rest segments)
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

(defcustom before-straight-hook nil
  :group 'radian-hooks
  :type 'hook)

(defcustom after-init-hook nil
  :group 'radian-hooks
  :type 'hook)

(defvar hook-contents nil)

(defvar straight-current-profile)

(defmacro radian--load-local-init-file ()
  (if byte-compile-current-file
      (let ((forms nil))
	(with-temp-buffer
	  (ignore-errors
	    ;; Can't do this literally because it breaks Unicode
	    ;; characters.
	    (insert-file-contents radian-local-init-file))
	  (condition-case _
	      (while t
		(let ((form (read (current-buffer))))
		  (if (and (listp form)
			   (eq (nth 0 form) #'radian-local-on-hook)
			   (nth 1 form)
			   (symbolp (nth 1 form))
			   (nthcdr 2 form))
		      (let* ((name (nth 1 form))
			     (body (nthcdr 2 form))
			     (hook (intern (format "%S-hook" name)))
			     (link (assq hook hook-contents)))
			(unless link
			  (setq link (cons hook nil))
			  (push link radian--hook-contents))
			(dolist (subform body)
			  (push subform (cdr link))))
		    (push form forms))))
	    (end-of-file)))
	(setq forms (nreverse forms))
	(dolist (link hook-contents)
	  (setf (cdr link)
		(nreverse (cdr link))))
	`(progn ,@forms))
    `(load radian-local-init-file 'noerror 'nomessage)))

(defmacro local-on-hook (name &rest body)
  (declare (indent 1))
  (let ((func-name (intern (format "local--%S" name)))
	(hook (intern (format "%S-hook" name))))
    `(progn
       (defhook ,func-name ()
	 ,hook
	 "Automatically-generated local hook function."
	 (protect-macros
	   ,@body)))))

(defmacro run-hook (name)
  (declare (indent 0))
  (let ((hook (intern (format "%S-hook" name))))
    `(let ((straight-current-profile 'radian-local))
       (run-hooks ',hook)
       ,@(when byte-compile-current-file
	   (alist-get hook hook-contents)))))
