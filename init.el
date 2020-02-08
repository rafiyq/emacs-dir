;; -*- lexical-binding: t -*-

;; Prevent package.el from modifying this file.
(setq package-enable-at-startup nil)

;; Prevent Custom from modifying this file.
(setq custom-file (expand-file-name
		   (format "custom-%d-%d.el" (emacs-pid) (random))
		   temporary-file-directory))


(setq file-name-handler-alist nil
      load-prefer-newer t
      stale-bytecode nil)
;(load
; (expand-file-name "radian.el" user-emacs-directory) nil 'nomessage 'nosuffix)


;;; Macro definition


(defvar finalize-init-hook nil
  "Hook run unconditionally after init, even if it fails.
Unlike `after-init-hook', this hook is run every time the
init-file is loaded, not just once.")

(defmacro def-hook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "No docstring provided for `def-hook'"))
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

;;; Startup optimizations

;; Disabling GC (by setting `gc-cons-threshold' to a very large value,
;; in this case 500MB) during startup is said to improve startup time
;; by reducing the number of GC runs.

(defvar default-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold'.")

(def-hook reset-gc-cons-threshold ()
  finalize-init-hook
  "Reset `gc-cons-threshold' to its original value.
Otherwise, Emacs will just get slower and slower over time."
  (setq gc-cons-threshold radian--orig-gc-cons-threshold))

(setq gc-cons-threshold (* 50 1000 1000))

;; After we enabled `load-prefer-newer' in init.el, disable it again
;; for the duration of init. Presumably, it slows things down, and we
;; shouldn't need it for anything but loading radian.el itself.
(setq load-prefer-newer nil)
