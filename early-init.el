;;; early-init.el -*- lexical-binding: t; -*-

(defvar init-gc-cons-threshold gc-cons-threshold)
(defvar init-gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
  gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq init-gc-cons-threshold
                  init-gc-cons-percentage)))

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(advice-add #'x-apply-session-resources :override #'ignore)

(load
 (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix)
