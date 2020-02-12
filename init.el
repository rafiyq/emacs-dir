;;; init.el -*- lexical-binding: t; -*-

(defvar initial-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun reset-file-handler-alist-h ()
  (setq file-name-handler-alist initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'reset-file-handler-alist-h)

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(fset #'display-startup-echo-area-message #'ignore)

(defconst is-mac     (eq system-type 'darwin))
(defconst is-linux   (eq system-type 'gnu/linux))
(defconst is-windows (memq system-type '(cygwin windows-nt ms-dos)))

(defconst emacs-dir
  (eval-when-compile (file-truename user-emacs-directory)))

(defconst local-dir (concat emacs-dir "local/"))
(defconst etc-dir (concat local-dir "etc/"))
(defconst cache-dir (concat local-dir "cache/"))
(defconst site-lisp-dir (concat local-dir "site-lisp/"))

(setq straight-repository-branch "develop")

(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(check-on-save find-when-checking)))

(setq straight-recipe-overrides nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(use-feature straight-x
  :commands (straight-x-fetch-all))

(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.02)

(setq resize-mini-windows 'grow-only
      max-mini-window-height 0.15)

(fset #'yes-or-no-p #'y-or-n-p)

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq confirm-kill-emacs #'y-or-n-p)

(setq uniquify-buffer-name-style 'forward)

(setq window-resize-pixelwise t
  frame-resize-pixelwise t)

(setq window-divider-default-places t
  window-divider-default-bottom-width 1
  window-divider-default-right-width 1)

(setq split-width-threshold 160
  split-height-threshold nil)

(setq indicate-buffer-boundaries nil
  indicate-empty-lines nil)

(setq gnutls-verify-error (getenv "INSECURE")
  tls-checktrust gnutls-verify-error
  tls-program '("gnutls-cli --x509cafile %t -p %p %h"
		    ;; compatibility fallbacks
		    "gnutls-cli -p %p %h"
		    "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(setq auth-sources (list (expand-file-name "authinfo.gpg" etc-dir)
			 "~/.authinfo.gpg"))

(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))
  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 3072))

(setq ffap-machine-p-known 'reject)

(setq-default display-line-numbers-width 3)

(setq-default display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")

(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil
              fill-column 80)

(add-hook 'text-mode-hook #'auto-fill-mode)

(setq auto-mode-case-fold nil)

(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)

(when is-mac
  (setq mac-redisplay-dont-reset-vscroll t
	mac-mouse-wheel-smooth-scroll nil))

(setq fast-but-imprecise-scrolling t)

(setq apropos-do-all t)

(setq ring-bell-function #'ignore
      visible-bell nil)

(use-package gcmh
  :straight (:host gitlab :repo "koral/gcmh")
  :init
  (gcmh-mode 1))

(setq message-log-max 8192)

(setq ad-redefinition-action 'accept)

(setq idle-update-delay 1)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq frame-inhibit-implied-resize t)

(when is-windows
  (setq w32-get-true-file-attributes nil)
  (setq inhibit-compacting-font-caches t))

(unless is-mac   (setq command-line-ns-option-alist nil))
(unless is-linux (setq command-line-x-option-alist nil))

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(unless is-windows
  (setq selection-coding-system 'utf-8))

(setq-default tab-width 4
      tab-always-indent t
      indent-tabs-mode nil)

(setq kill-do-not-save-duplicates t)

(setq mouse-yank-at-point t)

(setq x-underline-at-descent-line t)

(straight-use-package 'doom-themes)
(use-package emacs-color-theme-solarized
  :straight (:host github :repo "sellout/emacs-color-theme-solarized")
  :init
  (setq solarized-termcolor 256
    solarized-broken-srgb t)
  (load-theme 'solarized t)
  (set-frame-parameter nil 'background-mode 'light)
  (enable-theme 'solarized))

(setq frame-title-format '("%b")
  icon-title-format frame-title-format)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when is-linux
  (setq x-gtk-use-system-tooltips nil))

(blink-cursor-mode -1)
(setq blink-matching-paren nil
      visible-cursor nil
      x-stretch-cursor nil)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq find-file-suppress-same-file-warnings t)

(setq delete-by-moving-to-trash is-mac)
(setq delete-by-moving-to-trash is-windows)

(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-list-file-name (concat cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat cache-dir "backup/"))))

(setq abbrev-file-name             (concat local-dir "abbrev.el")
  async-byte-compile-log-file  (concat etc-dir "async-bytecomp.log")
  bookmark-default-file        (concat etc-dir "bookmarks")
  custom-file                  (concat local-dir "custom.el")
  custom-theme-directory       (concat local-dir "themes/")
  desktop-dirname              (concat etc-dir "desktop")
  desktop-base-file-name       "autosave"
  desktop-base-lock-name       "autosave-lock"
  pcache-directory             (concat cache-dir "pcache/")
  request-storage-directory    (concat cache-dir "request")
  server-auth-dir              (concat cache-dir "server/")
  shared-game-score-directory  (concat etc-dir "shared-game-score/")
  tramp-auto-save-directory    (concat cache-dir "tramp-auto-save/")
  tramp-backup-directory-alist backup-directory-alist
  tramp-persistency-file-name  (concat cache-dir "tramp-persistency.el")
  url-cache-directory          (concat cache-dir "url/")
  url-configuration-directory  (concat etc-dir "url/")
  gamegrid-user-score-file-directory (concat etc-dir "games/"))
