;;; init.el -*- lexical-binding: t; -*-

(defvar initial-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun reset-file-handler-alist-h ()
  (setq file-name-handler-alist initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'reset-file-handler-alist-h)

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

(use-package delight
    :demand t)

(use-feature straight-x
  :commands (straight-x-fetch-all))

(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

(setq window-resize-pixelwise t
  frame-resize-pixelwise t)

(setq split-width-threshold 160
  split-height-threshold nil)

(setq indicate-buffer-boundaries nil
      indicate-empty-lines t)

(use-feature winner
  :demand t
  :config
  (winner-mode +1))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

(use-feature uniquify
  :demand t
  :init
  (setq uniquify-buffer-name-style 'forward))

(column-number-mode +1)

(setq enable-recursive-minibuffers t
      resize-mini-windows 'grow-only
      max-mini-window-height 0.15
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-feature savehist
  :demand t
  :init
  (setq savehist-file (concat cache-dir "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode +1))

(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-splash-screen t
      initial-scratch-message nil)

(use-feature tooltip
  :init
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1))
  (when is-linux
    (setq x-gtk-use-system-tooltips nil)))

(use-feature emacs
  :init
  (setq x-underline-at-descent-line t
        underline-minimum-offset 1)

  (set-face-attribute 'default nil :height 100)
  (add-to-list 'default-frame-alist `(font . "Fira Mono"))
  (set-face-attribute 'fixed-pitch nil :family 'unspecified))

(use-package emacs-color-theme-solarized
  :straight (:host github :repo "sellout/emacs-color-theme-solarized")
  :init
  (setq solarized-termcolor 256
        solarized-broken-srgb t
        solarized-contrast 'normal)

  (defun solarized-light ()
      (load-theme 'solarized t)
      (set-frame-parameter nil 'background-mode 'light)
      (enable-theme 'solarized))

  (defun solarized-dark ()
      (load-theme 'solarized t)
      (set-frame-parameter nil 'background-mode 'dark)
      (enable-theme 'solarized))

  (defun solarized-switch ()
      (interactive)
      (if (string= (frame-parameter nil 'background-mode) 'light)
          (solarized-dark)
        (solarized-light)))

  (solarized-light)
  :bind* (("C-c <f6>" . #'solarized-switch)))

(use-feature emacs
  :init
  (setq cursor-type 'box
        cursor-in-non-selected-windows 'hollow
        visible-cursor nil
        x-stretch-cursor nil)
  :config
  (blink-cursor-mode -1))

(use-feature paren
  :init
  (setq show-paren-style 'parenthesis
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        blink-matching-paren nil)
  (show-paren-mode 1))

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

(use-package magit
  :bind (("C-x g" . #'magit-status)
         ("C-x M-g" . #'magit-dispatch)
         ("C-c M-g" . #'magit-file-dispatch)))

(setq-default display-line-numbers-width 2
              display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; (defun switch-relative-absolute-linum ()
;;   (interactive)
;;   (if (string= (bound-and-true-p display-line-numbers) 'relative)
;;       (display-line-numbers 'relative)
;;     (display-line-numbers 1)))

;; (global-set-key (kbd "C-c <f7>") 'switch-relative-absolute-linum)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")

(use-feature windmove
  :demand t
  :config
  (windmove-default-keybindings)

  (when (fboundp 'windmove-display-default-keybindings)
    (windmove-display-default-keybindings))

  (when (fboundp 'windmove-delete-default-keybindings)
    (windmove-delete-default-keybindings)))

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

(use-package emacs
  :init
  (setq frame-title-format '("%b %& GNU Emacs")
        echo-keystrokes 1e-6
        find-file-visit-truename t
        vc-follow-symlinks t
        find-file-suppress-same-file-warnings t
        ring-bell-function #'ignore
        visible-bell t
        disabled-command-function nil
        confirm-kill-emacs #'y-or-n-p)
  (fset #'yes-or-no-p #'y-or-n-p))

(use-feature ibuffer
  :config
  (setq ibuffer-expert t
        ibuffer-use-other-window nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("Main"
           ("Directories" (mode . dired-mode))
           ("Org" (mode . org-mode))
           ("Programming" (mode . prog-mode))
           ("Markdown" (mode . markdown-mode))
           ("Emacs" (or
                     (name . "^\\*Help\\*$")
                     (name . "^\\*Custom.*")
                     (name . "^\\*Org Agenda\\*$")
                     (name . "^\\*info\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Backtrace\\*$")
                     (name . "^\\*Completion\\*$")
                     (name . "^\\*straight-process\\*$")
                     (name . "^\\*Messages\\*$"))))))
  :hook
  (ibuffer-mode . hl-line-mode)
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "Main")))
  :bind
  (([remap list-buffers] . #'ibuffer)))

(use-feature saveplace
  :demand t
  :init
  (setq save-place-file (concat cache-dir "saveplace")
        save-place-limit 100)
  :config
  (save-place-mode +1))

(use-package gcmh
  :straight (:host gitlab :repo "koral/gcmh")
  :demand t
  :delight
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

(use-feature emacs
  :init
  (setq-default tab-width 4
                tab-always-indent t
                indent-tabs-mode nil))

(use-feature delsel
  :demand t
  :config
  (delete-selection-mode +1))

(setq kill-do-not-save-duplicates t)

(setq mouse-yank-at-point t)

(use-feature isearch
  :config
  (setq lazy-highlight-initial-delay 0))

(use-feature emacs
  :init
  (setq auto-save-default nil
        create-lockfiles nil
        make-backup-files nil
        auto-save-list-file-name           (concat cache-dir "autosave")
        backup-directory-alist             `(("." . ,(concat cache-dir "backup/")))
        abbrev-file-name                   (concat local-dir "abbrev.el")
        async-byte-compile-log-file        (concat etc-dir "async-bytecomp.log")
        bookmark-default-file              (concat etc-dir "bookmarks")
        custom-file                        (concat local-dir "custom.el")
        custom-theme-directory             (concat local-dir "themes/")
        desktop-dirname                    (concat etc-dir "desktop")
        desktop-base-file-name             "autosave"
        desktop-base-lock-name             "autosave-lock"
        pcache-directory                   (concat cache-dir "pcache/")
        request-storage-directory          (concat cache-dir "request")
        server-auth-dir                    (concat cache-dir "server/")
        shared-game-score-directory        (concat etc-dir "shared-game-score/")
        tramp-auto-save-directory          (concat cache-dir "tramp-auto-save/")
        tramp-backup-directory-alist backup-directory-alist
        tramp-persistency-file-name        (concat cache-dir "tramp-persistency.el")
        url-cache-directory                (concat cache-dir "url/")
        url-configuration-directory        (concat etc-dir "url/")
        gamegrid-user-score-file-directory (concat etc-dir "games/")))

(use-feature autorevert
  :defer 2
  :delight
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode +1)
  (setq global-auto-revert-non-file-buffers t)
  (setq revert-without-query '(".*")))

(use-feature recentf
  :demand t
  :init
  (setq recentf-save-file (concat cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 10
        recentf-max-saved-items 100)
  :config
  (recentf-mode 1))

(use-feature dired
  :config
  (setq dired-auto-revert-buffer t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        delete-by-moving-to-trash t
        dired-listing-switches "-AFhv --color"
        dired-dwim-target t)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)))

(use-feature dired-aux
  :config
  (setq dired-isearch-filenames 'dwim
        dired-create-destination-dirs 'ask))

(use-feature wdired
  :after dired
  :commands wdired-change-to-wdired-mode
  :config
  (setq wdired-allow-to-change-permissions t
        wdired-create-parent-directories t)

  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))

  :bind (:map wdired-mode-map
              ([remap move-beginning-of-line] . #'dired-back-to-start-of-files)))

(use-feature dired-x
  :after dired
  :config
  (when-let (cmd (cond (is-mac "open")
                       (is-linux "xdg-open")
                       (is-windows "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-feature icomplete
  :init
  (fido-mode 1))

(use-feature org-indent
  :init
  (add-hook 'org-mode-hook #'org-indent-mode))
