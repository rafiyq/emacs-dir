;;; init.el -*- lexical-binding: t; -*-

;; Max the gc threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;;(let (file-name-handler-alist)
  ;; Ensure Doom is running out of this file's directory
;;  (setq user-emacs-directory (file-name-directory load-file-name)))


(defvar initial-file-name-handler-alist file-name-handler-alist)

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(setq file-name-handler-alist nil)

;; Restore `file-name-handler-alist', because it is needed for handling
;; encrypted or compressed files, among other things.
(defun reset-file-handler-alist-h ()
  (setq file-name-handler-alist initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'reset-file-handler-alist-h)


;; What system are emacs on
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


;;; Directories/files

;; The path to the currently loaded .emacs.d directory.
(defconst emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))

;; Storage location for system's installation of Emacs.
(defconst local-dir (concat emacs-dir "local/"))

;; Directory for non-volatile local storage,
;; files that don't change much, like server binaries, external
;; dependencies or long-term shared data.
(defconst etc-dir (concat local-dir "etc/"))

;; Directory for volatile local storage,
;; for files that change often, like cache files.
(defconst cache-dir (concat local-dir "cache/"))

;; Directory for local installation packages
(defconst site-lisp-dir (concat local-dir "site-lisp/"))


;;-------------------------------------------------------------------------;
;; CORE                                                                    ;
;;-------------------------------------------------------------------------;

;; longer logs ahoy, reliably locate lapses
(setq message-log-max 8192)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error doom-debug-mode
      jka-compr-verbose doom-debug-mode)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
;; Except for the clipboard on Windows, where its contents could be in an
;; encoding that's wider than utf-8, let Emacs/the OS decide what encoding
;; to use.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Disable warnings from legacy advice system. They aren't useful, and we can't
;; often do anything about them besides changing packages upstream
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

;; Don't make a second case-insensitive pass over `auto-mode-alist'. If it has
;; to, it's our (the user's) failure. One case for all!
(setq auto-mode-case-fold nil)

;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;; Emacs is a huge security vulnerability, what with all the dependencies it
;; pulls in from all corners of the globe. Let's at least try to be more
;; discerning.
(setq gnutls-verify-error (getenv "INSECURE")
      tls-checktrust gnutls-verify-error
      tls-program '("gnutls-cli --x509cafile %t -p %p %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"
                    "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; Emacs stores authinfo in HOME and in plaintext. Let's not do that, mkay? This
;; file usually stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
(setq auth-sources (list (expand-file-name "authinfo.gpg" doom-etc-dir)
                         "~/.authinfo.gpg"))

;; Emacs on Windows frequently confuses HOME (C:\Users\<NAME>) and APPDATA,
;; causing `abbreviate-home-dir' to produce incorrect paths.
(when IS-WINDOWS
  (setq abbreviated-home-dir "\\`'"))

;; Don't litter `doom-emacs-dir'. We don't use `no-littering' because it's a
;; mote too opinionated for our needs.
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


;;; Optimizations

;; Disable bidirectional text rendering for a modest performance boost.
;; this renders Emacs unable to detect/display right-to-left languages
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved.
(when IS-WINDOWS
  ;; Reduce the workload when doing file IO
  (setq w32-get-true-file-attributes nil)

  ;; Font compacting can be terribly expensive, especially for rendering icon
  ;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
  ;; been determined.
  (setq inhibit-compacting-font-caches t))

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash IS-MAC)



;;-------------------------------------------------------------------------;
;; UI                                                                      ;
;;-------------------------------------------------------------------------;

;;; General UX

;; Simpler confirmation prompt when killing Emacs
(setq confirm-kill-emacs #'y-or-n-p)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)


;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(when IS-MAC
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))


;;; Cursor

;; Don't blink the cursor.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point.
(setq blink-matching-paren nil)

(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)


;;; Fringes

;; Reduce the clutter in the fringes; reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; remove continuation arrow on right fringe
(delq! 'continuation fringe-indicator-alist 'assq)


;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; always avoid GUI
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; ...especially on linux
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)


;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; Line numbers

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)


;;; Theme & font

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)



;;-------------------------------------------------------------------------;
;; EDITING                                                                 ;
;;-------------------------------------------------------------------------;

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Don't autosave files or create lock/history/backup files.
;; But have a place to store them.
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-list-file-name (concat cache-dir "autosave")
      backup-directory-alist `(("." . ,(concat cache-dir "backup/"))))


;;; Formatting

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")  ; for :retab

;; Favor hard-wrapping in text modes
(add-hook 'text-mode-hook #'auto-fill-mode)


;;; Clipboard / kill-ring

;; Eliminate duplicates in the kill ring.
(setq kill-do-not-save-duplicates t)


;;; Extra file extensions to support

(push '("/LICENSE\\'" . text-mode) auto-mode-alist)
(push '("\\.log\\'" . text-mode) auto-mode-alist)
(push '("\\.env\\'" . sh-mode) auto-mode-alist)


;;
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))