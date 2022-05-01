;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   ██████╗ ███╗   ██╗██╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝ ████╗  ██║██║   ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  ██║  ███╗██╔██╗ ██║██║   ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██║   ██║██║╚██╗██║██║   ██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ╚██████╔╝██║ ╚████║╚██████╔╝    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;   ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝     ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: the font above is ANSI Shadow


;; =========================
;; ====== WHO'S THAT? ======
;; =========================
;; Riccardo Mura created and uses this configuration file


;; =======================
;; ====== RESOURCES ======
;; =======================
;; Some resources that helped in building this configuration
;;;; https://www.emacswiki.org/
;;;;;; You should consult too!
;;;; https://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs
;;;;;; Many thanks to Brent Longborough!
;;;; Doom Emacs FAQs (https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org)
;;;;;; It is often said that Doom Emac's startup is fast. And it's true!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ===============================
;; ====== MACRO DEFINITIONS ======
;; ===============================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro bind-with-args (f &rest args)
  `(lambda ()
     (interactive)
     (,f ,@args)))

(defmacro global-set-key-extended (key command &rest args)
  `(global-set-key ,key (lambda () (interactive) (,command ,@args))))

(defmacro setvar (name &rest args)
  `(if (not (boundp ',name))
       (defvar ,name ,@args)
     (setq ,name ,@args)))


;; ==========================================
;; ====== GLOBAL CONSTANTS AND ALIASES ======
;; ==========================================

;; ====== GARBAGE COLLECTOR ======
;; NOTE:: This is changed at the end of the config
(setq gc-cons-threshold (* 512 1024 1024))
(setq gc-cons-percentage 0.6)

;; ====== GENERIC ======
(if (version<= emacs-version "24.4")
    (dexfalias 'with-eval-after-load 'eval-after-load)
  nil)

;; ====== FONTS ======
;(setvar font-linux "Source Code Pro")
(setvar font-linux "Cascadia Code")
;(setvar font-linux-fallback "DejaVu Sans Mono")
(setvar font-linux-fallback "Source Code Pro")
(setvar font-darwin "Monaco")
(setvar font-darwin-fallback "Menlo")
(setvar font-windows "Consolas")
(setvar font-windows-fallback "Lucida Console")
(set-face-attribute 'default nil :height 110)

;; ====== COLOR THEME ======
;(setvar theme-dark 'monokai-pro-ristretto)
;(setq theme-light 'sanityinc-tomorrow-day)
;(setvar theme-dark 'nord)
;(setvar theme-light 'gruvbox-light-medium)
 ;(setvar theme-light 'solarized-light)
(setvar theme-light 'one-light)
;(setvar theme-dark 'sanityinc-tomorrow-eighties)
;(setvar theme-dark 'monokai-pro-machine)
;(setvar theme-dark 'zenburn)
(setvar theme-dark 'creamsody)

;; ====== GEOMETRY ======
(setvar geometry-width 110)
(setvar geometry-height 40)
(setvar geometry-margin-top 70)
(setvar geometry-margin-left 140)


;; ====== DOC VIEW ======
(setvar doc-view-resolution 400)


;; ====== C / C++ ======
(setvar c-default-style "stroustrup")

;; ====== LISP / SCHEME ======
(defvar chicken-startup-lib "~/fluent-assistant/portable-core.scm"
  "Path to the library to be loaded when running CHICKEN Scheme via geiser.")
(add-hook 'scheme-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; ====== PRETTIFY-SYMBOLS ======
(add-hook 'scheme-mode-hook 'prettify-symbols-mode)

;; ====== PERFORMANCE WITH LONG LINES ======
(if (>= emacs-major-version 27)
    (progn
      (global-so-long-mode 1)
      (setq bidi-inhibit-bpa t)
      (setq bidi-paragraph-direction 'left-to-right))
  nil)

;; ====== AVOID YASNIPPET LOADING AT STARTUP ======
;;(setq package-load-list '((yasnippet nil) all))

(add-hook 'server-switch-hook #'raise-frame)
;(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

(setq custom-file (concat user-emacs-directory "/custom.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ==================================
;; ====== FUNCTION DEFINITIONS ======
;; ==================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-string-list (source-list pattern)
  (cl-remove-if-not
   (lambda (item)
     (string-match item pattern))
   source-list))

(defun OTF (command name)
  "execute a process in a new terminal session On The Fly
or switch to the buffer if it already exists"
  (interactive)
  (if (get-buffer name)
      (switch-to-buffer name)
    (if (string= 'command "") ;; Is there any command to be executed?
	(progn
	  (multi-vterm)
	  (rename-buffer name))
      (progn
	(multi-vterm)
	(rename-buffer name)
	(vterm-send-string command)
	(execute-kbd-macro (kbd "<return>"))))))

;;;; SEE:
;; https://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun load-dark-theme nil
  (interactive)
  (progn
    (disable-theme theme-light)
    (load-theme theme-dark t)))

(defun load-light-theme nil
  (interactive)
  (progn
    (disable-theme theme-dark)
    (load-theme theme-light t)))

(defun platform? (os-family)
  "Returns t if the string matches the platform (OS) in use, nil otherwise."
  (string= os-family system-type))

(defun gnu/linux? ()
  "Returns t if a GNU/Linux distribution is in use, nil otherwise."
  (platform? "gnu/linux"))

(defun darwin? ()
  "Returns t if OS X is in use, nil otherwise."
  (platform? "darwin"))

(defun windows-nt? ()
  "Returns t if Microsoft Windows is in use, nil otherwise."
  (platform? "windows-nt"))

(defun reload-config ()
  (interactive)
  "Reload the configuration from ~/.emacs"
  (load-file "~/.emacs"))

(defun my-run-julia ()
  (interactive)
  (if (get-buffer "Julia")
      (switch-to-buffer "Julia")
    (progn
      (multi-vterm)
      (rename-buffer "Julia")
      (vterm-send-string "julia")
      (execute-kbd-macro (kbd "<return>")))))

(defun run-in-julia ()
  (interactive)
  (progn
    (kill-ring-save)
    (my-run-julia)
    (yank)
    (execute-kbd-macro (kbd "<return>"))))

(defun run-cmus ()
  (interactive)
  (if (get-buffer "CMUS")
      (switch-to-buffer "CMUS")
    (progn
      (multi-vterm)
      (rename-buffer "CMUS")
      (vterm-send-string "cmus")
      (execute-kbd-macro (kbd "<return>")))))

;; TO DO: modify ELSE so that only the window is killed if the buffer is open also on another window
(defun clean ()
  "Kills the current buffer. Additionally kills the current window if other windows are present."
  (interactive)
  (if (one-window-p)
      (kill-current-buffer)
    (kill-buffer-and-window)))

;;;; SEE:
;; https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
(if (>= emacs-major-version 27)
    (progn
      (defun screenshot-svg ()
	"Save a screenshot of the current frame as an SVG image. Saves to a temp file and puts the filename in the kill ring."
	(interactive)
	(let* ((filename (make-temp-file "Emacs" nil ".svg"))
               (data (x-export-frames nil 'svg)))
	  (with-temp-file filename
	    (insert data))
	  (kill-new filename)
	  (message filename)))
      (global-set-key [f4] 'screenshot-svg))
  nil)


;; ================================
;; ====== SPECIAL EXTENSIONS ======
;; ================================

;; .rpl (replay) files for IcemCFD, actually TCL files
(defun set-rpl-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.rpl\\'" buffer-file-name))
    (tcl-mode)))
(add-hook 'find-file-hook 'set-rpl-mode)

(defun set-gnuplot-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.plt\\'" buffer-file-name))
    (gnuplot-mode)))
(add-hook 'find-file-hook 'set-gnuplot-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =============================
;; ====== GLOBAL SETTINGS ======
;; =============================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(if (>= emacs-major-version 27)
;    (setq package-quickstart t)
;  nil)

;; ====== GConf INTERACTION ======
;;;; PROBLEM
;; The font is set according to Gnome defaults instead of enforcing settings specified below
;;;; SEE
;;https://emacs.stackexchange.com/questions/32641/something-changes-the-default-face-in-my-emacs/32664#32664
(define-key special-event-map [config-changed-event] 'ignore);; Not workig at the moment...

;; ====== HISTORIES AND SESSIONS ======
(savehist-mode 1); Persistent minibuffer history
(when window-system
  (desktop-save-mode 0)); Persistent sessions (Emacs desktop)

;; ====== FONTS AND ASPECT ======

;;;; Default font, according to the platform in use
(defun set-font ()
  (cond ((gnu/linux?)
	 (if (member font-linux (font-family-list))
	     (if (>= emacs-major-version 27)
		 (set-face-attribute 'default t :family font-linux)
	       (set-default-font font-linux))
	   (if (member font-linux-fallback (font-family-list))
	       (if (>= emacs-major-version 27)
		   (set-face-attribute 'default t :family font-linux-fallback)
		 (set-default-font font-linux-fallback) nil))))
	((darwin?)
	 (if (>= emacs-major-version 27)
	     (set-face-attribute 'default t :family font-darwin)
	   (set-default-font font-darwin)))
	((windows-nt?)
	 (if (>= emacs-major-version 27)
	     (set-face-attribute 'default t :family font-windows)
	   (set-default-font font-windows)))
	)
  )

(set-font)

;; SET DEFAULT THEME (LIGHT OR DARK)
(load-dark-theme)
;(load-light-theme)

;;;; Suppress unused interface components
;(toggle-scroll-bar -1); Disable the scrollbar
(menu-bar-mode -1); Disable the menu bar
(setq inhibit-splash-screen t); Disable splash screen
(setq initial-scratch-message nil); Returns empty scratch buffer
(tool-bar-mode -1); Disable the toolbar
;(setq default-frame-alist '((undecorated . t)))

(defun enable-decorations ()
  "Enables window decorations on the fly."
  (interactive)
  (set-frame-parameter nil 'undecorated nil))

(defun disable-decorations ()
  "Disables window decorations on the fly."
  (interactive)
  (set-frame-parameter nil 'undecorated t))

;; It could be useful to enable/disable decorations on the fly
(global-set-key [f5] 'enable-decorations)
(global-set-key [f7] 'disable-decorations)

;; Default frame geometry and features
(setq default-frame-alist '())
(add-to-list 'default-frame-alist (cons 'left geometry-margin-left))
(add-to-list 'default-frame-alist (cons 'top geometry-margin-top))
(add-to-list 'default-frame-alist (cons 'width geometry-width))
(add-to-list 'default-frame-alist (cons 'height geometry-height))
(add-to-list 'default-frame-alist '(undecorated . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;;;;; Favourite frame dimensions
;(when (and window-system
;	   (not desktop-save-mode))
;  (set-frame-size (selected-frame) frame-width frame-height))
;(global-hl-line-mode 1); Highlight current row (reported bad behaviour)
;;;; Window behaviour
;(add-to-list 'default-frame-alist '(fullscreen . maximized)); Open Emacs maximized

;; ====== EDITING ======
;;;; Automation
(add-hook 'before-save-hook 'delete-trailing-whitespace); Delete despicable trailing whitespaces
;;;; Visual preferences
(global-visual-line-mode 1); Proper line wrapping
(if (version<= "26.0.50" emacs-version)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode); The new fancy mode
  (add-hook 'prog-mode-hook 'linum-mode)); The old, quite ugly mode

;; ====== UTILITIES ======
(setq column-number-mode t); Display the column number beside the line number

;; ====== ALIA ======
(setq calendar-week-start-day 1); Weeks start on Monday in calendar
;(setq visible-bell t); Flashes on errors instead of ringing the bell
(fset 'yes-or-no-p 'y-or-n-p); <y/n> instead of <yes/no>
;; No more annoying alarms when not needed
;; TODO Check and expand
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))
;; TEMP ???
(setq ring-bell-function 'ignore)


;; ====== BUFFERS ======
;;;; SEE:
;; https://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening/152151#152151
;;;;;; Removes *messages* from the buffer.
;(setq-default message-log-max nil)
;(kill-buffer "*Messages*")
;;;;;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))


;; =====================
;; ====== BACKUPS ======
;; =====================

;; Set directory for storing backup files
(setq backup-directory-alist `(("." . "~/.emacs_backups")))
;; Always make backups by copying
(setq backup-by-copying t)


;; =======================================
;; ====== LANGUAGES AND SPELL CHECK ======
;; =======================================

;; Spell check
;; Remember to install related hunspell dictionaries if not already present on the system!
(with-eval-after-load 'flyspell
;;;; Choose languages for spell check
  (let ((langs '("american" "italiano" "english")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))
;;;; Define function to change the current language for spell check
  (defun cycle-ispell-languages ()
    "Cycles through the specified dictionaries for spell check via ispell."
    (interactive)
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang)))
;;;; Bind the function to a key
  (global-set-key [f6] 'cycle-ispell-languages))


;; ================================
;; ====== PACKAGE MANAGEMENT ======
;; ================================

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(if (< 27 emacs-major-version)
    (package-initialize)
  nil)

(require 'package)

(cond ((or (gnu/linux?) (darwin?))
       (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
			   (not (gnutls-available-p))))
	      (proto (if no-ssl "http" "https")))
	 (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
	 (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
	 ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
	 ;; and `package-pinned-packages`. Most users will not need or want to do this.
					;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
	 ))
      ((windows-nt?)
       (setq package-archives
	     `(("melpa" . , "~/elpa-mirror/melpa/")
	       ("org"   . , "~/elpa-mirror/org/")
	       ("gnu"   . , "~/elpa-mirror/gnu/")))))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =============================================
;; ====== Per-package settings --- CODING ======
;; =============================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; =============================================================================
;; ====== APDL-MODE (major mode for the ANSYS Parametric Design Language) ======
;; =============================================================================

(use-package apdl-mode
  :ensure t
  )


;; ================
;; ====== C# ======
;; ================
;; Not exactly functional at the moment, going to do some research when enough time

(use-package csharp-mode
  :ensure t
  :ensure omnisharp
  :hook (csharp-mode . omnisharp-mode)
  )


;; ===================================
;; ====== ELPY (PYTHON COMFORT) ======
;; ===================================

(use-package elpy
  :ensure t
  :ensure python-black
  :config (elpy-enable)
  :hook (elpy-mode . (lambda () (highlight-indentation-mode -1))); Deactivated since slow
  )


;; ======================================
;; ====== IRONY (SURVIVING TO C++) ======
;; ======================================
;;;; IMPORTANT:
;; After installing irony and related packages, llvm-config must be present
;; on the system in order for the command "irony-compile-server" to work.
;; On Fedora, "dnf provides llvm-config" easily reveals that on this OS the
;; needed package is called llvm-devel.
;; Also, clang-libs should be installed (on systems other than Fedora the name
;; could differ).
;;;; SEE:
;; https://parbo.github.io/blog/2016/05/10/configuring-emacs-for-cpp/
;; https://oremacs.com/2017/03/28/emacs-cpp-ide/

(use-package irony
  :ensure t
  :config (defun my-irony-mode-hook ()
	    (define-key irony-mode-map
	      [remap completion-at-point] 'counsel-irony)
	    (define-key irony-mode-map
	      [remap complete-symbol] 'counsel-irony))
  :hook (
	 (c++-mode . irony-mode)
	 (c-mode . irony-mode)
	 (objc-mode . irony-mode)
	 (irony-mode . my-irony-mode-hook)
	 ;; The following hook must not be used on Debian and derivatives such as Ubuntu
	 ;; because the "irony-server" package takes care of it system-wide rather than per-user
	 (irony-mode . irony-cdb-autosetup-compile-options)
	 )
  :bind ((:map irony-mode-map
	       ("C-q" . 'counsel-irony)))
  )


;; ====================
;; ====== JULIA  ======
;; ====================

(use-package julia-mode
  :ensure t
  :bind ("C-c C-c" . 'run-in-julia)
  )


;; ======================
;; ====== HASKELL  ======
;; ======================

(use-package haskell-mode
  :ensure t
  )


;; ===========================
;; ====== MESON / NINJA ======
;; ===========================

(use-package meson-mode
  :ensure t
  )


;; =================================
;; ====== MAGIT (GIT COMFORT) ======
;; =================================

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status)
  )
;; NOTE: magit-diff-range for diffs between branches
(use-package magit-delta
  :ensure t
  )


;; ========================
;; ====== POWERSHELL ======
;; ========================
(use-package powershell
  :ensure t
  )


;; ===================
;; ====== RUST  ======
;; ===================
;;
;; WARNING: Remember to install rust-src for the analyzer!
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom (lsp-prefer-capf t)
  )
(use-package lsp-ui
  :ensure t
  )
(use-package rustic
  :ensure
  )



;; =======================================
;; ====== GEISER (MODES FOR SCHEME) ======
;; =======================================

(use-package geiser
  :ensure t
  :config (if (file-exists-p chicken-startup-lib)
	      (setq geiser-chicken-init-file chicken-startup-lib)
	    nil)
  :bind ("C-S-s" . 'geiser)
  :custom ((geiser-default-implementation 'chicken)
	   (geiser-active-implementations '(chicken chez racket)))
  )


;; =============================
;; ====== SCHEME-COMPLETE ======
;; =============================

;(autoload 'scheme-smart-complete "scheme-complete" nil t)
;(with-eval-after-load 'scheme
;  '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))
;  '(define-key geiser-mode-map "\t" 'scheme-complete-or-indent))
;(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
;(add-hook 'scheme-mode-hook
;	  (lambda ()
;	    (make-local-variable 'eldoc-documentation-function)
;	    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
;	    (eldoc-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =================================================
;; ====== Per-package settings --- UTILITIES ======
;; =================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ===========================
;; ====== AUTO-COMPLETE ======
;; ===========================

(use-package auto-complete
  :ensure t
  ;:ensure auto-complete-config
  :ensure auto-complete-auctex
  :config (ac-config-default)
  :config (when (>= emacs-major-version 24)
	    (global-auto-complete-mode -1))
  )


;; ======================================
;; ====== AUTOMATIC PACKAGE UPDATE ======
;; ======================================

;;;; SEE:
;; https://emacs.stackexchange.com/questions/31826/does-use-package-keep-packages-automatically-updated
;; https://emacs.stackexchange.com/questions/31872/how-to-update-packages-installed-with-use-package

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 7) ;; update once a week
   (auto-package-update-maybe)
   )


;; =================================
;; ====== CIRCE (IRC CLIENT) =======
;; =================================

;;;; SEE:
;; https://github.com/jorgenschaefer/circe/wiki/Configuration

;;;; IMPORTANT:
;; If you're on Fedora Linux, please verify you have gnutls-utils installed!!

(use-package circe
  :ensure t
  ;; machine irc.freenode.net login <user_name> password <user_password> port 6667
  :config (progn
	    (setq auth-sources '("~/.authinfo.gpg"))
	    ;; ==================================================================
	    (defun my-fetch-password (&rest params)
					;(require 'auth-source)
	      (let ((match (car (apply 'auth-source-search params))))
		(if match
		    (let ((secret (plist-get match :secret)))
		      (if (functionp secret)
			  (funcall secret)
			secret))
		  (error "Password not found for %S" params))))
	    ;; ==================================================================
	    (defun my-nickserv-password (server)
	     (my-fetch-password :login "rmura" :machine "irc.freenode.net"))
	    ;; ==================================================================
	    (setq circe-network-options
		  '(("Freenode"
		     :tls nil
		     :nick "rmura"
		     :nickserv-nick "rmura"
		     :nickserv-password my-nickserv-password
		     :realname "Riccardo Mura"))))
  )


;; =====================
;; ====== COMPANY ======
;; =====================

(use-package company
  :if (>= emacs-major-version 24)
  :ensure t
  :ensure company-ansible
  :ensure company-c-headers
  :ensure company-irony
  :ensure company-irony-c-headers
  :hook (after-init . global-company-mode)
  :custom (
	   (company-idle-delay 0.5)
	   (company-tooltip-align-annotations t) ;; Added for Rust
	   (company-minimum-prefix-length 1) ;; Added for Rust
	   )
  :config (progn
	    (defvar my-company-backends '(company-ansible
    					  company-c-headers
    					  company-irony
    					  company-irony-c-headers))
	    (defun add-company-backend (backend)
	      (when (package-installed-p 'backend)
  		(add-to-list 'company-backends 'backend)))
	    (mapcar 'add-company-backend my-company-backends))
  :bind (:map company-active-map
	      ("C-n" . 'company-select-next)
	      ("C-p" . 'company-select-previous)
	      )
  )


;; =====================
;; ====== COUNSEL ======
;; =====================

(use-package counsel
  :ensure t
  )


;; ================================
;; ====== COUNSEL-PROJECTILE ======
;; ================================

(use-package counsel-projectile
  :ensure t
  :init (counsel-projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-c p" . 'projectile-commander)
	      )
  :custom (projectile-use-git-grep 1)
  )


;; =======================
;; ====== DIFFVIEW  ======
;; =======================

(use-package diffview
  :ensure t
  )


;; ===========================
;; ====== DIRED-SIDEBAR ======
;; ===========================

(use-package dired-sidebar
  :if (>= emacs-major-version 25)
  :ensure t
  :config (put 'dired-find-alternate-file 'disabled nil); disables warning (???)
  :bind (([f9] . 'dired-sidebar-toggle-with-current-directory)
	 (:map dired-mode-map
	       ;; Make dired open in the same window when using RET or ^
	       ("RET" . 'dired-find-alternate-file); was dired-advertised-find-file
	       ("^" . (lambda () (interactive) (find-alternate-file "..")))); was dired-up-directory
	 )
  :custom (dired-sidebar-one-instance-p t)
  )


;; =======================================
;; ====== FLYCHECK (SYNTAX CHECKING)======
;; =======================================

;;;; SEE:
;; https://www.reddit.com/r/emacs/comments/7mjyz8/flycheck_syntax_checking_makes_editing_files/

(use-package flycheck
  :ensure t
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :custom (
	   (flycheck-check-syntax-automatically '(save new-line))
	   (flycheck-display-errors-delay .9)
	   )
  :hook (
	 (prog-mode . flycheck-mode)
	 (c++-mode . flycheck-mode)
	 (c-mode . flycheck-mode)
	 (objc-mode . flycheck-mode)
	 )
  )


;; ==========================
;; ====== GNUPLOT MODE ======
;; ==========================

(use-package gnuplot
  :ensure t
  )


;; ==============================
;; ====== MULTIPLE-CURSORS ======
;; ==============================

(use-package multiple-cursors
  :ensure t
  :bind ("C-c m" . 'mc/edit-lines)
  )


;; ===============================
;; ====== NOV (EPUB READER) ======
;; ===============================

(use-package nov
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )


;; =======================
;; ====== PDF-TOOLS ======
;; =======================

(use-package pdf-tools
  :ensure t
  :ensure swiper
  ;;:if window-system
  :magic ("%PDF" . pdf-view-mode)
  :bind (
	 (:map pdf-outline-buffer-mode-map
	      ("RET" . 'pdf-outline-follow-link-and-quit)
	      ("M-RET" . 'pdf-outline-follow-link)
	      )
	 (:map pdf-view-mode-map
	       ("j" . pdf-view-next-line-or-next-page)
	       ("k" . pdf-view-previous-line-or-previous-page)
	       ("C-q" . isearch-forward)
	       )
	 )
  :config (pdf-tools-install :no-query)
  :custom (
	   (pdf-cache-prefetch-delay 0.2); Previously 0.15, then 0.05, then 0.5, then 0.2
	   (image-cache-eviction-delay 400); Previously 360, then 160, then 1000
	   (pdf-view-use-scaling t)
	   )
  :hook (
	 (pdf-view-mode . (lambda() (blink-cursor-mode -1)))
	 (pdf-view-mode . (lambda() (visual-line-mode -1)))
	 )
  )

;; ==========================
;; ====== POLY-ANSIBLE ======
;; ==========================

(use-package poly-ansible
  :ensure t
  )


;; ==========================
;; ====== QUICK-PREVIEW =====
;; ==========================

(use-package quick-preview
  :ensure t
  :unless (windows-nt?)
  )
(when (fboundp 'quick-preview-at-point)
  (progn
    (global-set-key (kbd "C-c q") 'quick-preview-at-point)
    (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point)))


;; ==========================================
;; ====== SWIPER (COMPLETION FRONTEND) ======
;; ==========================================

(use-package swiper
  :ensure t
  :bind (
	 ("C-s" . swiper)
	 )
  :custom (ivy-display-style 'fancy)
  )


;; ======================================
;; ====== TELEGA (TELEGRAM CLIENT) ======
;; ======================================
;;;;; If on Fedora, in order for telegram-server to compile, install tdlib from
;;;;;;;  https://copr.fedorainfracloud.org/coprs/carlis/tdlib-fresh/

(use-package telega
  :ensure t
  :config (progn
	    (telega-notifications-mode 1)
	    (add-hook 'telega-chat-mode-hook
		      (lambda ()
			(set (make-local-variable 'company-backends)
			     (append '(telega-company-emoji
				       telega-company-username
				       telega-company-hashtag)
				     (when (telega-chat-bot-p telega-chatbuf--chat)
				       '(telega-company-botcmd))))
			(company-mode 1))))
  :custom (
	   (telega-use-images t)
	   (telega-chat-show-avatars t)
	   (telega-root-show-avatars t)
	   (telega-user-show-avatars t)
	   (telega-emoji-font-family "Noto Color Emoji")
	   (telega-emoji-use-images t)
	   )
  )


;; =============================================================
;; ====== TERM, ANSI-TERM, VTERM, MULTI-TERM, MULTI-VTERM ======
;; =============================================================

(use-package multi-term
  :ensure t
  :config (defadvice term-handle-exit
	      (after term-kill-buffer-on-exit activate)
	    (kill-buffer))
  :bind ("C-x t" . 'multi-term)
  )

(use-package multi-vterm
  :ensure vterm
  :ensure t
  :bind ("C-x t" . 'multi-vterm)
  )


;; ===============================
;; ======= VIEW LARGE FILES ======
;; ===============================

(use-package vlf
  :ensure t
  :init (require 'vlf-setup)
  )


;; =========================
;; ======= VIMRC MODE ======
;; =========================

(use-package vimrc-mode
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =================================================
;; ====== Per-package settings --- AESTHETICS ======
;; =================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ======================================
;; ====== CREAMSODY (COLOR SCHEME) ======
;; ======================================

(use-package creamsody-theme
  :ensure t
  )


;; ===========================
;; ====== DOOM-MODELINE ======
;; ===========================

(use-package doom-modeline
  :ensure t
  :ensure all-the-icons
  ;:config (all-the-icons-install-fonts)
  :custom (
	   (doom-modeline-buffer-encoding t)
	   (doom-modeline-env-version t)
	   ;(doom-modeline-icon (display-graphic-p))
	   (doom-modeline-icon t)
	   (doom-modeline-irc t)
	   (doom-modeline-minor-modes t)
	   (inhibit-compacting-font-caches t)
	   )
  :hook (after-init . doom-modeline-mode)
  )


;; ==================================
;; ====== DREAM (COLOR SCHEME) ======
;; ==================================

(use-package dream-theme
  :ensure t
  )


;; ====================================
;; ====== GRUVBOX (COLOR SCHEME) ======
;; ====================================

(use-package gruvbox-theme
  :ensure t
  )


;; ========================================
;; ====== MONOKAI PRO (COLOR SCHEME) ======
;; ========================================

(use-package monokai-pro-theme
  :ensure t
  )


;; =================================
;; ====== NORD (COLOR SCHEME) ======
;; =================================

(use-package nord-theme
  :ensure t
  )


;; =================================
;; ====== ONE (COLOR SCHEME) ======
;; =================================

(use-package one-themes
  :ensure t
  )


;; ================================
;; ====== RAINBOW DELIMITERS ======
;; ================================

(use-package rainbow-delimiters
  :ensure t
  :hook (scheme-mode . rainbow-delimiters)
  )


;; ===============================================
;; ====== SANITYINC TOMORROW (COLOR SCHEME) ======
;; ===============================================

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  )

;; ======================================
;; ====== SOLARIZED (COLOR SCHEME) ======
;; ======================================
(use-package solarized-theme
  :ensure t
  )

;; ====================================
;; ====== ZENBURN (COLOR SCHEME) ======
;; ====================================

(use-package zenburn-theme
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ============================================
;; ====== Per-package settings --- LaTeX ======
;; ============================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ===========================================
;; ====== LaTeX --- COMFORT WITH AUCTEX ======
;; ===========================================

(use-package tex
  :defer t
  :ensure auctex :ensure auctex-latexmk
  :config (progn
	    (auctex-latexmk-setup)
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer))
  :custom (
	   (TeX-engine 'xetex)
	   (reftex-plug-into-AUCTeX t)
	   (reftex-extra-bindings t); C-c c is enough for citations
	   (TeX-view-program-selection '((output-pdf "PDF Tools"))); Use pdf-tools to open PDF files
	   (TeX-source-correlate-start-server t)
	   )
  :hook (
	 (LaTeX-mode turn-on-reftex)
	 (Tex-mode . flyspell-mode); Enable spell check (not for comments)
	 (Tex-mode . auto-complete)
	 (Tex-mode . (lambda() (local-set-key [C-tab] 'TeX-complete-symbol)))
	 )
  )


;; ======================================
;; ====== LaTeX --- MATH PREVIEW  =======
;; ======================================

(use-package latex-math-preview
  :ensure t
  :bind ("C-c e" . 'latex-math-preview-expression); Preview for equations
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ==========================================
;; ====== Per-package settings --- ORG ======
;; ==========================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ======================
;; ====== ORG MODE ======
;; ======================

(use-package org
  :ensure t
  ;;;; SEE:
  ;; https://emacs.stackexchange.com/questions/60379/c-c-c-e-l-o-does-not-open-the-pdf-anymore
  :config (push '("\\.pdf\\'" . emacs) org-file-apps)
  :config (push '("\\.html\\'" . epiphany) org-file-apps)
  :config (remove-hook 'org-cycle-hook
		       #'org-optimize-window-after-visibility-change)
  :custom (
	   (org-log-done 'time)
	   (org-hide-emphasis-markers t)
	   (org-latex-listings 'minted)
	   (org-latex-minted-options '(("autogobble=true")
				       ("frame" "single") ;; (none | leftline | topline | bottomline | lines | single)
				       ("fontsize=\\scriptsize")
				       ("linenos=true")))
	   (org-latex-prefer-user-labels t)
	   (org-latex-pdf-process
	    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	      "bibtex %b"
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
	   )
  )


;; =========================
;; ====== ORG BULLETS ======
;; =========================

;; Fancy bullets for sections
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  )


;; ====================================
;; ====== OX (ORG MODE EXPORTER) ======
;; ====================================
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("letter"
		 "\\documentclass{letter}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 )
	       )
  )

;; ==================================
;; ====== ORG LaTeX SUBFIGURES ======
;; ==================================
;; Only good if the figures already have the same aspect ratio
(use-package ox-latex-subfigure
  :ensure t
  :after ox
  :custom (org-latex-prefer-user-labels t)
  )


;; =====================
;; ====== OX-HUGO ======
;; =====================
(use-package ox-hugo
  :ensure t
  :after ox
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; =============================
;; ====== GLOBAL KEYBINDS ======
;; =============================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTANT: Don't forget anything assigend to C-i is automatically assigned to TAB
(global-set-key (kbd "C-c :") 'goto-line); Quasi-Vim choice for reaching a line
(global-set-key (kbd "C-S-a") 'ansi-term); Terminal emulator
(global-set-key (kbd "C-S-c") 'clean); Kill the buffer (and the window if not unique)
(global-set-key (kbd "C-S-s") 'eshell); Emacs shell: Emacs Lisp interpreter is there
(global-set-key (kbd "C-c -'") 'forward-word)
(global-set-key (kbd "C-c ;") 'backward-word)
(global-set-key (kbd "C-c C-l") 'magit-log-buffer-file); Summon git history for file in buffer
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-c c") 'kill-ring-save); Copy text
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c r") 'reload-config); Reload configuration
(global-set-key (kbd "C-c s") 'create-scratch-buffer); Create scratch or swith to existing
(global-set-key (kbd "C-x C-b") 'ibuffer); A buffer to rule them all
(global-set-key (kbd "C-x b") 'ivy-switch-buffer); Replace default buffer switching
(global-set-key (kbd "H-b") 'backward-word)
(global-set-key (kbd "H-f") 'forward-word)
(global-set-key (kbd "H-n") 'end-of-buffer)
(global-set-key (kbd "H-p") 'beginning-of-buffer)
;; Resize frames (Vim style)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-j") 'shrink-window)
(global-set-key (kbd "C-S-k") 'enlarge-window)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
;; Numbering
(global-set-key (kbd "C-x +") 'shift-number-up); Increment number
(global-set-key (kbd "C-x -") 'shift-number-down); Decrease number
;; Move between windows (directional arrows)
(global-set-key (kbd "C-c <down>") 'windmove-down); Move to lower window
(global-set-key (kbd "C-c <left>") 'windmove-left); Move to left window
(global-set-key (kbd "C-c <right>") 'windmove-right); Move to right window
(global-set-key (kbd "C-c <up>") 'windmove-up); Move to upper window
;; Move between windows (Vim style)
(global-set-key (kbd "C-c h") 'windmove-left); Move to left window
(global-set-key (kbd "C-c j") 'windmove-down); Move to lower window
(global-set-key (kbd "C-c k") 'windmove-up); Move to upper window
(global-set-key (kbd "C-c l") 'windmove-right); Move to right window

;(global-set-key (kbd "C-c C-j") (bind-OTF "julia" "Julia"))
;(global-set-key (kbd "C-c C-m") (bind-OTF "cmus" "CMUS"))
;(global-set-key (kbd "C-c C-p") (bind-OTF "htop" "HTOP"))

;(global-set-key (kbd "C-c C-j") (bind-with-args OTF "julia" "Julia"))
;(global-set-key (kbd "C-c C-m") (bind-with-args OTF "cmus" "CMUS"))
;(global-set-key (kbd "C-c C-p") (bind-with-args OTF "htop" "HTOP"))

(global-set-key-extended (kbd "C-c C-h") OTF "htop" "HTOP")
(global-set-key-extended (kbd "C-c C-j") OTF "julia" "Julia")
(global-set-key-extended (kbd "C-c C-m") OTF "cmus" "CMUS")
;(global-set-key-extended (kbd "C-c C-t") OTF "" "vterm")

(defun launch-terminal ()
  (interactive)
  (progn
    (OTF "" "*vterm*" )))
(global-set-key (kbd "C-c C-t") 'launch-terminal)

(global-set-key [f12] 'load-dark-theme)
(global-set-key [f8] 'load-light-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ===============================
;; ====== WINDOWS ADDITIONS ======
;; ===============================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (windows-nt?)
  ;; This configuration was created under Fedora Linux
  ;; The user is a GNU/Linux user who does not enjoy working under Windows
  ;; Use under Microsoft Windows was only needed because of circumstances
  ;; (no freedom on enterprise laptops provided by the company...)

  ;; Donwload, e.g.: https://the.earth.li/~sgtatham/putty/latest/w64/putty.zip
  ;; Unzip the archive and add the folder with executables to path
  ;; As a normal user, something like below shoud be fine:
  ;; setx path "%path%;<PuTTY directory absolute path."
  ;;(require 'tramp)
  ;;(set-default 'tramp-auto-save-directory "C:\\Users\\<username>\\AppData\\Local\\Temp")
  ;;(set-default 'tramp-default-method "plink")

  (setq ring-bell-function 'ignore); On Windows the bell is usually annoying, the visible unuseful
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ===============================
;; ====== GARGABE COLLECTOR ======
;; ===============================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE:: This overrides the values used while reading the config
(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.1)
