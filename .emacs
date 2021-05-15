;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   ██████╗ ███╗   ██╗██╗   ██╗    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝ ████╗  ██║██║   ██║    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  ██║  ███╗██╔██╗ ██║██║   ██║    █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██║   ██║██║╚██╗██║██║   ██║    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ╚██████╔╝██║ ╚████║╚██████╔╝    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;   ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝     ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: the font above is ANSI Shadow


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


;; ==========================================
;; ====== GLOBAL CONSTANTS AND ALIASES ======
;; ==========================================

;; ====== GARBAGE COLLECTOR ======
;; NOTE:: This is changeg at the end of the config
(setq gc-cons-threshold (* 512 1024 1024))
(setq gc-cons-percentage 0.6)

;; ====== GENERIC ======
(if (version<= emacs-version "24.4")
    (dexfalias 'with-eval-after-load 'eval-after-load)
  nil)

;; ====== FONTS ======
(setq font-linux "Source Code Pro")
;(setq font-linux "Cascadia Code")
(setq font-linux-fallback "DejaVu Sans Mono")
(setq font-darwin "Monaco")
(setq font-darwin-fallback "Menlo")
(setq font-windows "Consolas")
(setq font-windows-fallback "Lucida Console")

;; ====== COLOR THEME ======
(setq theme-dark 'monokai-pro-spectrum)
;(setq theme-light 'sanityinc-tomorrow-day)
(setq theme-light 'gruvbox-light-medium)

;; ====== GEOMETRY ======
(setq geometry-width 110)
(setq geometry-height 40)
(setq geometry-margin-top 70)
(setq geometry-margin-left 140)


;; ====== DOC VIEW ======
(setq doc-view-resolution 400)


;; ====== C / C++ ======
(setq c-default-style "stroustrup")

;; ====== SCHEME ======
(defvar chicken-startup-lib "~/fluent-assistant/portable-core.scm"
  "Path to the library to be loaded when running CHICKEN Scheme via geiser.")
(add-hook 'scheme-mode-hook 'show-paren-mode)
;(add-hook 'scheme-mode-hook 'paredit-mode)

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


;; ==================================
;; ====== FUNCTION DEFINITIONS ======
;; ==================================

(defun OTF (process name)
  "execute a process in a new terminal session on the fly
or switch to the buffer if it already exists"
  (interactive)
  (if (get-buffer name)
      (switch-to-buffer name)
    (progn
      (multi-vterm)
      (rename-buffer name)
      (vterm-send-string process)
      (execute-kbd-macro (kbd "<return>")))))

;; See https://www.emacswiki.org/emacs/RecreateScratchBuffer
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

;; See https://www.reddit.com/r/emacs/comments/idz35e/emacs_27_can_take_svg_screenshots_of_itself/
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


;; ===============================
;; ====== MACRO DEFINITIONS ======
;; ===============================

;(defmacro bind-OTF (process name)
;  `(lambda ()
;     (interactive)
;     (OTF ,process ,name)))

(defmacro bind-with-args (f &rest args)
  `(lambda ()
     (interactive)
     (,f ,@args)))

(defmacro global-set-key-extended (key command &rest args)
  `(global-set-key ,key (lambda () (interactive) (,command ,@args))))


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

;; ====== HISTORIES AND SESSIONS ======
(savehist-mode 1); Persistent minibuffer history
(when window-system
  (desktop-save-mode 0)); Persistent sessions (Emacs desktop)

;; ====== FONTS AND ASPECT ======

;;;; Default font, according to the platform in use
(cond ((gnu/linux?)
       (if (member font-linux (font-family-list))
	   (if (>= emacs-major-version 27)
	       (set-face-attribute 'default t :font font-linux)
	     (set-default-font font-linux))
	 (if (member font-linux-fallback (font-family-list))
	     (if (>= emacs-major-version 27)
		 (set-face-attribute 'default t :font font-linux-fallback)
	       (set-default-font font-linux-fallback) nil))))
      ((darwin?)
       (if (>= emacs-major-version 27)
	   (set-face-attribute 'default t :font font-darwin)
	 (set-default-font font-darwin)))
      ((windows-nt?)
       (if (>= emacs-major-version 27)
	   (set-face-attribute 'default t :font font-windows)
	 (set-default-font font-windows))))

(load-dark-theme)

;;;; Suppress unused interface components
;(toggle-scroll-bar -1); Disable the scrollbar
;(menu-bar-mode -1); Disable the menu bar
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
;; See:
;;;; https://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening/152151#152151
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ==================================
;; ====== PER-PACKAGE SETTINGS ======
;; ==================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ===========================
;; ====== DOOM-MODELINE ======
;; ===========================

(use-package doom-modeline
  :ensure t
  :ensure all-the-icons
  ;:config (all-the-icons-install-fonts)
  :custom ((doom-modeline-buffer-encoding t)
	   (doom-modeline-env-version t)
	   ;(doom-modeline-icon (display-graphic-p))
	   (doom-modeline-icon t)
	   (doom-modeline-irc t)
	   (doom-modeline-minor-modes t)
	   (inhibit-compacting-font-caches t))
  :hook (after-init . doom-modeline-mode)
  )


;; ========================================
;; ====== MONOKAI PRO (COLOR SCHEME) ======
;; ========================================
(use-package monokai-pro-theme
  :ensure t
  )


;; ===============================================
;; ====== SANITYINC TOMORROW (COLOR SCHEME) ======
;; ===============================================
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  )


;; ====================================
;; ====== GRUVBOX (COLOR SCHEME) ======
;; ====================================

(use-package gruvbox-theme
  :ensure t
  )


;; ===============================
;; ======= VIEW LARGE FILES ======
;; ===============================
(use-package vlf
  :ensure t
  :init (require 'vlf-setup)
  )


;; =============================================================================
;; ====== APDL-MODE (major mode for the ANSYS Parametric Design Language) ======
;; =============================================================================
(use-package apdl-mode
  :ensure t
  )

;; =================================
;; ====== CIRCE (IRC CLIENT) =======
;; =================================

;; See: https://github.com/jorgenschaefer/circe/wiki/Configuration
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


;; ===================================
;; ====== ELPY (PYTHON COMFORT) ======
;; ===================================

(use-package elpy
  :ensure t
  :ensure python-black
  :config (elpy-enable)
  :hook (elpy-mode . (lambda () (highlight-indentation-mode -1))); Deactivated since slow
  )


;; ====================
;; ====== JULIA  ======
;; ====================

(use-package julia-mode
  :ensure t
  :bind ("C-c C-c" . 'run-in-julia)
  )


;(use-package julia-repl
;  :ensure t
;  :bind ("C-x j" . 'julia-repl)
;  :init (setenv "JULIA_NUM_THREADS" "6")
;  :custom (julia-repl-set-terminal-backend 'vterm)
;  :hook (julia-mode . julia-repl-mode)
;  )


;; ======================
;; ====== HASKELL  ======
;; ======================

(use-package haskell-mode
  :ensure t
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


;; ==============================
;; ====== MULTIPLE-CURSORS ======
;; ==============================

(use-package multiple-cursors
  :ensure t
  :bind ("C-c m" . 'mc/edit-lines)
  )


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
  :custom ((company-idle-delay 0.5))
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
	      ("C-p" . 'company-select-previous))
  )


;; =======================================
;; ====== FLYCHECK (SYNTAX CHECKING)======
;; =======================================
;; SEE:
;;;; https://www.reddit.com/r/emacs/comments/7mjyz8/flycheck_syntax_checking_makes_editing_files/

(use-package flycheck
  :ensure t
  :custom ((flycheck-check-syntax-automatically '(save new-line))
	   (flycheck-display-errors-delay .9))
  :hook ((c++-mode . flycheck-mode)
	 (c-mode . flycheck-mode)
	 (objc-mode . flycheck-mode)
	 )
  )


;; ========================================
;; ====== AUCTEX (COMFORT FOR LaTeX) ======
;; ========================================

(use-package tex
  :defer t
  :ensure auctex :ensure auctex-latexmk
  :config (progn
	    (auctex-latexmk-setup)
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer))
  :custom ((TeX-engine 'xetex)
	   (reftex-plug-into-AUCTeX t)
	   (reftex-extra-bindings t); C-c c is enough for citations
	   (TeX-view-program-selection '((output-pdf "PDF Tools"))); Use pdf-tools to open PDF files
	   (TeX-source-correlate-start-server t))
  :hook ((LaTeX-mode turn-on-reftex)
	 (Tex-mode . flyspell-mode); Enable spell check (not for comments)
	 (Tex-mode . auto-complete)
	 (Tex-mode . (lambda() (local-set-key [C-tab] 'TeX-complete-symbol))))
  )


;; ===========================
;; ====== PREVIEW-LATEX ======
;; ===========================

(use-package latex-math-preview
  :ensure t
  :bind ("C-c e" . 'latex-math-preview-expression); Preview for equations
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
	       ("^" . (lambda () (interactive) (find-alternate-file ".."))))); was dired-up-directory
  :custom (dired-sidebar-one-instance-p t)
  )


;; ======================================
;; ====== IRONY (SURVIVING TO C++) ======
;; ======================================
;; IMPORTANT:
;;;; After installing irony and related packages, llvm-config must be present
;;;; on the system in order for the command "irony-compile-server" to work.
;;;; On Fedora, "dnf provides llvm-config" easily reveals that on this OS the
;;;; needed package is called llvm-devel.
;;;; Also, clang-libs should be installed (on systems other than Fedora the name
;;;; could differ).
;; SEE:
;;;; https://parbo.github.io/blog/2016/05/10/configuring-emacs-for-cpp/
;;;; https://oremacs.com/2017/03/28/emacs-cpp-ide/

(use-package irony
  :ensure t
  :config (defun my-irony-mode-hook ()
	    (define-key irony-mode-map
	      [remap completion-at-point] 'counsel-irony)
	    (define-key irony-mode-map
	      [remap complete-symbol] 'counsel-irony))
  :hook ((c++-mode . irony-mode)
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

;(use-package cmake-ide
;  :ensure t
;  :init
;  (use-package semantic/bovine/gcc)
;  (setq cmake-ide-flags-c++ (append '("-std=c++11")
;				    (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
;  (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
;  (cmake-ide-setup)
;  )


;; ==========================
;; ====== GNUPLOT MODE ======
;; ==========================
(use-package gnuplot
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


;; ===================
;; ====== MPDEL ======
;; ===================
;(require 'mpdel)
;(require 'ivy-mpdel)
;(mpdel-mode)


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
  :bind ((:map pdf-outline-buffer-mode-map
	      ("RET" . 'pdf-outline-follow-link-and-quit)
	      ("M-RET" . 'pdf-outline-follow-link))
	 (:map pdf-view-mode-map
	       ("C-q" . isearch-forward))
	 )
  :config (pdf-tools-install :no-query)
  :custom ((pdf-cache-prefetch-delay 0.2); Previously 0.15, then 0.05, then 0.5, then 0.2
	   (image-cache-eviction-delay 400); Previously 360, then 160, then 1000
	   (pdf-view-use-scaling t))
  :hook ((pdf-view-mode . (lambda() (blink-cursor-mode -1)))
	 (pdf-view-mode . (lambda() (visual-line-mode -1))))
  )


;; ======================
;; ====== ORG MODE ======
;; ======================

(use-package org
  :ensure t
  ;; See https://emacs.stackexchange.com/questions/60379/c-c-c-e-l-o-does-not-open-the-pdf-anymore
  :config (push '("\\.pdf\\'" . emacs) org-file-apps)
  :config (push '("\\.html\\'" . epiphany) org-file-apps)
  :config (remove-hook 'org-cycle-hook
		       #'org-optimize-window-after-visibility-change)
  :custom ((org-log-done 'time)
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
              "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
  )

;; Fancy bullets for sections
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  )

;; Only good if the figures already have the same aspect ratio
(use-package ox-latex-subfigure
  :ensure t
  :after ox
  :custom (org-latex-prefer-user-labels t)
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


;; ================================
;; ====== RAINBOW DELIMITERS ======
;; ================================

(use-package rainbow-delimiters
  :ensure t
  :hook (scheme-mode . rainbow-delimiters)
  )


;; ======================================
;; ====== RTAGS (SURVIVING TO C++) ======
;; ======================================
;; SEE https://thebeautifullmind.com/2019/07/13/rtags-completes-emacs/
;; On Fedora Linux:
;;;; dnf in rtags
;(when (package-installed-p 'rtags)
;  (progn
;    (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;    (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;
;    (defun setup-flycheck-rtags ()
;      (interactive)
;      (flycheck-select-checker 'rtags)
;      (setq-local flycheck-highlighting-mode nil)
;      (setq-local flycheck-check-syntax-automatically nil))
;
;    (rtags-enable-standard-keybindings)
;    (setq rtags-autostart-diagnostics t)
;    (rtags-diagnostics)
;
;    (push 'company-rtags company-backends)
;
;    (setq rtags-completions-enabled t)
;
;    ;; use rtags flycheck mode -- clang warnings shown inline
;    (require 'flycheck-rtags)
;    ;; c-mode-common-hook is also called by c++-mode
;    (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
;
;    (when (package-installed-p 'ivy-rtags)
;      (setq rtags-display-result-backend 'ivy))))


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
	      ("C-c p" . 'projectile-commander))
  :custom (projectile-use-git-grep 1)
  )


;; ==========================================
;; ====== SWIPER (COMPLETION FRONTEND) ======
;; ==========================================

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  :custom (ivy-display-style 'fancy)
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


; ======================================
; ====== TELEGA (TELEGRAM CLIENT) ======
; ======================================
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
  :custom ((telega-use-images t)
	   (telega-chat-show-avatars t)
	   (telega-root-show-avatars t)
	   (telega-user-show-avatars t)
	   (telega-emoji-font-family "Noto Color Emoji")
	   (telega-emoji-use-images t)
	   )
  )


;; =============================
;; ====== GLOBAL KEYBINDS ======
;; =============================

(global-set-key (kbd "H-f") 'forward-word)
(global-set-key (kbd "H-b") 'backward-word)
(global-set-key (kbd "H-p") 'beginning-of-buffer)
(global-set-key (kbd "H-n") 'end-of-buffer)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-c s") 'create-scratch-buffer); Create scratch or swith to existing
(global-set-key (kbd "C-c C-l") 'magit-log-buffer-file); Summon git history for file in buffer
(global-set-key (kbd "C-c r") 'reload-config); Reload configuration
(global-set-key (kbd "C-c c") 'kill-ring-save); Copy text
(global-set-key (kbd "C-x C-b") 'ibuffer); A buffer to rule them all
(global-set-key (kbd "C-S-a") 'ansi-term); Terminal emulator
(global-set-key (kbd "C-S-s") 'eshell); Emacs shell: Emacs Lisp interpreter is there
(global-set-key (kbd "C-:") 'goto-line); Quasi-Vim choice for reaching a line
(global-set-key (kbd "C-x b") 'ivy-switch-buffer); Replace default buffer switching
(global-set-key (kbd "C-S-c") 'clean); Kill the buffer (and the window if not unique)
;; Resize frames (Vim style)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-j") 'shrink-window)
(global-set-key (kbd "C-S-k") 'enlarge-window)
;; Numbering
(global-set-key (kbd "C-x +") 'shift-number-up); Increment number
(global-set-key (kbd "C-x -") 'shift-number-down); Decrease number
;; Move between windows (directional arrows)
(global-set-key (kbd "C-c <up>") 'windmove-up); Move to upper window
(global-set-key (kbd "C-c <down>") 'windmove-down); Move to lower window
(global-set-key (kbd "C-c <left>") 'windmove-left); Move to left window
(global-set-key (kbd "C-c <right>") 'windmove-right); Move to right window
;; Move between windows (Vim style)
(global-set-key (kbd "C-c k") 'windmove-up); Move to upper window
(global-set-key (kbd "C-c j") 'windmove-down); Move to lower window
(global-set-key (kbd "C-c h") 'windmove-left); Move to left window
(global-set-key (kbd "C-c l") 'windmove-right); Move to right window

;(global-set-key (kbd "C-c C-j") (bind-OTF "julia" "Julia"))
;(global-set-key (kbd "C-c C-m") (bind-OTF "cmus" "CMUS"))
;(global-set-key (kbd "C-c C-p") (bind-OTF "htop" "HTOP"))

;(global-set-key (kbd "C-c C-j") (bind-with-args OTF "julia" "Julia"))
;(global-set-key (kbd "C-c C-m") (bind-with-args OTF "cmus" "CMUS"))
;(global-set-key (kbd "C-c C-p") (bind-with-args OTF "htop" "HTOP"))

(global-set-key-extended (kbd "C-c C-j") OTF "julia" "Julia")
(global-set-key-extended (kbd "C-c C-m") OTF "cmus" "CMUS")
(global-set-key-extended (kbd "C-c C-p") OTF "htop" "HTOP")

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" default))
 '(package-selected-packages
   '(gnuplot-mode gnuplot haskell-mode cyberpunk-theme color-theme-sanityinc-tomorrow ample-theme monokai-pro-theme tiny use-package telega rainbow-delimiters quick-preview python-black poly-ansible pdf-tools ox-latex-subfigure org-bullets nov multiple-cursors multi-vterm multi-term meson-mode magit latex-math-preview key-chord julia-mode irony-eldoc hydra htmlize helm-bibtex gruvbox-theme geiser flycheck-irony elpy doom-modeline dired-sidebar counsel-projectile company-irony-c-headers company-irony company-c-headers company-ansible cmake-ide circe auto-complete-auctex auctex-latexmk apdl-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ====== GARBAGE COLLECTOR ======
;; NOTE:: This overrides the values used while reading the config
(setq gc-cons-threshold (* 4 1024 1024))
(setq gc-cons-percentage 0.1)
