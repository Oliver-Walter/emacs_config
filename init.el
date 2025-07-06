;; ~/.emacs.d/init.el
;; Written by Oliver Walter

;;  _____ _                         
;; |_   _| |__   ___ _ __ ___   ___ 
;;   | | | '_ \ / _ \ '_ ` _ \ / _ \
;;   | | | | | |  __/ | | | | |  __/
;;   |_| |_| |_|\___|_| |_| |_|\___|

;; Rebecca theme
;; https://github.com/Oliver-Walter/rebecca_theme
(defvar rebecca-bg "#161616")
(defvar rebecca-fg "#FFF5EE")
(defvar rebecca-white "#FFF5EE")
(defvar rebecca-black "#000000")
(defvar rebecca-red "#B22222")
(defvar rebecca-green "#00FF7F")
(defvar rebecca-yellow "#FFB800")
(defvar rebecca-blue "#6A5ACD")
(defvar rebecca-magenta "#663399")
(defvar rebecca-cyan "#87CEFA")
(defvar rebecca-light-gray "#696969")
(defvar rebecca-dark-gray "#3B3B3B")
(defvar rebecca-light-red "#F08080")
(defvar rebecca-light-green "#90EE90")
(defvar rebecca-light-yellow "#F0E68C")
(defvar rebecca-light-blue "#8EA6FF")
(defvar rebecca-light-magenta "#9370DB")
(defvar rebecca-light-cyan "#E0FFFF")

(deftheme rebecca "The Rebecca theme, based on my girlfriend and some nice colours I like")

(custom-theme-set-faces
 'rebecca
 `(default ((t (:background ,rebecca-bg :foreground ,rebecca-fg))))
 `(cursor ((t (:background ,rebecca-cyan))))
 `(font-lock-comment-face ((t (:foreground ,rebecca-light-gray :italic t))))
 `(font-lock-string-face ((t (:foreground ,rebecca-light-green))))
 `(font-lock-keyword-face ((t (:foreground ,rebecca-magenta :bold t))))
 `(font-lock-function-name-face ((t (:foreground ,rebecca-blue))))
 `(font-lock-variable-name-face ((t (:foreground ,rebecca-yellow))))
 `(font-lock-type-face ((t (:foreground ,rebecca-cyan))))
 `(font-lock-constant-face ((t (:foreground ,rebecca-red))))
 `(font-lock-builtin-face ((t (:foreground ,rebecca-light-blue))))
 `(font-lock-warning-face ((t (:foreground ,rebecca-red :bold t))))
 `(region ((t (:background ,rebecca-dark-gray))))
 `(highlight ((t (:background ,rebecca-dark-gray))))
 `(mode-line ((t (:background ,rebecca-dark-gray :foreground ,rebecca-fg))))
 `(mode-line-inactive ((t (:background ,rebecca-bg :foreground ,rebecca-light-gray))))
 `(minibuffer-prompt ((t (:foreground ,rebecca-blue))))
 `(link ((t (:foreground ,rebecca-cyan :underline t))))
 `(error ((t (:foreground ,rebecca-red :bold t))))
 `(success ((t (:foreground ,rebecca-green :bold t))))
 `(warning ((t (:foreground ,rebecca-yellow :bold t))))
 `(header-line ((t (:background ,rebecca-dark-gray :foreground ,rebecca-fg))))
 `(vertical-border ((t (:foreground ,rebecca-light-gray))))

 ;; All faces used by whitespace-mode
 `(whitespace-space ((t (:foreground ,rebecca-dark-gray))))
 `(whitespace-hspace ((t (:foreground ,rebecca-dark-gray))))
 `(whitespace-tab ((t (:foreground ,rebecca-dark-gray))))
 `(whitespace-newline ((t (:foreground ,rebecca-bg))))
 `(whitespace-trailing ((t (:foreground ,rebecca-dark-gray))))
 `(whitespace-line ((t (nil))))
 `(whitespace-empty ((t (:foreground ,rebecca-dark-gray))))
 `(whitespace-space-after-tab ((t (:foreground ,rebecca-dark-gray))))
 `(whitespace-indentation ((t (:foreground ,rebecca-dark-gray))))
 )

(enable-theme 'rebecca)

(set-face-attribute 'default nil :font "Cascadia Code 14")

;;  ____            _                         
;; |  _ \ __ _  ___| | ____ _  __ _  ___  ___ 
;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;; |  __/ (_| | (__|   < (_| | (_| |  __/\__ \
;; |_|   \__,_|\___|_|\_\__,_|\__, |\___||___/
;;                            |___/           

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package helm               :ensure t :config (helm-mode))
(use-package magit              :ensure t)
(use-package which-key          :ensure t :config (which-key-mode))
(use-package move-text          :ensure t :bind (("M-<up>" . move-text-up) ("M-<down>" . move-text-down)))
(use-package dashboard          :ensure t :config (dashboard-setup-startup-hook))
(use-package all-the-icons      :ensure t)
(use-package nerd-icons         :ensure t)
(use-package vterm              :ensure t)

(use-package lsp-mode           :ensure t :hook ((c-mode c++-mode java-mode web-mode) . lsp-deferred) :commands lsp)
(use-package lsp-java           :ensure t :after lsp-mode)
(use-package company            :ensure t :hook (lsp-mode . company-mode))

;;  ____       _                 _                  
;; | __ )  ___| |__   __ ___   _(_) ___  _   _ _ __ 
;; |  _ \ / _ \ '_ \ / _` \ \ / / |/ _ \| | | | '__|
;; | |_) |  __/ | | | (_| |\ V /| | (_) | |_| | |   
;; |____/ \___|_| |_|\__,_| \_/ |_|\___/ \__,_|_|   

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)

(defvar backup-dir "~/.emacs.d/backups")
(unless (file-directory-p backup-dir) (make-directory backup-dir t))
(setq backup-directory-alist `((backup-dir)))
(setq make-backup-files t)
(setq backup-by-copying t)
(setq auto-save-default t)

;; (defvar line-length 100)
;; (setq whitespace-line-column line-length)
;; (setq-default fill-column line-length)
;; (setq display-fill-column-indicator-character ?\u2502)
;; (add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method 'hungry)

;; LSP stuff

(setq lsp-auto-guess-root t)
(setq lsp-log-io nil)
(setq lsp-restart 'auto-restart)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-hook nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-semantic-tokens-enable nil)
(setq lsp-enable-folding nil)
(setq lsp-enable-imenu nil)
(setq lsp-enable-snippet nil)
(setq read-process-output-max (* 1024 1024))
(setq lsp-idle-delay 0.5)

;;   ____          _      
;;  / ___|___   __| | ___ 
;; | |   / _ \ / _` |/ _ \
;; | |__| (_) | (_| |  __/
;;  \____\___/ \__,_|\___|

;;  __  __                          
;; |  \/  | __ _  ___ _ __ ___  ___ 
;; | |\/| |/ _` |/ __| '__/ _ \/ __|
;; | |  | | (_| | (__| | | (_) \__ \
;; |_|  |_|\__,_|\___|_|  \___/|___/

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "M-c") 'compile)

(defun comment-current-line ()
  "Comment out the current line."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (not (string-blank-p line))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (message "No line to comment out."))))

(global-set-key (kbd "C-;") 'comment-current-line)

;; Change lsp commands from s-l to C-c l
(global-set-key (kbd "C-c l w d") 'lsp-describe-session)
(global-set-key (kbd "C-c l h h") 'lsp-describe-thing-at-point)
(global-set-key (kbd "C-c l w D") 'lsp-disconnect)
(global-set-key (kbd "C-c l a h") 'lsp-document-highlight)
(global-set-key (kbd "C-c l a a") 'lsp-execute-code-action)
(global-set-key (kbd "C-c l g g") 'lsp-find-definition)
(global-set-key (kbd "C-c l g i") 'lsp-find-implementation)
(global-set-key (kbd "C-c l g r") 'lsp-find-references)
(global-set-key (kbd "C-c l g t") 'lsp-find-type-definition)
(global-set-key (kbd "C-c l = =") 'lsp-format-buffer)
(global-set-key (kbd "C-c l = r") 'lsp-format-region)
(global-set-key (kbd "C-c l T b") 'lsp-headerline-breadcrumb-mode)
(global-set-key (kbd "C-c l T l") 'lsp-lens-mode)
(global-set-key (kbd "C-c l T a") 'lsp-modeline-code-actions-mode)
(global-set-key (kbd "C-c l T D") 'lsp-modeline-diagnostics-mode)
(global-set-key (kbd "C-c l r o") 'lsp-organize-imports)
(global-set-key (kbd "C-c l r r") 'lsp-rename)
(global-set-key (kbd "C-c l w r") 'lsp-restart-workspace)
(global-set-key (kbd "C-c l w q") 'lsp-shutdown-workspace)
(global-set-key (kbd "C-c l g e") 'lsp-treemacs-errors-list)
(global-set-key (kbd "C-c l g h") 'lsp-treemacs-call-hierarchy)
(global-set-key (kbd "C-c l F b") 'lsp-workspace-blocklist-remove)
(global-set-key (kbd "C-c l F a") 'lsp-workspace-folders-add)
(global-set-key (kbd "C-c l F r") 'lsp-workspace-folders-remove)
(global-set-key (kbd "C-c l w r") 'lsp-workspace-restart)

;; (global-set-key (kbd "C-c C-c s") 'mc/edit-beginnings-of-lines)
;; (global-set-key (kbd "C-c C-c e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-c C-c l") 'mc/edit-lines)

;;  ____        _           _       ____                           
;; / ___| _ __ | | __ _ ___| |__   / ___|  ___ _ __ ___  ___ _ __  
;; \___ \| '_ \| |/ _` / __| '_ \  \___ \ / __| '__/ _ \/ _ \ '_ \ 
;;  ___) | |_) | | (_| \__ \ | | |  ___) | (__| | |  __/  __/ | | |
;; |____/| .__/|_|\__,_|___/_| |_| |____/ \___|_|  \___|\___|_| |_|
;;       |_|                                                       

(recentf-mode 1)
(setq recentf-max-saved-items 100)

(setq dashboard-startup-banner "~/.emacs.d/emacs_splash.png")
(setq dashboard-icon-type 'all-the-icons)

(setq dashboard-items '((bookmarks . 10)
			(recents   . 15)))

(setq dashboard-startupify-list '(dashboard-insert-banner
				  dashboard-insert-newline
				  dashboard-insert-banner-title
				  dashboard-insert-newline
				  dashboard-insert-init-info
				  dashboard-insert-items
				  dashboard-insert-newline
				  dashboard-insert-footer))

;;   ____          _                  
;;  / ___|   _ ___| |_ ___  _ __ ___  
;; | |  | | | / __| __/ _ \| '_ ` _ \ 
;; | |__| |_| \__ \ || (_) | | | | | |
;;  \____\__,_|___/\__\___/|_| |_| |_|
;; The text below is added automatically by Emacs
;; Do not edit

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
