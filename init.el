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

(use-package magit           :ensure t)
(use-package which-key       :ensure t :config (which-key-mode))
(use-package move-text       :ensure t :bind (("M-<up>" . move-text-up) ("M-<down>" . move-text-down)))
(use-package dashboard       :ensure t :config (dashboard-setup-startup-hook))
(use-package all-the-icons   :ensure t)
(use-package nerd-icons      :ensure t)
(use-package vterm           :ensure t)
(use-package ivy             :ensure t :config (ivy-mode))
(use-package helm            :ensure t :config (helm-mode))

(use-package lsp-mode :ensure t :hook (java-mode . lsp) :commands lsp)
(use-package lsp-java :ensure t :after lsp-mode :config (setq lsp-java-format-on-type-enabled t) (setq lsp-java-save-actions-organize-imports t))
(use-package company :ensure t :hook (lsp-mode . company-mode) :config (setq company-minimum-prefix-length 1) (setq company-idle-delay 0.0))

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

(defvar line-length 100)

(setq whitespace-line-column line-length)

(setq-default fill-column line-length)
(setq display-fill-column-indicator-character ?\u2502)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;   ____          _      
;;  / ___|___   __| | ___ 
;; | |   / _ \ / _` |/ _ \
;; | |__| (_) | (_| |  __/
;;  \____\___/ \__,_|\___|

;; General
(setq indent-tabs-mode t)
(setq tab-width 4)

;; C/C++
(setq c-basic-offset 4)
(setq c-indent-level 4)

;; Java
(setq java-indent-level 2)

;; Python
(setq python-indent-offset 4)

;;  __  __                          
;; |  \/  | __ _  ___ _ __ ___  ___ 
;; | |\/| |/ _` |/ __| '__/ _ \/ __|
;; | |  | | (_| | (__| | | (_) \__ \
;; |_|  |_|\__,_|\___|_|  \___/|___/

(global-set-key (kbd "C-w") 'whitespace-mode)
(global-set-key (kbd "M-c") 'compile)

(defun comment-current-line ()
  "Comment out the current line."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (if (not (string-blank-p line))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (message "No line to comment out."))))

(global-set-key (kbd "C-;") 'comment-current-line)

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
;; The below text is added automatically by Emacs
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
