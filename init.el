;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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


;; package
(use-package buffer-expose
	:straight t)

(use-package color-identifiers-mode
  :straight t
  :hook (after-init . global-color-identifiers-mode))

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  :config
  (company-tng-configure-default))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-wilmersdorf t))

(use-package dumbparens
  :straight (dumbparens :type git :host github :repo "raxod502/dumbparens")
  :config
  (dumbparens-global-mode))

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package nyan-mode
	:straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package sky-color-clock
	:straight (sky-color-clock :type git :host github :repo "zk-phi/sky-color-clock"))

(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package yasnippet
	:straight t)

(use-package yasnippet-snippets
	:straight t)


;; basic setting
(column-number-mode 1)
(global-linum-mode 1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)
(show-paren-mode 1)
(tool-bar-mode -1)
(transient-mark-mode 1)

(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
(setq ring-bell-function 'ignore)

(setq-default tab-width 2)

(set-face-attribute 'default nil :family "Ricty Diminished with Fira Code" :height 110)
(set-frame-size (selected-frame) 145 30)
