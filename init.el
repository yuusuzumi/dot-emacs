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
(use-package all-the-icons
	:straight t)

(use-package buffer-expose
	:straight t
	:config
	(buffer-expose-mode 1))

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

(use-package counsel
	:straight t
	:config
	(counsel-mode 1)
	(ivy-mode 1))

(use-package doom-modeline
  :straight t
	:init
	(setq doom-modeline-height 30)
	(setq doom-modeline-buffer-encoding t)
	(setq doom-modeline-buffer-modification-icon t)
	(setq doom-modeline-buffer-file-name-style 'relative-from-project)
	(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
	(setq doom-modeline-enable-word-count t)
	(setq doom-modeline-env-version t)
	(setq doom-modeline-indent-info t)
	(setq doom-modeline-lsp t)
	(setq doom-modeline-vcs-max-length 12)
	(setq doom-modeline-workspace-name t)
	
  :config
  (doom-modeline-mode 1))

(use-package doom-themes
  :straight t
	:init
	(setq doom-themes-enable-bold t
				doom-themes-enable-italic t)
  :config
	(doom-themes-neotree-config)
	(doom-themes-org-config)
  (load-theme 'doom-wilmersdorf t))

(use-package dumbparens
  :straight (dumbparens :type git :host github :repo "raxod502/dumbparens")
  :config
  (dumbparens-global-mode))

(use-package emojify
	:straight t
	:hook (after-init . global-emojify-mode)
	      (after-init . global-emojify-mode-line-mode)
	)

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package ivy-yasnippet
	:straight t
	:defer 1
	:hook (prog-mode . yas-minor-mode))

(use-package neotree
	:straight t
	:after projectile
	:commands (neotree-show neotree-hide neotree-dir neotree-find)
	:init
	(setq neo-create-file-auto-open t)
	(setq neo-autorefresh nil)
	(setq neo-show-hidden-files t)
	(setq neo-smart-open t)
	(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	(setq-default neo-keymap-style 'concise)
	:bind (([f8] . neotree-toggle)
				 ([f9] . neotree-projectile-toggle)
				 :map neotree-mode-map
				 ("RET" . neotree-enter-hide)
				 ("a" . neotree-hidden-file-toggle)
				 ("<left>" . neotree-select-up-node)
				 ("<right>" . neotree-change-root))
	:config
	(defun neotree-text-scale ()
		"text scale for neotree."
		(interactive)
		(text-scale-adjust 0)
		(text-scale-decrease 1)
		(message nil))
	(add-hook 'neo-after-create-hook
						(lambda (_)
							(call-interactively 'neotree-text-scale)))
	(defun neo-open-file-hide (full-path &optional arg)
		"open file and hiding neotree. the description of full-path & arg is in `neotree-enter'."
		(neo-global--select-mru-window arg)
		(find-file full-path)
		(neotree-hide))
	(defun neotree-enter-hide (&optional arg)
		"neo-open-file-hide if file, neo-open-dir if dir. the description of arg is in `neo-buffer--execute'."
		(interactive "P")
		(neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))

(use-package nyan-mode
	:straight t
	:config
	(nyan-mode 1)
	(nyan-toggle-wavy-trail)
	(nyan-start-animation))

(use-package projectile
	:straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restart-emacs
	:straight t)

(use-package sky-color-clock
	:straight (sky-color-clock :type git :host github :repo "zk-phi/sky-color-clock")
	:init
	(setq sky-color-clock-enable-emoji-icon t)
	(setq sky-color-clock-enable-temperature-indicator t)
	(setq sky-color-clock-format "%d %H:%M")
	:config
	(sky-color-clock-initialize 35)
	(push '(:eval (sky-color-clock)) (default-value 'mode-line-format)))

;; (use-package treemacs
;; 	:straight t)

(use-package vimish-fold
	:straight t)

(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package yasnippet
	:straight t
	:defer 1
	:hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
	:straight t
	:after yasnippet
	:config
	(yasnippet-snippets-initialize))


;; basic setting
(column-number-mode 1)
(global-linum-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
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

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-attribute 'default nil :family "Ricty Diminished with Fira Code" :height 110)
(set-frame-size (selected-frame) 145 30)
