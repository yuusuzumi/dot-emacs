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
  (doom-modeline-mode 1)
	(doom-modeline-def-modeline 'my-modeline
		'(matches buffer-info remote-host buffer-position parrot selection-info)
		'(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))
	(defun setup-custom-doom-modeline ()
		(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)))

(use-package doom-themes
  :straight t
	:init
	(setq doom-themes-enable-bold t
				doom-themes-enable-italic t)
  :config
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

(use-package nyan-mode
	:straight t
	:config
	(nyan-mode 1)
	(nyan-toggle-wavy-trail)
	(nyan-start-animation))

(use-package origami
	:straight t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ranger
	:straight t)

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

(set-face-attribute 'default nil :family "Ricty Diminished with Fira Code" :height 110)
(set-frame-size (selected-frame) 145 30)
