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

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" . yaml-mode))


;; basic setting
(setq auto-save-default nil)
(setq make-backup-files nil)

(set-face-attribute 'default nil :family "Ricty Diminished with Fira Code" :height 110)
(set-frame-size (selected-frame) 145 30)
