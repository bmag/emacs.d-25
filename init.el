(defconst init-start-time (current-time))

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(column-number-mode)
(ido-mode)
(setq ido-enable-flex-matching t)
(show-paren-mode)
(windmove-default-keybindings)
(winner-mode)

(push (file-name-as-directory (expand-file-name "lisp" user-emacs-directory)) load-path)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(require 'use-package)

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)
	 ("C-c C-SPC" . ace-jump-mode-pop-mark)))

(use-package anaconda
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company
  :diminish company-mode
  :functions company-complete-common-or-cycle
  :defer 1
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
  (setq company-idle-delay 0.3))

(use-package flycheck
  :defer 1
  :config
  (global-flycheck-mode))

(use-package iedit
  :defer 1)

(use-package magit :defer t)

(use-package paredit
  :diminish paredit-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package paredit-everywhere
  :defer t
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)))

(use-package window-purpose
  :defer 1
  :config
  (purpose-mode)
  (push '("*Pp Macroexpand Output*" . general) purpose-user-name-purposes)
  (purpose-compile-user-configuration))

(use-package xcscope
  :defer t
  :init
  (add-hook 'python-mode-hook #'cscope-minor-mode)
  (setq cscope-option-do-not-update-database t
	cscope-display-cscope-buffer nil))

(use-package xref-conf
  :defer 1
  :config
  (xref-conf-install-python))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :config
  (yas-global-mode))

(defconst init-finish-time (current-time))
(defconst init-total-time (time-subtract init-finish-time init-start-time))
(message "Loaded init (%s seconds)" (time-to-seconds init-total-time))
