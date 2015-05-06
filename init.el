(defconst init-start-time (current-time))

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(column-number-mode)
(show-paren-mode)
(windmove-default-keybindings)
(winner-mode)

(defalias 'yes-or-no-p #'y-or-n-p)

(push (file-name-as-directory (expand-file-name "lisp" user-emacs-directory)) load-path)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

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
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (setq company-idle-delay 0.3))

(use-package eclim
  :defer 1
  :config
  (setq eclim-eclipse-dirs '("/opt/eclipse"))
  (setq eclim-executable "/opt/eclipse/eclim")
  (global-eclim-mode)
  (use-package eclimd)
  (use-package company-emacs-eclim
    :config
    (company-emacs-eclim-setup)))

(use-package flycheck
  :defer 1
  :config
  (global-flycheck-mode))

(use-package flx-ido
  :defer 1
  :config
  (flx-ido-mode)
  (setq ido-enable-flex-matching t
        ido-use-faces nil))

(use-package gh-md :defer t)

(use-package ido
  :defer 1
  :config
  (ido-mode)
  (ido-everywhere)
  (setq ido-enable-flex-matching t))

(use-package ido-vertical-mode
  :defer 1
  ;; faces: ido-vertical-{first,only}-match-face
  :config
  (ido-vertical-mode))

(use-package idomenu
  :bind ("C-c i" . idomenu))

(use-package iedit
  :defer 1)

(use-package imenu-list
  :defer t
  :load-path "modules/imenu-list/")

(use-package jump-do
  :commands (jump-do-anaconda-view-doc jump-do-anaconda-usages)
  :init
  (with-eval-after-load 'anaconda-mode
    (bind-key "C-c h" #'jump-do-anaconda-view-doc anaconda-mode-map)
    (bind-key "C-c u" #'jump-do-anaconda-usages anaconda-mode-map)))

(use-package magit
  :defer t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (defun magit-switch-fullscreen (buffer)
    (prog1 (switch-to-buffer buffer)
      (delete-other-windows)))
  (setq magit-restore-window-configuration t
        magit-status-buffer-switch-function #'magit-switch-fullscreen))

(use-package markdown-mode :defer t)

(use-package markdown-toc :defer t)

(use-package nav-mode
  ;; bug: first time "M-o" is pressed, it fails because of
  ;; "Wrong type argument: commandp, nav-mode-prefix"
  ;; second time "M-o" is pressed, it succeeds.
  ;; seems that Emacs loads `nav-mode-prefix' after it checks whether it's a
  ;; command, instead of before.
  :bind (("M-m" . global-nav-mode)
         ("M-o" . nav-mode-prefix)))

(use-package org-bullets
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package paredit
  :diminish paredit-mode
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package paredit-everywhere
  :defer t
  :init
  (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

(use-package persp-projectile :defer 1)

(use-package perspective
  :defer 1
  :init
  (persp-mode))

(use-package projectile
  :diminish projectile-mode
  :defer 1
  :config
  (projectile-global-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package recentf
  :defer 1
  :bind ("C-x C-r" . ido-recentf)
  :config
  (defun ido-recentf ()
    "Use `ido' to open a recent file."
    (interactive)
    (find-file (ido-completing-read "Find recent: " recentf-list)))
  (recentf-mode)
  (setq recentf-save-file
        (expand-file-name "recent-files" user-emacs-directory)))

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

(use-package xref
  :defer t
  :config
  (use-package xref-conf
    :config
    (xref-conf-install-python)))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :config
  (yas-global-mode))

(defconst init-finish-time (current-time))
(defconst init-total-time (time-subtract init-finish-time init-start-time))
(message "Loaded init (%s seconds)" (time-to-seconds init-total-time))
(put 'dired-find-alternate-file 'disabled nil)
