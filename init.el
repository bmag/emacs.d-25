;;; -*- lexical bindings: t -*-
(defconst init-start-time (current-time))

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file)

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
  :defer t
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode-pop-mark)))

(use-package anaconda
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package anzu
  :defer 1
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(use-package comint
  :defer t
  :config
  ;; originally on C-c M-r and C-c M-s
  (define-key comint-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
  (define-key comint-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)
  ;; originally on M-p and M-n
  (define-key comint-mode-map (kbd "C-c M-r") #'comint-previous-input)
  (define-key comint-mode-map (kbd "C-c M-s") #'comint-next-input))

(use-package company
  :diminish company-mode
  :defer 1
  :config
  (global-company-mode)
  (bind-key (kbd "<tab>") #'company-complete-common-or-cycle company-active-map)
  (bind-key (kbd "<backtab>") #'company-select-previous company-active-map)
  (bind-key "C-n" #'company-select-next company-active-map)
  (bind-key "C-p" #'company-select-previous company-active-map)
  (bind-key "C-d" #'company-show-doc-buffer company-active-map)
  (bind-key "M-." #'company-show-location company-active-map)
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

(use-package eyebrowse :defer t)

(use-package flycheck
  :defer 1
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package flx-ido
  :defer 1
  :config
  (flx-ido-mode)
  (setq ido-enable-flex-matching t
        ido-use-faces nil))

(use-package gh-md :defer t)

(use-package git-gutter
  :if (not (display-graphic-p))
  :diminish git-gutter-mode
  :defer 1
  :config
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :if (display-graphic-p)
  :diminish git-gutter-mode
  :defer 1
  :config
  (fringe-helper-define 'git-gutter-fr:added nil
    "..X.."
    "..X.."
    "XXXXX"
    "..X.."
    "..X..")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "..X.."
    ".XXX."
    "XXXXX"
    ".XXX."
    "..X..")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "....."
    "....."
    "XXXXX"
    "....."
    ".....")
  (setq git-gutter-fr:side 'right-fringe)
  (global-git-gutter-mode))

(use-package helm
  :defer 1
  :bind ("C-c h" . helm-command-prefix)
  :defines (helm-ff-search-library-in-sexp
            helm-ff-file-name-history-use-recentf
            helm-recentf-fuzzy-match
            helm-buffers-fuzzy-matching
            helm-M-x-fuzzy-match
            helm-semantic-fuzzy-match
            helm-imenu-fuzzy-match)
  :config
  (use-package helm-config)
  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  ;; (bind-key "M-x" #'helm-M-x)
  ;; (bind-key "C-x b" #'helm-mini)
  ;; (bind-key "C-x C-f" #'helm-find-files)
  (bind-key "M-y" #'helm-show-kill-ring)
  ;; (bind-key "C-c i" #'helm-semantic-or-imenu)
  ;; maybe replace `helm-occur' with `helm-swoop'?
  (bind-key "C-c h o" #'helm-occur)
  (bind-key "C-h SPC" #'helm-all-mark-rings)
  (bind-key "C-c h x" #'helm-register)
  ;; (with-eval-after-load 'comint
  ;;  (bind-key "C-c C-l" #'helm-comint-input-ring comint-mode-map))
  ;; (bind-key "C-c C-l" #'helm-minibuffer-history minibuffer-local-map)
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-scroll-amount 8
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t)
  (helm-mode)
  (helm-autoresize-mode)
  ;; (helm-adaptive-mode) ; causes some problem with `custom-set-variables'
  (with-eval-after-load 'window-purpose
    (defun display-helm-at-bottom (buffer)
      (let ((window (or (purpose-display-reuse-window-buffer buffer nil)
                        (purpose-display-reuse-window-purpose buffer nil)
                        (purpose-display-at-bottom buffer nil 0.4))))
        (if window
            (progn
              (select-window window)
              (prog1
                  (switch-to-buffer buffer t t)
                (purpose-set-window-purpose-dedicated-p window t)))
          (prog1
              (funcall #'helm-default-display-buffer buffer)
            (purpose-set-window-purpose-dedicated-p nil t)))))
    
    (setq helm-display-function #'display-helm-at-bottom)))

(use-package helm-cscope
  :defer t
  :init
  (add-hook 'python-mode-hook #'helm-cscope-mode)
  :config
  (setq-default cscope-option-do-not-update-database t)
  (dolist (e '(("C-c s =" . helm-cscope-find-assignments-to-this-symbol) ; assignments
               ("C-c s C" . helm-cscope-find-called-function) ; called
               ("C-c s c" . helm-cscope-find-calling-this-funtcion) ; calling
               ("C-c s d" . helm-cscope-find-global-definition) ; definition
               ("C-c s g" . helm-cscope-find-global-definition) ; global
               ("C-c s e" . helm-cscope-find-egrep-pattern) ; egrep
               ("C-c s f" . helm-cscope-find-this-file) ; file
               ("C-c s i" . helm-cscope-find-files-including-file) ; includes
               ("C-c s s" . helm-cscope-find-this-symbol) ; symbol
               ("C-c s t" . helm-cscope-find-this-text-string) ; text
               ("C-c s u" . helm-cscope-pop-mark))) ; pop mark
    (bind-key (car e) (cdr e) helm-cscope-mode-map)
    ;; (bind-key (car e) (cdr e) cscope-minor-mode-map)
    ))

(use-package hydra :defer 1
  :config
  (defhydra cycle-buffer (global-map "C-c n")
    ("<right>" (lambda () (interactive)
                 (next-buffer)
                 (message "%s" (current-buffer)))
     "next")
    ("<left>" (lambda () (interactive)
                (previous-buffer)
                (message "%s" (current-buffer)))
     "previous"))
  
  (defhydra cycle-persp (global-map "C-c m")
    ("<right>" (lambda () (interactive)
                 (persp-next)
                 (message "%s" (persp-name persp-curr)))
     "next")
    ("<left>" (lambda () (interactive)
                 (persp-prev)
                 (message "%s" (persp-name persp-curr)))
     "previous")))

;; (defun buffer-switcher (buffer)
;;   "Choose a BUFFER and choose where to open it."
;;   (interactive "BBuffer: ")
;;   (lexical-let ((new-buffer (get-buffer-create buffer)))
;;     (defhydra buffer-switcher (:color blue :hint nil)
;;       "
;; _b_: same window  _o_: other window
;; _j_: below  _k_: above  _h_: left _l_: right
;; _J_: bottom  _K_: top  _H_: left-most  _L_: right-most
;; "
;;       ("b" (lambda () (interactive) (switch-to-buffer new-buffer)))
;;       ("o" (lambda () (interactive) (switch-to-buffer-other-window new-buffer)))
;;       ("J" (lambda () (interactive)
;;              (select-window (purpose-display-at-bottom new-buffer nil))))
;;       ("K" (lambda () (interactive)
;;              (select-window (purpose-display-at-top new-buffer nil))))
;;       ("H" (lambda () (interactive)
;;              (select-window (purpose-display-at-left new-buffer nil))))
;;       ("L" (lambda () (interactive)
;;              (select-window (purpose-display-at-right new-buffer nil))))
;;       ("j" (lambda () (interactive)
;;              (select-window (purpose-display--at
;;                              #'ignore
;;                              (lambda () (split-window nil nil 'below))
;;                              new-buffer nil))))
;;       ("k" (lambda () (interactive)
;;              (select-window (purpose-display--at
;;                              #'ignore
;;                              (lambda () (split-window nil nil 'above))
;;                              new-buffer nil))))
;;       ("h" (lambda () (interactive)
;;              (select-window (purpose-display--at
;;                              #'ignore
;;                              (lambda () (split-window nil nil 'left))
;;                              new-buffer nil))))
;;       ("l" (lambda () (interactive)
;;              (select-window (purpose-display--at
;;                              #'ignore
;;                              (lambda () (split-window nil nil 'right))
;;                              new-buffer nil))))))
;;   (buffer-switcher/body))

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

(use-package imenu
  :defer t
  :init
  ;; source: https://gist.github.com/jordonbiondo/6385874a70420b05de18
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'imenu-generic-expression
                           '("Packages"
                             "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))))

(use-package imenu-list :defer t)

(use-package jump-do
  :commands (jump-do-anaconda-view-doc jump-do-anaconda-usages)
  :defines (anaconda-mode-map)
  :init
  (with-eval-after-load 'anaconda-mode
    (bind-key "C-c h" #'jump-do-anaconda-view-doc anaconda-mode-map)
    (bind-key "C-c u" #'jump-do-anaconda-usages anaconda-mode-map)))

(use-package magit
  :defer t
  :defines (magit-last-seen-setup-instructions)
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
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  :config
  (unbind-key "M-s" paredit-mode-map)
  (bind-key "M-s M-s" #'paredit-splice-sexp paredit-mode-map))

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

(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (with-eval-after-load 'window-purpose
    (purpose-set-extension-configuration
     :common-lisp
     (purpose-conf :name-purposes '(("*sly-description*" . general)))))
  (use-package sly-company
    :defer t
    :init
    (add-hook 'sly-mode-hook #'sly-company-mode)))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package window-purpose
  :load-path "~/emacs-purpose"
  :defer 1
  :config
  (purpose-mode)
  (bind-key "C-c b" #'purpose-switch-buffer-with-purpose purpose-mode-map)
  (setq purpose-default-layout-file (file-name-as-directory (expand-file-name "layouts" user-emacs-directory)))
  ;; (push '("*Pp Macroexpand Output*" . general) purpose-user-name-purposes)
  ;; (purpose-compile-user-configuration)

  (use-package window-purpose-x
    :load-path "~/emacs-purpose"
    :config
    (cl-pushnew "*Pp Macroexpand Output*" purpose-x-popwin-buffer-names
                :test 'equal)
    (purpose-x-popwin-setup)))

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

(use-package thingatpt
  :defer 1
  :bind (("M-s ." . my-isearch-forward-symbol-at-point)
         ("M-s ," . my-isearch-forward-word-at-point))
  :init
  (defun my-isearch-forward-word-at-point ()
    "Search for word at point."
    (interactive)
    (let ((word (thing-at-point 'word t))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (isearch-mode t nil nil nil t)
            (when (< (car bounds) (point))
              (goto-char (car bounds)))
            (isearch-yank-string word))
        (user-error "No word at point"))))

  (defun my-isearch-forward-symbol-at-point ()
    "Search for symbol at point."
    (interactive)
    (let ((symbol (thing-at-point 'symbol t))
          (bounds (bounds-of-thing-at-point 'symbol)))
      (if symbol
          (progn
            (isearch-mode t nil nil nil 'isearch-symbol-regexp)
            (when (< (car bounds) (point))
              (goto-char (car bounds)))
            (isearch-yank-string symbol))
        (user-error "No symbol at point")))))

(defun load-common-lisp-layout ()
  (remove-hook 'sly-connected-hook #'load-common-lisp-layout)
  (purpose-load-window-layout
   (expand-file-name "common-lisp.window-layout" purpose-default-layout-file)))

(defun open-common-lisp-perspective ()
  (interactive)
  (persp-switch "common-lisp")
  (add-hook 'sly-connected-hook #'load-common-lisp-layout)
  (sly))

(defun post-theme-init (theme)
  "Personal additions to themes."
  (cond
   ((eql theme 'tangotango)
    (custom-theme-set-faces
     'tangotango
     '(rainbow-delimiters-depth-1-face ((t (:foreground "dark gray"))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground "gainsboro"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "salmon"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "burlywood"))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground "dark sea green"))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground "RoyalBlue1"))))
     '(hydra-face-blue ((t (:foreground "#0080FF"))))))))

(defun load-theme-and-init (theme)
  "Load THEME and call `post-theme-init'."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (load-theme theme)
  (post-theme-init theme))

(defconst init-finish-time (current-time))
(defconst init-total-time (time-subtract init-finish-time init-start-time))

(put 'dired-find-alternate-file 'disabled nil)
