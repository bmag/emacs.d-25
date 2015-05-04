;;; nav-mode.el --- Navigate without modifier keys -*- lexical-binding: t -*-

;;; Commentary:
;; `nav-mode' provides a minor mode for issuing commands without the
;; need of modifier keys.  When `nav-mode' is enabled, you can for
;; example start a search by pressing "/".
;;
;; `nav-mode' can be enabled locally `(nav-mode)' or globally
;; `(global-nav-mode)'.

;;; Code:

(define-prefix-command 'nav-mode-buffer-prefix 'nav-mode-buffer-map)
(define-prefix-command 'nav-mode-search-prefix 'nav-mode-search-map)
(define-prefix-command 'nav-mode-goto-prefix 'nav-mode-goto-map)
(define-prefix-command 'nav-mode-prefix 'nav-mode-map)

(let ((map nav-mode-map))
  (define-key map (kbd "/") #'isearch-forward)
  (define-key map (kbd "SPC") #'set-mark-command)
  (define-key map (kbd "j") #'ace-jump-mode)
  (define-key map (kbd "J") #'ace-jump-mode-pop-mark)
  (define-key map (kbd "n") #'next-line)
  (define-key map (kbd "p") #'previous-line)
  (define-key map (kbd "u") #'universal-argument)

  (define-key map (kbd "b") #'nav-mode-buffer-prefix)
  (let ((map nav-mode-buffer-map))
    (define-key map (kbd "b") #'ido-switch-buffer)
    (define-key map (kbd "k") #'ido-kill-buffer)
    (define-key map (kbd "n") #'next-buffer)
    (define-key map (kbd "p") #'previous-buffer))
  
  (define-key map (kbd "s") #'nav-mode-search-prefix)
  (let ((map nav-mode-search-map))
    (define-key map (kbd "c") #'lazy-highlight-cleanup)
    (define-key map (kbd "i") #'idomenu)
    (define-key map (kbd "s") #'occur))

  (define-key map (kbd "g") #'nav-mode-goto-prefix)
  (let ((map nav-mode-goto-map))
    (define-key map (kbd "g") #'xref-find-definitions)
    (define-key map (kbd "f") #'ffap)))

(defvar nav-mode--saved-lazy-highlight lazy-highlight-cleanup)

(defvar nav-mode-temp nil
  "Non-nil if `nav-mode' is temporarily disabled.")

(defface nav-mode-line-face
  '((t :foreground "white"
       :background "dark green"))
  "Face for `nav-mode''s in the mode-line.")

;;;###autoload
(define-minor-mode nav-mode
  nil
  :lighter (" " (:propertize "NAV" face nav-mode-line-face))
  
  (if nav-mode
      (nav-mode--enable)
    (nav-mode--disable)))


;;;###autoload
(define-globalized-minor-mode global-nav-mode nav-mode nav-mode-safe)

(defun nav-mode-safe ()
  "Turn on `nav-mode' if logical for current buffer."
  (unless (minibufferp)
    (nav-mode)))

(defun nav-mode--enable ()
  "Things to do when `nav-mode' is turned on."
  (mapc #'make-local-variable
        '(nav-mode--saved-lazy-highlight
          lazy-highlight-cleanup))
  
  (let ((view-read-only nil))
    (read-only-mode))
  (setq nav-mode--saved-lazy-highlight lazy-highlight-cleanup)
  (setq lazy-highlight-cleanup nil)
  (unless nav-mode-temp
    (add-hook 'iedit-mode-hook #'nav-mode-temp-disable)
    (add-hook 'iedit-mode-end-hook #'nav-mode-temp-enable)))

(defun nav-mode-enable ()
  "Turn `nav-mode' on."
  (nav-mode))

(defun nav-mode--disable ()
  "Things to do when `nav-mode' is turned off."
  (read-only-mode -1)
  (setq lazy-highlight-cleanup nav-mode--saved-lazy-highlight)
  (unless nav-mode-temp
    (remove-hook 'iedit-mode-hook #'nav-mode-temp-disable)
    (remove-hook 'iedit-mode-end-hook #'nav-mode-temp-enable)))

(defun nav-mode-disable ()
  "Turn off `nav-mode'."
  (nav-mode -1))

(defun nav-mode-temp-disable ()
  "Disable `nav-mode' temporarily."
  (setq nav-mode-temp t)
  (nav-mode-disable))

(defun nav-mode-temp-enable ()
  "Enable `nav-mode' temporarily."
  (nav-mode-enable)
  (setq nav-mode-temp nil))

(provide 'nav-mode)
;;; nav-mode.el ends here
