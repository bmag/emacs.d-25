;;; Summary --- jump-do.el -*- lexical bindings: t -*-

;;; Commentary:

;;; Code:

(defvar jump-do-action nil
  "Internal variable- function to perform after an ace-jump.")

(defun jump-do-after-jump ()
  "Call `jump-do-action' after an ace-jump.
This function is intended to be used by hooking it to
`ace-jump-mode-end-hook'.  `jump-do-after-jump' removes itself from
`ace-jump-mode-end-hook'."
  (remove-hook 'ace-jump-mode-end-hook #'jump-do-after-jump)
  (when (functionp jump-do-action)
    (with-demoted-errors
        (funcall jump-do-action))
    (setq jump-do-action nil))
  (call-interactively #'ace-jump-mode-pop-mark))

;;;###autoload
(defun jump-do (fn)
  "Jump with `ace-jump-mode', call FN, and jump back.
FN should be a function, and will be called with no arguments.  Also, FN
shouldn't call `jump-do' itself."
  (setq jump-do-action fn)
  (add-hook 'ace-jump-mode-end-hook #'jump-do-after-jump)
  (call-interactively #'ace-jump-mode))

;;;###autoload
(defun jump-do-anaconda-usages ()
  "Show usages for symbol at some location.
Location is chosen with `ace-jump-mode'.  When invoking this command, a
prefix argument has the same meaning as in `ace-jump-mode'."
  (interactive)
  (jump-do #'anaconda-mode-usages))

;;;###autoload
(defun jump-do-anaconda-view-doc ()
  "Show usages for symbol at some location.
Location is chosen with `ace-jump-mode'.  When invoking this command, a
prefix argument has the same meaning as in `ace-jump-mode'."
  (interactive)
  (jump-do #'anaconda-mode-view-doc))

(provide 'jump-do)
;;; jump-do.el ends here
