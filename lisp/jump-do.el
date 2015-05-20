;;; Summary --- jump-do.el -*- lexical bindings: t -*-

;;; Commentary:

;;; Bug: `jump-do-after-jump' is not removed from
;;; `ace-jump-mode-end-hook' if the ace-jump is aborted or not completed
;;; normally.

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
  (ace-jump-mode-pop-mark))

;;;###autoload
(defun jump-do (fn)
  "Jump with `ace-jump-mode', call FN, and jump back.
FN should be a function, and will be called with no arguments.  Also, FN
shouldn't call `jump-do' itself."
  (setq jump-do-action fn)
  (add-hook 'ace-jump-mode-end-hook #'jump-do-after-jump)
  (call-interactively #'ace-jump-mode))

(defmacro define-jump-do-command (name fn &optional doc)
  "Define a command for jump-doing FN.
NAME is the name for the new command.
DOC is a docstring for the new command.  Should be nil if no docstring
is required.
FN is the function to call with `jump-do'."
  (let* ((jump-do-fn fn)
         (jump-do-doc (or doc
                          (format "Call `%s' with point at some location.
The location is chosen with `ace-jump-mode'.  When invoking this command, a
prefix argument has the same meaning as in `ace-jump-mode'."
                                  jump-do-fn))))
    `(defun ,name ()
       ,jump-do-doc
       (interactive)
       (jump-do ,jump-do-fn))))

;;;###autoload
(define-jump-do-command jump-do-anaconda-usages #'anaconda-mode-usages)
;;;###autoload
(define-jump-do-command jump-do-anaconda-view-doc #'anaconda-mode-view-doc)

;; (defun jump-hc-do (fn)
;;   "Jump with `helm-cscope', perform FN and jump back."
;;   (call-interactively #'helm-cscope-find-this-symbol)
;;   (funcall fn)
;;   (helm-cscope-pop-mark))

;; (defun jump-hc-echo ()
;;   (interactive)
;;   (jump-hc-do (lambda () (message "Hello from %s" (point)))))


(provide 'jump-do)
;;; jump-do.el ends here
