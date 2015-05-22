;;; TODO: let user decide one of several jump functions
;;; (`avy-goto-char', `avy-goto-word-0', etc.).
;;; TODO: use a better method to jump back

(defun avy-jump-do (fn)
  "Jump to a location, call FN, and jump back.
FN is a function to be called with no arguments.  FN is called only if
the jump was successful.
The jump is done with `avy-goto-char'."
  (let ((window (selected-window))
        (pos (point))
        (result (call-interactively #'avy-goto-char)))
    (unless (or (null result)
                (and (stringp result)
                     (string= result "zero candidates")))
      (unwind-protect
          (funcall fn)
        (select-window window t)
        (goto-char pos)))))

(defmacro define-avy-jump-do-command (name fn &optional doc)
  (let ((avy-jump-do-doc (or doc
                             (format "Call `%s' with point at some location.
The location is chosen with `avy-goto-char'."
                                     fn))))
    `(defun ,name ()
       ,avy-jump-do-doc
       (interactive)
       (avy-jump-do ,fn))))

(define-avy-jump-do-command ajd-anaconda-usages #'anaconda-mode-usages)
(define-avy-jump-do-command ajd-anaconda-view-doc #'anaconda-mode-view-doc)
(define-avy-jump-do-command ajd-elisp-describe #'describe-function-or-variable)
