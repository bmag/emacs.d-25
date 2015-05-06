;;; Summary --- a utility for building custom frame headers -*- lexical-binding: t -*-

;;; Commentary: header-bar.el is a utility for bulding custom frame
;;; headers.  It is a bundled with buffer-header, which uses header-bar
;;; to build a header displaying existing buffers.

;;; Bugs:
;;; MAX-HEIGHT for `hb-update-window' doesn't shrink window if changed
;;; to a number smaller than current window's height.

;;; Code:

(require 'subr-x)

(defgroup header-bar nil
  "Frame header."
  :group 'frames)

(defface header-bar-face
  '((((class color) (background light)) :foreground "light blue" :background "dark blue")
    (((class color) (background dark)) :foreground "black" :background "light blue"))
  "Face for the header-bar window."
  :group 'header-bar)

(defvar hb-separator "|")
(defvar hb-end-char ?#)
(defvar hb-insert-text-function #'hb-insert-text-default)
(defvar hb-insert-single-text-function #'insert)

(defvar hb-windows nil)

(defun hb-normalize-buffer (buffer-or-name)
  (if (bufferp buffer-or-name)
      buffer-or-name
    (get-buffer-create buffer-or-name)))

(defun hb-get-or-create-window (buffer-or-name)
  (let* ((buffer (hb-normalize-buffer buffer-or-name))
         (window (get-buffer-window buffer)))
    (if window
        window
      (hb-create-window buffer))))

(defun hb-create-window (buffer-or-name)
  (let* ((buffer (hb-normalize-buffer buffer-or-name))
         (window (split-window (frame-root-window) -1 'above))
         (delta (window-resizable window (- (- (window-height window) 1))
                                  nil 'safe)))
    (window-resize window delta nil 'safe)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (set-window-parameter window 'no-other-window t)
    (with-current-buffer buffer
      (setq-local window-size-fixed 'height)
      (setq-local mode-line-format nil)
      (setq-local cursor-in-non-selected-windows nil))
    (cl-pushnew window hb-windows)
    window))

(defun hb-resize-window-to-buffer (window max-height)
  (let ((window-size-fixed nil))
    (fit-window-to-buffer window max-height window-safe-min-height)))

(defun hb-update-window (buffer-or-name list-of-text &optional max-height)
  (let* ((buffer (hb-normalize-buffer buffer-or-name))
         (window (get-buffer-window buffer)))
    (if window
        (progn
          (funcall hb-insert-text-function buffer window list-of-text)
          (with-current-buffer buffer
            (hb-resize-window-to-buffer window max-height)))
      (error "Buffer %s has no window" buffer))))

(defun hb-clean-window-list ()
  (setq hb-windows (cl-delete-if-not #'window-live-p hb-windows)))
(add-hook 'window-configuration-change-hook #'hb-clean-window-list)

(defun hb-window-at-top-p (window)
  "Return non-nil if WINDOW is a header window in the top of the frame.
WINDOW has be a member of `hb-windows', span the frame from left to 
right, and be either the top-most window or below another window that
satisfies `hb-window-at-top-p'."
  (and (member window hb-windows)
       (window-at-side-p window 'left)
       (window-at-side-p window 'right)
       (or (window-at-side-p window 'top)
           (let ((window-above (window-in-direction 'above window t)))
             (and window-above (hb-window-at-top-p window-above))))))

(defun hb-move-to-top ()
  (cl-loop for window in (window-list)
           when (and (member window hb-windows)
                     (not (hb-window-at-top-p window)))
           do
           (let ((buffer (window-buffer window))
                 (height (window-height window)))
             (delete-window window)
             (when-let ((new-window (hb-create-window buffer)))
               (hb-resize-window-to-buffer new-window height)))))
(add-hook 'window-configuration-change-hook #'hb-move-to-top)

(defun hb-insert-text-default (buffer window list-of-text)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (width (window-width window)))
      (erase-buffer)
      (while list-of-text
        (while (and list-of-text
                    (< (+ (length (car list-of-text))
                          (current-column))
                       width))
          (funcall hb-insert-single-text-function (car list-of-text))
          (insert (propertize hb-separator 'face 'header-bar-face))
          (setq list-of-text (cdr list-of-text)))
        ;; delete separator at end of line
        (delete-char -1)
        (insert (propertize (make-string (- width (current-column)) hb-end-char) 'face 'header-bar-face))
        (insert (propertize "\n" 'face 'header-bar-face)))
      ;; delete final new-line
      (delete-char -1))))



;;; buffer-header --- show buffer buttons in header-bar

(defgroup buffer-header nil
  "Show buffers at frame's header."
  :group 'header-bar)

(defface bh-button-face
  '((t :inherit header-bar-face
       :underline (:color foreground-color :style line)
       ;; :foreground "black"
       ;; :background "orange"
       ))
  "Face for buffer-header buttons."
  :group 'header-bar)

(defcustom bh-ignore-buffer-regexps
  '("^\\*magit" "^\\*Shell Command Output\\*$")
  "List of regular expressions for ignoring buffers by name."
  :group 'header-bar
  :type '(repeat string))

(defcustom bh-max-height 10
  "Maximum height for the buffer header."
  :group 'header-bar
  :type 'number)

(defconst bh-buffer-name "buffer-header")

(defun bh-insert-single-text (buffer-or-name)
  (let ((buffer (hb-normalize-buffer buffer-or-name)))
    (insert-button (buffer-name buffer)
                   'face 'bh-button-face
                   'buffer buffer)))

(defun bh-get-buffers ()
  "Return list of buffer names to be displayed."
  (cl-loop for buffer in (buffer-list)
           unless (bh-ignore-buffer buffer)
           collect (buffer-name buffer)))

;; TODO: make this function smarter
;; `bh-ignore-buffer-predicates'
(defun bh-ignore-buffer (buffer)
  (let ((buffer-name (buffer-name buffer)))
    (or (minibufferp buffer)
        (string-prefix-p " " buffer-name)
        (cl-member-if (lambda (regexp)
                        (string-match-p regexp buffer-name))
                      bh-ignore-buffer-regexps))))

(defun bh-open-buffer-at-point (&optional pos)
  (let* ((pos (or pos (point)))
         (button (button-at pos))
         (buffer (and button
                      (button-get button 'buffer))))
    (pop-to-buffer buffer)))

(defun bh-display ()
  (let ((hb-insert-single-text-function #'bh-insert-single-text)
        (hb-separator " ")
        (hb-end-char 32))
    (hb-get-or-create-window bh-buffer-name)
    (hb-update-window bh-buffer-name (bh-get-buffers) bh-max-height)))
;; yes?
;;(add-hook 'buffer-list-update-hook #'bh-display)

(provide 'header-bar)
;;; header-bar.el ends here
