(setq my-mode-line-frame-identification
      '(:eval (if (or (null window-system)
                      (eq window-system 'pc))
                  "-%F  "
                "")))
(setq my-mode-line-sep "|")

(setq my-mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        my-mode-line-frame-identification

        my-mode-line-sep

        mode-line-buffer-identification

        my-mode-line-sep

        mode-line-position
        
        ;; mode-line-modes

        ;; mode-line-misc-info

        mode-line-end-spaces
        ))

(defvar original-mode-line
  '(:mode-line-format ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
                       (vc-mode vc-mode)
                       "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
    :mode-line-front-space (:eval (if (display-graphic-p) " " "-"))
    :mode-line-mule-info (""
                          (current-input-method
                           (:propertize
                            ("" current-input-method-title)
                            help-echo
                            (concat "Current input method: " current-input-method "\nmouse-2: Disable input method\nmouse-3: Describe current input method")
                            local-map
                            (keymap
                             (mode-line keymap
                                        (mouse-3 .
                                                 #[(e)
                                                   "\302\303!\211@\262!r\304\216\305	@\306\"\210\307 +\207"
                                                   [e save-selected-window--state internal--before-with-selected-window event-start
                                                      #[nil "\301!\207"
                                                            [save-selected-window--state internal--after-with-selected-window]
                                                            2]
                                                      select-window norecord describe-current-input-method]
                                                   3 nil "e"])
                                        (mouse-2 .
                                                 #[(e)
                                                   "\302\303!\211@\262!r\304\216\305	@\306\"\210\307 \210\310 +\207"
                                                   [e save-selected-window--state internal--before-with-selected-window event-start
                                                      #[nil "\301!\207"
                                                            [save-selected-window--state internal--after-with-selected-window]
                                                            2]
                                                      select-window norecord toggle-input-method force-mode-line-update]
                                                   3 nil "e"])))
                            mouse-face mode-line-highlight))
                          #("%z" 0 2
                            (local-map
                             (keymap
                              (mode-line keymap
                                         (mouse-3 .
                                                  #[(e)
                                                    "\302\303!\211@\262!r\304\216\305	@\306\"\210\307\310!+\207"
                                                    [e save-selected-window--state internal--before-with-selected-window event-start
                                                       #[nil "\301!\207"
                                                             [save-selected-window--state internal--after-with-selected-window]
                                                             2]
                                                       select-window norecord call-interactively set-buffer-file-coding-system]
                                                    3 nil "e"])
                                         (mouse-1 .
                                                  #[(e)
                                                    "\304\305!\211@\262!r\306\216\307	@\310\"\210\n\205 \205 \311!+\207"
                                                    [e save-selected-window--state enable-multibyte-characters buffer-file-coding-system internal--before-with-selected-window event-start
                                                       #[nil "\301!\207"
                                                             [save-selected-window--state internal--after-with-selected-window]
                                                             2]
                                                       select-window norecord describe-coding-system]
                                                    3 nil "e"])))
                             mouse-face mode-line-highlight help-echo mode-line-mule-info-help-echo))
                          (:eval
                           (mode-line-eol-desc)))
    :mode-line-client ("" (:propertize ("" (:eval (if (frame-parameter nil 'client) "@" ""))) help-echo "emacsclient frame"))
    :mode-line-modified (#("%1*" 0 3
                           (mouse-face mode-line-highlight local-map
                                       (keymap (mode-line keymap (mouse-1 . mode-line-toggle-read-only)))
                                       help-echo mode-line-read-only-help-echo))
                         #("%1+" 0 3
                           (mouse-face mode-line-highlight local-map
                                       (keymap (mode-line keymap (mouse-1 . mode-line-toggle-modified)))
                                       help-echo mode-line-modified-help-echo)))
    :mode-line-remote (#("%1@" 0 3
                         (help-echo #[(window _object _point) "\303\304\305!r\306\216\307	@\310\"\210\n;\203$ \311\n!\203 \312\202 \313\nP\202% \314+\"\207"
                                      [window save-selected-window--state default-directory format "%s" internal--before-with-selected-window
                                              #[nil "\301!\207" [save-selected-window--state internal--after-with-selected-window] 2]
                                              select-window norecord file-remote-p "Current directory is remote: " "Current directory is local: " "Current directory is nil"]
                                      5]
                                    mouse-face mode-line-highlight)))
    :mode-line-frame-identification (:eval (mode-line-frame-control))
    :mode-line-buffer-identification (#("%12b" 0 4
                                        (local-map (keymap (header-line keymap
                                                                        (mouse-3 . mode-line-next-buffer)
                                                                        (down-mouse-3 . ignore)
                                                                        (mouse-1 . mode-line-previous-buffer)
                                                                        (down-mouse-1 . ignore))
                                                           (mode-line keymap
                                                                      (mouse-3 . mode-line-next-buffer)
                                                                      (mouse-1 . mode-line-previous-buffer)))
                                                   mouse-face mode-line-highlight help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer" face mode-line-buffer-id)))
    :mode-line-position ((-3
                          #("%p" 0 2
                            (help-echo "Size indication mode\nmouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
                                       (keymap
                                        (mode-line keymap
                                                   (down-mouse-1 keymap
                                                                 (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                                                                     (:toggle . column-number-mode))
                                                                 (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                                                                   (:toggle . line-number-mode))
                                                                 "Toggle Line and Column Number Display"))))))
                         (size-indication-mode
                          (8
                           #(" of %I" 0 6
                             (help-echo "Size indication mode\nmouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
                                        (keymap
                                         (mode-line keymap
                                                    (down-mouse-1 keymap
                                                                  (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                                                                      (:toggle . column-number-mode))
                                                                  (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                                                                    (:toggle . line-number-mode))
                                                                  "Toggle Line and Column Number Display")))))))
                         (line-number-mode
                          ((column-number-mode
                            (10
                             #(" (%l,%c)" 0 8
                               (help-echo "Line number and Column number\nmouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
                                          (keymap
                                           (mode-line keymap
                                                      (down-mouse-1 keymap
                                                                    (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                                                                        (:toggle . column-number-mode))
                                                                    (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                                                                      (:toggle . line-number-mode))
                                                                    "Toggle Line and Column Number Display"))))))
                            (6
                             #(" L%l" 0 4
                               (help-echo "Line Number\nmouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
                                          (keymap
                                           (mode-line keymap
                                                      (down-mouse-1 keymap
                                                                    (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                                                                        (:toggle . column-number-mode))
                                                                    (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                                                                      (:toggle . line-number-mode))
                                                                    "Toggle Line and Column Number Display"))))))))
                          ((column-number-mode
                            (5
                             #(" C%c" 0 4
                               (help-echo "Column number\nmouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
                                          (keymap
                                           (mode-line keymap
                                                      (down-mouse-1 keymap
                                                                    (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                                                                        (:toggle . column-number-mode))
                                                                    (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                                                                      (:toggle . line-number-mode))
                                                                    "Toggle Line and Column Number Display"))))))))))
    :vc-mode nil
    :mode-line-modes (#("%[" 0 2
                        (help-echo "Recursive edit, type C-M-c to get out"))
                      "("
                      (:propertize
                       ("" mode-name)
                       help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes" mouse-face mode-line-highlight local-map
                       (keymap
                        (mode-line keymap
                                   (down-mouse-3 keymap
                                                 (abbrev-mode menu-item "Abbrev (Abbrev)" abbrev-mode :help "Automatically expand abbreviations" :button
                                                              (:toggle . abbrev-mode))
                                                 (auto-fill-mode menu-item "Auto fill (Fill)" auto-fill-mode :help "Automatically insert new lines" :button
                                                                 (:toggle . auto-fill-function))
                                                 (auto-revert-mode menu-item "Auto revert (ARev)" auto-revert-mode :help "Revert the buffer when the file on disk changes" :button
                                                                   (:toggle bound-and-true-p auto-revert-mode))
                                                 (auto-revert-tail-mode menu-item "Auto revert tail (Tail)" auto-revert-tail-mode :help "Revert the tail of the buffer when buffer grows" :enable
                                                                        (buffer-file-name)
                                                                        :button
                                                                        (:toggle bound-and-true-p auto-revert-tail-mode))
                                                 (flyspell-mode menu-item "Flyspell (Fly)" flyspell-mode :help "Spell checking on the fly" :button
                                                                (:toggle bound-and-true-p flyspell-mode))
                                                 (font-lock-mode menu-item "Font Lock" font-lock-mode :help "Syntax coloring" :button
                                                                 (:toggle . font-lock-mode))
                                                 (glasses-mode menu-item "Glasses (o^o)" glasses-mode :help "Insert virtual separators to make long identifiers easy to read" :button
                                                               (:toggle bound-and-true-p glasses-mode))
                                                 (hide-ifdef-mode menu-item "Hide ifdef (Ifdef)" hide-ifdef-mode :help "Show/Hide code within #ifdef constructs" :button
                                                                  (:toggle bound-and-true-p hide-ifdef-mode))
                                                 (highlight-changes-mode menu-item "Highlight changes (Chg)" highlight-changes-mode :help "Show changes in the buffer in a distinctive color" :button
                                                                         (:toggle bound-and-true-p highlight-changes-mode))
                                                 (outline-minor-mode menu-item "Outline (Outl)" outline-minor-mode :help "" :button
                                                                     (:toggle bound-and-true-p outline-minor-mode))
                                                 (overwrite-mode menu-item "Overwrite (Ovwrt)" overwrite-mode :help "Overwrite mode: typed characters replace existing text" :button
                                                                 (:toggle . overwrite-mode))
                                                 "Minor Modes")
                                   (mouse-2 . describe-mode)
                                   (down-mouse-1 menu-item "Menu Bar" ignore :filter
                                                 #[(_)
                                                   "\300 \207"
                                                   [mouse-menu-major-mode-map]
                                                   1]))))
                      ("" mode-line-process)
                      (:propertize
                       ("" minor-mode-alist)
                       mouse-face mode-line-highlight help-echo "Minor mode\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode\nmouse-3: Toggle minor modes" local-map
                       (keymap
                        (header-line keymap
                                     (down-mouse-3 keymap
                                                   (abbrev-mode menu-item "Abbrev (Abbrev)" abbrev-mode :help "Automatically expand abbreviations" :button
                                                                (:toggle . abbrev-mode))
                                                   (auto-fill-mode menu-item "Auto fill (Fill)" auto-fill-mode :help "Automatically insert new lines" :button
                                                                   (:toggle . auto-fill-function))
                                                   (auto-revert-mode menu-item "Auto revert (ARev)" auto-revert-mode :help "Revert the buffer when the file on disk changes" :button
                                                                     (:toggle bound-and-true-p auto-revert-mode))
                                                   (auto-revert-tail-mode menu-item "Auto revert tail (Tail)" auto-revert-tail-mode :help "Revert the tail of the buffer when buffer grows" :enable
                                                                          (buffer-file-name)
                                                                          :button
                                                                          (:toggle bound-and-true-p auto-revert-tail-mode))
                                                   (flyspell-mode menu-item "Flyspell (Fly)" flyspell-mode :help "Spell checking on the fly" :button
                                                                  (:toggle bound-and-true-p flyspell-mode))
                                                   (font-lock-mode menu-item "Font Lock" font-lock-mode :help "Syntax coloring" :button
                                                                   (:toggle . font-lock-mode))
                                                   (glasses-mode menu-item "Glasses (o^o)" glasses-mode :help "Insert virtual separators to make long identifiers easy to read" :button
                                                                 (:toggle bound-and-true-p glasses-mode))
                                                   (hide-ifdef-mode menu-item "Hide ifdef (Ifdef)" hide-ifdef-mode :help "Show/Hide code within #ifdef constructs" :button
                                                                    (:toggle bound-and-true-p hide-ifdef-mode))
                                                   (highlight-changes-mode menu-item "Highlight changes (Chg)" highlight-changes-mode :help "Show changes in the buffer in a distinctive color" :button
                                                                           (:toggle bound-and-true-p highlight-changes-mode))
                                                   (outline-minor-mode menu-item "Outline (Outl)" outline-minor-mode :help "" :button
                                                                       (:toggle bound-and-true-p outline-minor-mode))
                                                   (overwrite-mode menu-item "Overwrite (Ovwrt)" overwrite-mode :help "Overwrite mode: typed characters replace existing text" :button
                                                                   (:toggle . overwrite-mode))
                                                   "Minor Modes"))
                        (mode-line keymap
                                   (down-mouse-3 keymap
                                                 (abbrev-mode menu-item "Abbrev (Abbrev)" abbrev-mode :help "Automatically expand abbreviations" :button
                                                              (:toggle . abbrev-mode))
                                                 (auto-fill-mode menu-item "Auto fill (Fill)" auto-fill-mode :help "Automatically insert new lines" :button
                                                                 (:toggle . auto-fill-function))
                                                 (auto-revert-mode menu-item "Auto revert (ARev)" auto-revert-mode :help "Revert the buffer when the file on disk changes" :button
                                                                   (:toggle bound-and-true-p auto-revert-mode))
                                                 (auto-revert-tail-mode menu-item "Auto revert tail (Tail)" auto-revert-tail-mode :help "Revert the tail of the buffer when buffer grows" :enable
                                                                        (buffer-file-name)
                                                                        :button
                                                                        (:toggle bound-and-true-p auto-revert-tail-mode))
                                                 (flyspell-mode menu-item "Flyspell (Fly)" flyspell-mode :help "Spell checking on the fly" :button
                                                                (:toggle bound-and-true-p flyspell-mode))
                                                 (font-lock-mode menu-item "Font Lock" font-lock-mode :help "Syntax coloring" :button
                                                                 (:toggle . font-lock-mode))
                                                 (glasses-mode menu-item "Glasses (o^o)" glasses-mode :help "Insert virtual separators to make long identifiers easy to read" :button
                                                               (:toggle bound-and-true-p glasses-mode))
                                                 (hide-ifdef-mode menu-item "Hide ifdef (Ifdef)" hide-ifdef-mode :help "Show/Hide code within #ifdef constructs" :button
                                                                  (:toggle bound-and-true-p hide-ifdef-mode))
                                                 (highlight-changes-mode menu-item "Highlight changes (Chg)" highlight-changes-mode :help "Show changes in the buffer in a distinctive color" :button
                                                                         (:toggle bound-and-true-p highlight-changes-mode))
                                                 (outline-minor-mode menu-item "Outline (Outl)" outline-minor-mode :help "" :button
                                                                     (:toggle bound-and-true-p outline-minor-mode))
                                                 (overwrite-mode menu-item "Overwrite (Ovwrt)" overwrite-mode :help "Overwrite mode: typed characters replace existing text" :button
                                                                 (:toggle . overwrite-mode))
                                                 "Minor Modes")
                                   (mouse-2 . mode-line-minor-mode-help)
                                   (down-mouse-1 . mouse-minor-mode-menu))))
                      #("%n" 0 2
                        (local-map
                         (keymap
                          (mode-line keymap
                                     (mouse-2 . mode-line-widen)))
                         mouse-face mode-line-highlight help-echo "mouse-2: Remove narrowing from buffer"))
                      ")"
                      #("%]" 0 2
                        (help-echo "Recursive edit, type C-M-c to get out"))
                      " ")
    :mode-line-misc-info ((which-func-mode ("" which-func-format " "))
                          (global-mode-string ("" global-mode-string " ")))
    :mode-line-end-spaces (:eval (unless (display-graphic-p) "-%-"))))

(defun my-ilist-mode-line ()
  (setq-local mode-line-format '("%e"
                           mode-line-front-space
                           mode-line-mule-info
                           mode-line-client
                           mode-line-modified
                           mode-line-remote
                           mode-line-frame-identification
                           (:propertize "%b" face mode-line-buffer-id)
                           " "
                           mode-line-position
                           mode-line-end-spaces)))
(add-hook 'imenu-list-major-mode-hook #'my-ilist-mode-line)
