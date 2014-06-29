;; ==============================================================
;; Initial setup
;; ==============================================================

;; Set the default directory to be the user's home directory if we opened the GUI version of Emacs.
(if window-system (setq default-directory "~"))

;; Highlight text selection.
(transient-mark-mode 1)

;; Delete selected text when you start typing.
(delete-selection-mode 1)

;; Enable line numbers by default.
(global-linum-mode t)

;; Better word-wrapping.
;; (global-visual-line-mode t) ; Wraps at words, rebinds keys to operated on visual lines.
(setq-default word-wrap t)     ; Wraps at words, but preserves original key behavior.

;; Set up Marmalade.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Set paths
(add-to-list 'load-path "~/.elisp")
(let ((default-directory "~/.elisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'custom-theme-load-path "~/.elisp/themes")

;; Multiple cursors.
(require 'multiple-cursors)
(global-set-key (kbd "C-M-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-l") 'mc/edit-lines)
(setq mc/list-file "~/.elisp/.mc-lists.el")

;; Autopair stuff.
(require 'autopair)
(autopair-global-mode)

;; Set up my MikeFuzzyFind stuff
(require 'MikeFuzzyFind)

;; Company-mode setup.
(require 'company)
;; This is if we wanted to use the company-go backend for every mode.
;; (require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))))
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-dabbrev t)
(add-to-list 'company-backends 'company-ispell t)
(add-to-list 'company-backends 'company-files t)
(setq company-idle-delay nil)

;; Better Scala mode.
(require 'scala-mode2)

;; ENSIME stuff
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(define-key ensime-mode-map (kbd "M-p") 'MikeUpSomeLines)
(define-key ensime-mode-map (kbd "M-n") 'MikeDownSomeLines)
(define-key ensime-mode-map (kbd "C-M-p") 'ensime-backward-note)
(define-key ensime-mode-map (kbd "C-M-n") 'ensime-forward-note)


;; Set up expand region.
(require 'expand-region)
(setq expand-region-fast-keys-enabled nil)
(global-set-key (kbd "M-h") 'er/expand-region)
(global-set-key (kbd "C-M-h") 'er/contract-region)
(eval-after-load "multi-web-mode"   '(require 'html-mode-expansions))
(eval-after-load "multi-web-global-mode"   '(require 'html-mode-expansions))

;; Hippie-expand stuff.
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         ;; try-expand-all-abbrevs
                                         ;; try-expand-list
                                         ;; try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; Autoload actionscript-mode.
(autoload 'actionscript-mode "actionscript-mode" "Major mode for editing ActionScript." t)

;; js2-mode stuff.
(autoload 'js2-mode "js2-mode" "Major mode for editing Javascript stuffs." t)
(add-hook 'js2-mode-hook
          (lambda () (local-set-key (kbd "C-M-e") 'js2-display-error-list)))
;; (require 'js2-mode)

;; Multi-web-mode...
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags
      '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
        (js2-mode  "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
        (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "hbs"))
(multi-web-global-mode 1)

;; Make go-mode autoload for .go files.
(require 'go-mode-load)
(add-hook 'before-save-hook #'gofmt-before-save)

;; Load PHP-mode.
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; Load PHP-mode.
(autoload 'textile-mode "textile-mode" "Major mode for editing textile files." t)

;; Load markdown-mode.
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; -----------------------------------
;; Set up aspell as the spell-checker.
;; -----------------------------------
;; Path to aspell on Windows.
;; (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
;; Path to aspell on OSX.
(add-to-list 'exec-path "/usr/local/bin/")
;; Set aspell as the ispell program to run.
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; Set up the Emacs theme.
(load-theme 'tomorrow-night t)
;; (load-theme 'monokai t)

;; Set SSH as the default method for Tramp.
(setq tramp-default-method "ssh")

;; Make F6 toggle autopair on and off for the purposes of pasting.
(global-set-key (kbd "<f6>") 'autopair-mode)

;; Set up ido mode.
;; (require 'flx-ido)
(ido-mode t)
(setq ido-enable-last-directory-history nil)
(setq ido-record-commands nil)
(setq ido-max-work-directory-list 0)
(setq ido-max-work-file-list 0)
(setq ido-everywhere t)
;; (flx-ido-mode 1)
(setq ido-use-faces nil)

;; Ignore extensions in completion.
(setq completion-ignored-extensions
      '(".o" ".lo" ".mh" ".elc" "~" ".bin" ".lbin" ".fasl" ".dvi" ".toc" ".aux"
        ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".pyc"))

;; Load yaml-mode.
(require 'yaml-mode)

;; Load the Autopair module.
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;; Set auto mode rules.
;; This is where you define the major modes for different file extensions.
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess$" . conf-javaprop-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.vert$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("crontab$" . conf-mode))
(add-to-list 'auto-mode-alist '("rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".gitconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.vcl$" . conf-javaprop-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . multi-web-mode))

;;
;; end Initial setup
;;



;; ==============================================================
;; Custom functions
;; ==============================================================

;; Need this or else MikeGrepInFiles will fail with "Wrong type argument: stringp, nil"
(grep-compute-defaults)

;; Set up ignored files and directories when using grep.
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files "*.pyc")
     (add-to-list 'grep-find-ignored-directories "migrations")
     (add-to-list 'grep-find-ignored-directories "ve")
     ))


(defun MikeGrepForSymbol (search_string)
  (interactive "sGrep for symbol: ")
  (rgrep (concat "^\\ *\\(class\\|def\\|defun\\|func\\|type\\|function!*\\)\\ .*" search_string) "*" "./"))


(defun MikeGrepInFiles (search_string file_extension)
  (interactive "sSearch for: \nsIn files ending in: ")
  (rgrep search_string (concat "*" file_extension) "./"))
;; (rgrep search_string (format "*.%s" file_extension) (pwd)))


(defun MikeGrepForFiles (search_string)
  (interactive "sSearch for file: ")
  (find-name-dired "." (concat "*" search_string "*")))


(defun MikeGetIndentationEnd (&optional REVERSE)
  "Return the furthest location in the file, going forward, before we reach a
line that is indented less than the starting indentation.

Specifying REVERSE as t will result in traversing the file backward."
  (interactive)
  (cond
   ;; If the function was called when the current line's indentation is zero...
   ((= 0 (current-indentation))
    ;; Just return the beginning of the current line.
    (line-beginning-position))

   ;; Otherwise, compute and return the appropriate position.
   (t
    (let ((starting-indentation (current-indentation))
          (to-continue t)
          (this-here-line nil)
          (to-return nil))

      ;; Set initial return value based on the REVERSE flag.
      (if REVERSE
          ;; Beginning of the starting line.
          (setq to-return (line-beginning-position))
        ;; End of the starting line.
        (setq to-return (line-end-position)))

      (unwind-protect
          ;; Save current cursor position, etc, so we can restore when done.
          (save-excursion
            ;; Loop until we break indentation.
            (while to-continue
              ;; Get the current line as a string.
              (setq
               this-here-line
               (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

              ;; Determine if we should continue looping, and whether or not to
              ;; update the return value.
              (cond
               ;; If the current line is greater or equal to the starting
               ;; indentation...
               ((>= (current-indentation) starting-indentation)
                (progn
                  (setq to-continue t)
                  ;; Update the return value.
                  (if REVERSE
                      (setq to-return (line-beginning-position))
                    (setq to-return (line-end-position)))))

               ;; If the current line is just whitespace...
               ((string-match "^$" this-here-line)
                (setq to-continue t))

               ;; Otherwise, don't continue.
               (t (setq to-continue nil)))

              ;; Go forward or backward depending on the given direction.
              (condition-case ex
                  (if REVERSE
                      (previous-line)
                    (next-line))
                ('error
                 ;; Catch error moving, like trying to move past the beginning or end
                 ;; of the buffer.
                 (message (format "Caught exception: [%s]" ex))
                 (setq to-continue nil)))))

        ;; (message "At the end of unwind-protect!"))
        (message "Done."))

      ;; Return the point.
      to-return))))


(defun MikeGetIndentation ()
  (interactive)
  (let ((start-pos (MikeGetIndentationEnd t))
        (end-pos (MikeGetIndentationEnd nil)))
    (if (= start-pos end-pos)
        (message "Nothing indented here to select!")
      (goto-char end-pos)
      (push-mark nil t t)
      (goto-char start-pos))))


(defun MikeDeIndent (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (if (and (= beg (line-beginning-position))
             (= end (line-end-position)))
        (indent-rigidly beg end -4)
      (progn
        (indent-rigidly beg end -4)
        (goto-char end-pos)
        (push-mark nil t t)
        (goto-char start-pos)))))


(defun MikeIndent (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (if (and (= beg (line-beginning-position))
             (= end (line-end-position)))
        (indent-rigidly beg end 4)
      (progn
        (indent-rigidly beg end 4)
        (goto-char end-pos)
        (push-mark nil t t)
        (goto-char start-pos)))))


(defun MikeUpSomeLines ()
  (interactive)
  (previous-line 6)
  )


(defun MikeDownSomeLines ()
  (interactive)
  (next-line 6)
  )


;; TODO: Work on this more. This is a custom hippie-expand function.
;;
;; (defun MikeExpand (arg)
;;   (message arg))
;; (add-to-list 'hippie-expand-try-functions-list 'MikeExpand)

;; Set up auto-complete.
;; (require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories "~/.elisp/ac-dict")
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-delay 0.2)
;; (add-to-list 'ac-user-dictionary-files (concat default-directory ".project_completions"))
;; (ac-clear-dictionary-cache)


;; ==============================================================
;; START Keybindings
;; ==============================================================

;; Shortcut key for discarding all changes and quitting Emacs without prompting.
(global-set-key "\C-x\C-\\" 'kill-emacs)

;; Shortcut key for indenting/de-indenting a region.
(global-set-key "\M-," 'MikeDeIndent)
(global-set-key "\M-." 'MikeIndent)

;; Shortcut key for title-casing a word/region.
(global-set-key "\M-u" 'downcase-word)
(global-set-key "\M-U" 'upcase-word)

;; Shortcut key for selecting everything at the current indentation.
(global-set-key "\M-j" 'MikeGetIndentation)

;; Shortcut for selecting an entire paragraph.
(global-set-key "\M-k" 'mark-paragraph)

;; Use the default shortcut for regexp isearch to activate custom rgrep.
(global-set-key "\C-x\C-r" 'MikeGrepForSymbol)

;; List all lines matching a pattern in the current file.
(global-set-key "\M-l" 'recenter-top-bottom)

;; List all lines matching a pattern in the current file.
(global-set-key "\C-xl" 'list-matching-lines)
(global-set-key "\C-x\C-l" 'list-matching-lines)

;; iMenu binding.
(global-set-key "\C-x\C-j" 'imenu)
(global-set-key "\C-xj" 'imenu)

;; Use the default shortcut for regexp isearch to find files by partial name.
;; (global-set-key "\C-x\C-g" 'MikeGrepForFiles)
;; (global-set-key "\C-xg" 'MikeGrepForFiles)
(global-set-key "\C-x\C-g" 'MikeFuzzyFileFinder)
(global-set-key "\C-xg" 'MikeFuzzyFileFinder)

;; Use the default shortcut for regexp isearch to activate custom rgrep.
(global-set-key "\C-xf" 'MikeGrepInFiles)

;; Rebind Ctrl-s and Ctrl-r to use the regexp versions of isearch.
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

;; Ctrl-\ saves the window layout.
(global-set-key "\C-\\" (lambda () (interactive) (window-configuration-to-register 'a)))
;; Alt-\ loads the window layout.
(global-set-key "\M-\\" (lambda () (interactive) (jump-to-register 'a)))

;; Make pressing enter while still holding down the control key insert a
;; newline.
(global-set-key (kbd "<C-return>") 'newline)

;; Make autocomplete feature use a better shortcut
;; (global-set-key (kbd "M-SPC") 'dabbrev-expand)
;; (global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "M-SPC") 'company-complete)


;; Make dabbrev-expand respect case...
(setq dabbrev-case-fold-search nil)

;; Some aliases to make things easier!
(defalias 'qrr 'query-replace-regexp)
(defalias 'ff 'find-name-dired)
(defalias 'sc 'diff-buffer-with-file)

;; Bind F4 to see changes between the current buffer and the version on disk.
(global-set-key (kbd "<f4>") 'sc)

;; Use electric buffer list for buffer navigation
(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; (global-set-key "\C-xb" 'electric-buffer-list)

;; Make it so we don't have to release ctrl when switching buffers...
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\M-o" 'other-window)

;; ...or killing buffers.
(global-set-key "\C-x\C-k" 'kill-buffer)

;; Custom other-window binding.
(global-set-key (kbd "C-]") 'other-window)

;; Make emacs use the system keyboard
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Make Alt-P and Alt-N act like Vim's Ctrl-y and Ctrl-e
(global-set-key "\M-p" 'MikeUpSomeLines)
(global-set-key "\M-n" 'MikeDownSomeLines)

;; Bindings for changing buffers.
;; (global-set-key (kbd "M-<left>") 'previous-buffer)
;; (global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-xp" 'previous-buffer)
(global-set-key "\C-xn" 'next-buffer)
;;
;; END Keybindings
;;


;; ==============================================================
;; START misc stuff here...
;; ==============================================================

;; Make rgrep searches be case insensitive.
(setq case-fold-search t)

;; make emacs keep the current working directory when opening files.
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))


;; Add a little padding around the line numbers.
;; Dynamically determine character width for the line numbers column, and add a
;; space for padding as well.
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))
;; (setq linum-format " %d  ")


;; Make F5 toggle line numbers on and off.
(global-set-key (kbd "<f5>") 'linum-mode)

;; Ediff with vertically split windows.
(setq ediff-split-window-function 'split-window-horizontally)

;; Allow smooth scrolling.
(setq scroll-step            1
      scroll-conservatively  10000)

;; Set default font.
;; (set-default-font "DejaVu Sans Mono-9")
(set-default-font "Menlo-12")

;; Set character wrapping.
(setq-default fill-column 79)

;; Put autosave and backup files in the system temp folder.
;;
;; NOTE: Commenting this out, since I disable saving backups entirely in the
;; lines below. Just leaving it in here for reference/if I ever want to go back
;; to using backup files.
;;
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; Disable saving backup files. Have never made use of this functionality.
;; personally.
(setq make-backup-files nil)

;; Disable auto-save. Have never made use of this functionality.
(setq auto-save-default nil)


;; Add highlighting of TODO, BUG, NOTE, and FIXME.
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))


;; ************************************************************************************************
;; Disable requiring the newline at EOF
;;
;; NOTE: This doesn't work for some reason... need to look into why, when I have the time.
;;
;; Ensure we do this *after* default.el is loaded, otherwise, when Emacs
;; executes the default.el file after your .emacs file, this value will get
;; changed.
;;
;; Reference: http://rura.org/blog/2004/10/26/emacs-require-final-newline-on-fedora-how-to-kill-it/
;;
(add-hook 'after-init-hook
          '(lambda ()
             (setq require-final-newline nil)))
;; ************************************************************************************************


;; Require the newline at EOF
(setq require-final-newline nil)

;; Disable useless decorations.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Accept y or n when presented with yes or no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable beeps.
(setq ring-bell-function 'ignore)

;; Always use syntax highlighting.
(global-font-lock-mode 1)

;; Show the column number in addition to the line number.
(setq-default column-number-mode 1)

;; Make the frame title show the file name.
(setq frame-title-format "%b")

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Remove trailing whitespace before saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ================================================================
;;                      Indentation settings
;; ================================================================

;; Indent only with spaces (default 4), never tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq indent-line-function 'insert-tab)

;; Use BSD style for formatting.
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Indentation settings for Ruby.
(setq ruby-indent-level 2)

;; Indentation for CSS
(setq css-indent-offset 4)

;; Indentation for Python
(setq python-indent 4)
