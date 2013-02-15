;; ==============================================================
;; Initial setup
;; ==============================================================

;; Set paths
(add-to-list 'load-path "~/.elisp")

;; Load PHP-mode.
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; Load Flyspell
;; (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; Loading a theme based whether or not emacs is in GUI mode.
(if window-system
    ;; (load-library "solarized-light-theme")
    ;; (load-library "~/.elisp/themes/github-theme.el")
    (load-theme 'adwaita t)
  (load-library "mikeokai-theme"))
  ;; (load-library "~/.elisp/themes/github-theme.el"))
  ;; (load-library "solarized-light-theme"))

;; set SSH as the default method for Tramp.
(setq tramp-default-method "ssh")

;; Load the Autopair module.
;; (require 'autopair)
;; (autopair-global-mode) ;; enable autopair in all buffers

;; Make F6 toggle autopair on and off for the purposes of pasting.
(global-set-key (kbd "<f6>") 'autopair-mode)

;;
;; end Initial setup
;;


;; ==============================================================
;; START Keybindings
;; ==============================================================

;; Shortcuts for saving and recalling window layout.
(global-set-key "\C-x\M-\\" 'window-configuration-to-register)
(global-set-key "\C-x\\" 'jump-to-register)

;; Make pressing enter while still holding down the control key insert a
;; newline.
(global-set-key (kbd "<C-return>") 'newline)

;; Make autocomplete feature use a better shortcut
(global-set-key (kbd "M-SPC") 'dabbrev-expand)

;; Some aliases to make things easier!
(defalias 'qrr 'query-replace-regexp)
(defalias 'ff 'find-name-dired)
(defalias 'sc 'diff-buffer-with-file)

;; Bind F2 the find a file recursive alias.
(global-set-key (kbd "<f2>") 'ff)

;; Bind F3 to rgrep.
(global-set-key (kbd "<f3>") 'rgrep)

;; Bind F4 to see changes between the current buffer and the version on disk.
(global-set-key (kbd "<f4>") 'sc)

;; Use electric buffer list for buffer navigation
(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; (global-set-key "\C-xb" 'electric-buffer-list)

;; Make it so we don't have to release ctrl when switching buffers...
(global-set-key "\C-x\C-o" 'other-window)

;; ...or killing buffers.
(global-set-key "\C-x\C-k" 'kill-buffer)

;; Custom other-window binding.
(global-set-key (kbd "C-]") 'other-window)

;; Make emacs use the system keyboard
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Make Alt-P and Alt-N act like Vim's Ctrl-y and Ctrl-e
(global-set-key "\M-p" (lambda () (interactive) (previous-line 6)))
(global-set-key "\M-n" (lambda () (interactive) (next-line 6)))

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

;; *********************
;; Turn on line numbers!
;; *********************
;;
;; (NOTE: Only do this in the GUI. Selecting by line in the terminal version with line
;; numbers enabled results in the line numbers also being selected)
;;
;; (if window-system
;;     ;; If we are using the GUI version of Emacs...
;;     (add-hook 'find-file-hook (lambda () (linum-mode 1)))
;;   )

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
(setq-default fill-column 80)

;; Put autosave and backup files in the system temp folder.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; Set autoload and add autoload rules.
;; This is where you define the major modes for different file extensions.
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess$" . conf-javaprop-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.vert$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '(".gitconfig$" . conf-mode))

;; Add highlighting of TODO, BUG, NOTE, and FIXME.
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME:\\|TODO:\\|BUG:\\|NOTE:\\)" 1 font-lock-warning-face t)))))



;; *************************************************************************************************
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
;; *************************************************************************************************


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
(setq-default standard-indent 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Use BSD style for formatting.
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Indentation settings for Ruby.
(setq ruby-indent-level 2)

;; Indentation for CSS
(setq css-indent-offset 4)

;; Indentation for Python
(setq python-indent 4)
