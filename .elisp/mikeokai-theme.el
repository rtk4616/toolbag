;;; mikeokai-theme.el --- Monokai color theme clone for GNU Emacs 24

;; Copyright (C) 2012 Mike Wilkerson <wilkystyle@gmail.com>

;; Author: Mike Wilkerson
;; URL: None yet...
;; Version: 0.0.2
;;
;; ========================================
;; Make sure you have a 256-color terminal
;; Put the following in your .bashrc:
;; export TERM=xterm-256color
;; ========================================


(deftheme mikeokai "Port of Monokai Theme for Emacs 24")

(let ((*background-color*  "#272822")
      (*background-mode*  'dark)
      (*border-color*  "black")
      (*cursor-color*  "#73d216")
      (*foreground-color*  "#F8F8F2")
      (*mouse-color*  "#73d216"))

  (custom-theme-set-faces
   'mikeokai

     `(default ((t (nil))))
     `(font-lock-comment-face ((t (:foreground "color-238"))))
     ;; `(font-lock-comment-face ((t (:foreground "#75715E")))) ; dark aluminum
     `(font-lock-comment-delimiter-face ((t (:foreground "color-238"))))
     ;; `(font-lock-comment-delimiter-face ((t (:foreground "#75715E")))) ; dark aluminum
     `(font-lock-doc-face ((t (:foreground "color-221"))))
     `(font-lock-doc-string-face ((t (:foreground "color-221"))))
     ;; `(font-lock-string-face ((t (:foreground "#E6DB74")))) ; plum
     `(font-lock-string-face ((t (:foreground "color-221"))))
     ;; `(font-lock-keyword-face ((t (:foreground "#F92672"))))
     `(font-lock-keyword-face ((t (:foreground "color-161"))))
     ;; `(font-lock-builtin-face ((t (:foreground "#855c1b")))) ; med-dark chocolate
     `(font-lock-builtin-face ((t (:foreground "color-81"))))
     `(font-lock-function-name-face ((t (:foreground "color-118"))))
     ;; `(font-lock-variable-name-face ((t (:foreground "#FD971F"))))
     `(font-lock-variable-name-face ((t (:foreground "#cccccc"))))
     `(font-lock-preprocessor-face ((t (:foreground "#66D9EF")))) ; aluminum
     ;; `(font-lock-constant-face ((t (:foreground "#4e9a06"))))
     `(font-lock-constant-face ((t (:foreground "color-99"))))
     `(font-lock-type-face ((t (:foreground "#66D9EF")))) ; light plum
     `(font-lock-warning-face ((t (:bold t :foreground "#cc0000")))) ; scarlet red

     ;; Set the diff context face.
     `(diff-context ((t (:foreground "color-238"))))

     ;; Search
     `(isearch ((t (:foreground "#080808" :background "#edd400"))))
     `(isearch-lazy-highlight-face ((t (:foreground "color-232" :background "color-221"))))

     ;; Match highlighting.
     `(match ((t (:foreground "color-232" :background "color-221"))))

     ;; Emacs Interface
     `(fringe ((t (:background "#0f0f0f"))))
     `(border ((t (:background "#0f0f0f"))))
     `(mode-line ((t (:background "#1f1f1f" :foreground "#eeeeec"))))
     `(mode-line-buffer-id ((t (:background "#1f1f1f" :foreground "#eeeeec"))))
     `(mode-line-inactive ((t (:background "#1f1f1f" :foreground "#888a85"))))
     `(minibuffer-prompt ((t (:foreground "#729fcf")))) ; light sky blue
     `(region ((t (:background "#49483E"))))

     ;; Parenthesis matching
     `(show-paren-match-face ((t (:foreground "#2e3436" :background "#3E3D32"))))
     `(show-paren-mismatch-face ((t (:foreground "#2e3436" :background "#ef2929"))))

     ;; Calendar
     `(holiday-face ((t (:foreground "#cc0000")))) ; dark scarlet red

     ;; Info
     `(info-xref ((t (:foreground "#729fcf")))) ; light sky blue
     `(info-xref-visited ((t (:foreground "#ad7fa8")))) ; light plum

     ;;; AUCTeX
     `(font-latex-sectioning-5-face ((t (:foreground "#c4a000" :bold t)))) ; dark butter
     `(font-latex-bold-face ((t (:foreground "#4e9a06" :bold t)))) ; dark chameleon
     `(font-latex-italic-face ((t (:foreground "#4e9a06" :italic t)))) ; dark chameleon
     `(font-latex-math-face ((t (:foreground "#855c1b")))) ; med-dark chocolate
     `(font-latex-string-face ((t (:foreground "#77507b")))) ; plum
     `(font-latex-warning-face ((t (:foreground "#cc0000")))) ; dark scarlet red
     `(font-latex-slide-title-face ((t (:foreground "#c4a000")))) ; dark butter
     ))

(provide-theme 'mikeokai)
