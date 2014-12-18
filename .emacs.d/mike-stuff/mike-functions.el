(defun MikeUpSomeLines ()
  (interactive)
  (previous-line 6)
  )

(defun MikeDownSomeLines ()
  (interactive)
  (next-line 6)
  )

(defun MikeScrollUpOneLine ()
  (interactive)
  ;; Emacs "down" means something different than I mean ;)
  (scroll-down 1)
  )

(defun MikeScrollDownOneLine ()
  (interactive)
  ;; Emacs "up" means something different than I mean ;)
  (scroll-up 1)
  )

(defun mike-next-tag ()
  (interactive)
  (let ((current-prefix-arg t)) ;; emulate C-u
    (call-interactively 'find-tag)
    )
  )
