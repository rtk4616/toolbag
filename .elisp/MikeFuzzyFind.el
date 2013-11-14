(defun convert-anything-to-string (input-symbol)
  "Converts the supplied symbol to a string."
  (cond

   ((stringp input-symbol)
    input-symbol)

   ((numberp input-symbol)
    (number-to-string input-symbol))

   (t
    ;; We handle only symbols and lists.
    ;; TODO: Better error handling here...
    (error "Error in convert-anything-to-string!"))
   )
  )


(defun get-dired-string-from-file-location (file-location-string)
  "Returns a string that can be parsed as a valid file by dired-mode."
  (let (
        (attribute-list (file-attributes file-location-string 'string))
        )
    (if attribute-list
        (progn
          ;; Return the string to insert.
          (mapconcat 'eval '(
                             (funcall 'convert-anything-to-string(nth 8 attribute-list))
                             (funcall 'convert-anything-to-string(nth 1 attribute-list))
                             (funcall 'convert-anything-to-string(nth 2 attribute-list))
                             (funcall 'convert-anything-to-string(nth 3 attribute-list))
                             (funcall 'convert-anything-to-string(nth 7 attribute-list))
                             (funcall 'format-time-string "%b %d %H:%M" (nth 5 attribute-list))
                             file-location-string
                             ) " ")
          )
      ;; Otherwise return nil.
      nil
      )))


(defun get-regex-alternation-from-list (list-of-strings &optional prepend-string)
  (if prepend-string
      (setq list-of-strings (mapcar (function (lambda (x) (concat prepend-string x))) mike-ignore-directories)))

  ;; Escape periods in preparation for regex.
  (setq list-of-strings (mapcar (function (lambda (x) (replace-regexp-in-string "\\." "\\\\." x))) list-of-strings))

  ;; Join the ignore directories into a regex string.
  (setq the-regex-search-string (format "\\(%s\\)" (mapconcat 'identity list-of-strings "\\|"))))


(defun insert-directory-files-recursive (directory match maxdepth)
  "Need some documentation about this function here..."
  (let* ((files-list '())
         (current-directory-list (directory-files directory t))
         )

    ;; While current-directory-list is not empty...
    (while current-directory-list
      (let ((f (car current-directory-list))) ; Set f as the first file in the list.
        (cond

         ((and
           ;; If ignore-files-regex-string is not nil...
           ignore-files-regex-string
           ;; ...is a regular file...
           (file-regular-p f)
           ;; ...and f is one of the ignored files...
           (string-match ignore-files-regex-string f))
          ;; ...then skip this file.

          ;; TODO: This is a debug line below...
          ;; (message (concat "ignoring file " f " based on \"" (match-string 1 f) "\""))
          nil)

         ((and
           ;; If ignore-dirs-regex-string is not nil...
           ignore-dirs-regex-string
           ;; ...and f is a directory...
           (file-directory-p f)
           ;; ...which is one of the ignored directories...
           (string-match ignore-dirs-regex-string f))
          ;; ...then skip this directory

          ;; TODO: This is a debug line below...
          ;; (message (concat "ignoring directory " f " based on \"" (match-string 1 f) "\""))
          nil)

         ((and
           ;; If f is a regular, readable file that matches the search string...
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          ;; Insert the file into the current buffer
          (insert (concat (get-dired-string-from-file-location f) "\n")))

         ((and
           ;; If f is a directory...
           (file-directory-p f)
           ;; ...is readable...
           (file-readable-p f)
           ;; ...is not the the current or previous directory...
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           ;; ...and we have not reached the max depth...
           (> maxdepth 0))
          ;; Then recursively call this function
          (insert-directory-files-recursive f match (- maxdepth -1))
          )
        (t)))
    ;; Remove f from the working list of files in the current directory.
    (setq current-directory-list (cdr current-directory-list)))

  ;; All done... return files-list!
  files-list)
) ;; End of defun.


(defun MikeFuzzyFileFinder (match-string)
  "Searches recursively for all files that match the file wildcard in the current directory"
  (interactive "sSearch for files matching: \n")

  (switch-to-buffer "*MikeFuzzyFind Results*")
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)

  (insert-directory-files-recursive default-directory match-string 10)

  (indent-region (point-min) (point-max) 2)

  (dired-mode)
  (if (fboundp 'dired-simple-subdir-alist)
      ;; will work even with nested dired format (dired-nstd.el,v 1.15
      ;; and later)
      (dired-simple-subdir-alist)
    ;; else we have an ancient tree dired (or classic dired, where
    ;; this does no harm)
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons default-directory (point-min-marker)))))
  (goto-line 2))


;; A list of ignored file names.
(setq
 mike-ignore-files
 '(
   ".pyc$"
   )
 )

;; A list of ignored directory names.
(setq
 mike-ignore-directories
 '(
   "ve"
   ".idea"
   ".git"
   "migrations"
   )
 )

;; Get the alternation for a list of files.
(setq ignore-files-regex-string (get-regex-alternation-from-list mike-ignore-files))

;; Get the alternation for a list of directories by prepending a slash to the directory names.
(setq ignore-dirs-regex-string (get-regex-alternation-from-list mike-ignore-directories "/"))

(provide 'MikeFuzzyFind)
