;; ======================================
;; Working on fuzzy find files feature...
;; ======================================

(defun get-regex-alternation-from-list (list-of-strings &optional prepend-string)
  (if prepend-string
      (setq list-of-strings (mapcar (function (lambda (x) (concat prepend-string x))) mike-ignore-directories)))

  ;; Escape periods in preparation for regex.
  (message (mapconcat 'identity list-of-strings ""))
  (setq list-of-strings (mapcar (function (lambda (x) (replace-regexp-in-string "\\." "\\\\." x))) list-of-strings))

  ;; Join the ignore directories into a regex string.
  (setq the-regex-search-string (format "\\(%s\\)" (mapconcat 'identity list-of-strings "\\|"))))


(defun directory-files-recursive (directory match maxdepth ignore-files-regex-string ignore-dirs-regex-string)
  "Need some documentation about this function here..."
  (let* ((files-list '())
         (current-directory-list (directory-files directory t))
         )

    ;; TODO: Debugging only... remove these!
    ;; (message (concat "ignore-files-regex-string is " ignore-files-regex-string))
    ;; (message (concat "ignore-dirs-regex-string is " ignore-dirs-regex-string))

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
          (message (concat "ignoring file " f " based on \"" (match-string 1 f) "\""))
          nil)

         ((and
           ;; If ignore-dirs-regex-string is not nil...
           ignore-dirs-regex-string
           ;; ...and f is a directory...
           (file-directory-p f)
           ;; ...which is one of the ignored directories...
           (string-match ignore-dirs-regex-string f))
          ;; ...then skip this directory
          (message (concat "ignoring directory " f " based on \"" (match-string 1 f) "\""))
          nil)

         ((and
           ;; If f is a regular, readable file that matches the search string...
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          ;; Add f to the files list.
          (setq files-list (cons f files-list)))

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
          ;; Then recursively call this function, and append the result to the files-list...
          (setq
           files-list
           (append files-list (directory-files-recursive f match (- maxdepth -1) ignore-files-regex-string ignore-dirs-regex-string)))
          ;; ...and add f to the files list.
          ;; TODO: I don't know if we should do this though...
          ;; Should it only return files?
          (setq files-list (cons f files-list)))

         ;; Return true?
         (t)))
      ;; End of "let"

      ;; Remove f from the working list of files in the current directory.
      (setq current-directory-list (cdr current-directory-list)))
    ;; End of "while"

    ;; All done... return files-list!
    files-list))


;; ===================================================================================
;; Testing stuff
;; ===================================================================================

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
(setq ignore-files-string (get-regex-alternation-from-list mike-ignore-files))

;; Get the alternation for a list of directories by prepending a slash to the directory names.
(setq ignore-dirs-string (get-regex-alternation-from-list mike-ignore-directories "/"))

;; An example of how to call this function...
;; You could also use "." as the directory.
(directory-files-recursive "~/development/Latinum-VM" "\\.py$" 10 ignore-files-string ignore-dirs-string)
