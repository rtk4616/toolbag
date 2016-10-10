(eval-when-compile (require 'subr-x))

(defun lady/read-connection-string ()
  (completing-read "Connection string: "
                   (with-temp-buffer
                     (if (file-exists-p "~/.lady") (insert-file-contents "~/.lady"))
                     (split-string (buffer-string) "[$\n]" t))
                   nil
                   nil
                   (when (file-exists-p "~/.lady_last") (with-temp-buffer (insert-file-contents "~/.lady_last") (buffer-string)))
                   ))

(defun lady/connection-already-saved (connection-string)
  (if (member connection-string
              (split-string
               (with-temp-buffer (insert-file-contents "~/.lady") (buffer-string))
               "[$\n]"
               t))
      t
    nil))

(defun lady/parse-hosts (connection-string)
  (split-string connection-string ">" t " +"))

(defun lady/build-tramp-string (host-string)
  (let* (
         (host-part (car (split-string host-string " +" t)))
         (sudo-user (nth 1 (split-string host-string " +" t)))
         (host (car (split-string host-part "#" t ".*@")))
         (port (nth 1 (split-string host-part "#" t)))
         (full-host (concat host (when port (concat "#" port))))
         (user (when (string-match "@" host-part) (car (split-string host-string "@" t))))
         )
    (concat "ssh:"
            (when user (concat user "@"))
            full-host
            (when sudo-user (concat "|sudo:" sudo-user "@" full-host)))))

(defun lady/get-full-connection-string (connection-string)
  (interactive)
  (concat "/"
          (mapconcat 'lady/build-tramp-string
           (lady/parse-hosts connection-string)
           "|")
          ":")
  )

(defun lady/tramp-connect ()
  (interactive)
  (let* (
        (connection-string (lady/read-connection-string))
        (full-tramp-string (lady/get-full-connection-string connection-string))
        )
    ;; Save the connection string if we haven't done so previously.
    (if (and
         (file-exists-p "~/.lady")
         (lady/connection-already-saved connection-string)
         )
        (ignore)
      (append-to-file (concat (string-trim connection-string) "\n") nil "~/.lady"))
    ;; Save as the last-used connection string
    (write-region connection-string nil "~/.lady_last")
    (find-file full-tramp-string)))
