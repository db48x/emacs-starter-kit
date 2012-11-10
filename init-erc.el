;;;; http://www.informatimago.com/develop/emacs/emacs/pjb-erc.el
;;; GPL

(defun string/=* (string1 string2 &rest cl-keys)
  ;; &key :start1 :end1 :start2 :end2)
  "Common-Lisp: compares two strings and return whether they're different.

        The keyword arguments :start1 and :start2 are the places in
        the strings to start the comparison. The arguments :end1 and
        :end2 are the places in the strings to stop comparing;
        comparison stops just before the position specified by a
        limit. The ``start'' arguments default to zero (beginning of
        string), and the ``end'' arguments (if either omitted or nil)
        default to the lengths of the strings (end of string), so that
        by default the entirety of each string is examined. These
        arguments are provided so that substrings can be compared
        efficiently.
"
  (not (apply 'string= string1 string2 cl-keys)))

(defun nsplit-list-on-indicator (list indicator)
  "
RETURN: a list of sublists of list (the conses from list are reused),
        the list is splited between items a and b for which (indicator a b).
"
  (let* ((result nil)
         (sublist list)
         (current list)
         (next    (cdr current)))
    (loop while next do
         (if (funcall indicator (car current) (car next))
             (progn ;; split
               (setf (cdr current) nil)
               (push sublist result)
               (setq current next)
               (setq next (cdr current))
               (setq sublist current))
             (progn ;; keep
               (setq current next)
               (setq next (cdr current)))))
    (push sublist result)
    (nreverse result)))

(unless (fboundp (quote netrc-parse))
  (defalias 'netrc-point-at-eol
      (if (fboundp 'point-at-eol)
          'point-at-eol
          'line-end-position))
  (defun netrc-parse (file)
    "Parse FILE and return a list of all entries in the file."
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((tokens '("machine" "default" "login"
                        "password" "account" "macdef" "force"
                        "port"))
              alist elem result pair)
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Go through the file, line by line.
          (while (not (eobp))
            (narrow-to-region (point) (netrc-point-at-eol))
            ;; For each line, get the tokens and values.
            (while (not (eobp))
              (skip-chars-forward "\t ")
              ;; Skip lines that begin with a "#".
              (if (eq (char-after) ?#)
                  (goto-char (point-max))
                  (unless (eobp)
                    (setq elem
                          (if (= (following-char) ?\")
                              (read (current-buffer))
                              (buffer-substring
                               (point) (progn (skip-chars-forward "^\t ")
                                              (point)))))
                    (cond
                      ((equal elem "macdef")
                       ;; We skip past the macro definition.
                       (widen)
                       (while (and (zerop (forward-line 1))
                                   (looking-at "$")))
                       (narrow-to-region (point) (point)))
                      ((member elem tokens)
                       ;; Tokens that don't have a following value are ignored,
                       ;; except "default".
                       (when (and pair (or (cdr pair)
                                           (equal (car pair) "default")))
                         (push pair alist))
                       (setq pair (list elem)))
                      (t
                       ;; Values that haven't got a preceding token are ignored.
                       (when pair
                         (setcdr pair elem)
                         (push pair alist)
                         (setq pair nil)))))))
            (when alist
              (push (nreverse alist) result))
            (setq alist nil
                  pair nil)
            (widen)
            (forward-line 1))
          (nreverse result))))))


;(defvar server erc-server        "For get-password")
;(defvar nick   (first erc-nick)  "For get-password")
(defun get-password (server nick)
  (cdr (assoc "password"
              (car
               (delete-if
                (lambda (entry)
                  (or (string/=* (cdr (assoc "machine" entry)) server)
                      (string/=* (cdr (assoc "login"   entry)) nick)))
                (nsplit-list-on-indicator
                 (reduce 'nconc (netrc-parse "~/.netrc"))
                 (lambda (current next) (string= "machine" (car next)))))))))

(defun pjb-set-erc-nickserv-passwords ()
  (setf erc-nickserv-passwords
        (mapcar (lambda (nickserv)
                  (list (second nickserv)
                        (mapcar
                         (lambda (entry)
                           (cons (cdr (assoc "login" entry))
                                 (cdr (assoc "password" entry))))
                         (delete-if
                          (lambda (entry)
                            (string/=* (cdr (assoc "machine" entry))
                                       (second nickserv)))
                          (nsplit-list-on-indicator
                           (reduce 'nconc (netrc-parse "~/.netrc"))
                           (lambda (current next) (string= "machine" (car next))))))))
                '((freenode "irc.freenode.org")
                  (moznet "irc.mozilla.org")))))

(pjb-set-erc-nickserv-passwords)

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))
