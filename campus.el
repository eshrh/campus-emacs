;;; campus --- sane and simple approach to repl programming

;; Package-Requires: (s dash)

;;; Commentary:

;; Improvement over the default Emacs inferior repl experience and alternate
;; workflow to notebooks.

;;; Code:

(require 's)
(require 'dash)
(defvar campus-partition-string "***"
  "String that is used in a comment to indicate the end of a partition.")

(defvar campus-eval-region-alist
  '(((eq major-mode 'emacs-lisp-mode) . eval-region)
    (t . "C-c C-e"))

  "Alist of ways to send code to an interactive process.
Keys to the alist should be forms that evaluate to t or nil.  Values should
be functions that operate on an active region, or strings that describe key
sequences.  The first item with key that evaluates truthy will have it's cdr
executed.")

(defun campus--partition ()
  "Return the string that partitions code."
  (concat comment-start campus-partition-string comment-end))

(defun campus--insert-partition ()
  "Insert a partition string."
  (insert (campus--partition)))

;;;###autoload
(defun campus-make-partition ()
  "Insert partition on a new line."
  (interactive)
  (let ((text-on-line (->> (thing-at-point 'line t)
                           (s-trim)
                           (string-empty-p)
                           (not)))
        (line-no (current-line)))
    (save-excursion
      (when text-on-line
        (end-of-line)
        (newline)
        (setq line-no (+ 1 line-no)))
      (campus--insert-partition))))

(defmacro campus--remove-partition (searchform)
  "Remove a partition.
SEARCHFORM is a form that takes the point to the line to be deleted."
  `(save-excursion
     (when ,searchform
       (delete-region (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun campus-remove-partition-backward ()
  "Remove the nearest partition backward."
  (interactive)
  (campus--remove-partition (search-backward (campus--partition) nil t)))

;;;###autoload
(defun campus-remove-partition-forward ()
  "Remove the nearest partition forward."
  (interactive)
  (campus--remove-partition (search-forward (campus--partition) nil t)))

;;;###autoload
(defun campus-remove-partitions ()
  "Remove all partitions."
  (interactive)
  (setq-local campus--saved-partitions nil)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (campus--partition) nil t)
      (push (current-line) campus--saved-partitions)
      (campus--remove-partition t))))

(defmacro campus--default (form default)
  "Return FORM if FORM is truthy, otherwise DEFAULT."
  `(let ((result ,form))
     (if result
         result
       ,default)))

(defun campus--get-send-function ()
  "Return the function that operates on the selected region."
  (let ((lookup (->> campus-eval-region-alist
                     (-filter (lambda (el) (eval (car el))))
                     (-first-item)
                     (cdr))))
    (if (stringp lookup)
        (key-binding (read-kbd-macro "C-c C-e"))
      lookup)))

;;;###autoload
(defun campus-send-region ()
  "Send the code in the partition to a region-operating function."
  (interactive)
  (save-excursion
    (push-mark
     (campus--default (search-backward (campus--partition) nil t) 0))
    (goto-char
     (campus--default (search-forward (campus--partition) nil t 2)
                      (point-max)))
    (activate-mark)
    (call-interactively (campus--get-send-function))))

(provide 'campus)
;;; campus.el ends here
