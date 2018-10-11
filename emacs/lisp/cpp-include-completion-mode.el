;; cpp-include-completion-mode.el --- minor mode for #include completion

;;; Commentary:

;; This minor mode is useful when editing files that use the C
;; preprocessor.  It takes over the tab key when you are on a line
;; that looks like an include directive, and provides filename
;; completion in the include string.

;;; Code:

(defvar cpp-include-completion-path
  (list "/usr/include"
        "/usr/local/include")
  "*List of directories to look at when doing include completion.")

;; Internal function to figure out what we should do when tab is
;; pressed if we aren't on a #include line.
(defun cpp-include--call-normal-tab-key ()
  (catch 'done
    ;; If other minor modes are doing similar tab key filtering, we'd
    ;; like to play nice about it, so we check for bindings in any of
    ;; the other minor mode maps.
    (let ((minor-list (minor-mode-key-binding "\t")))
      (when minor-list
        (dolist (l minor-list)
          (when (not (eq (car l) 'cpp-include-completion-mode))
            (funcall (cdr l))
            (throw 'done t)))))
    ;; Try the local map and then global map next.
    (dolist (map (list (current-local-map) (current-global-map)))
      (let ((f (lookup-key map "\t")))
        (when f
          (funcall f)
          (throw 'done t))))
    (error "Couldn't figure out what to do for tab key")))

;; Remove any adjacent duplicate key elements in a list, using the
;; supplied function to test for equality.
(defun cpp-include--uniq (list function)
  (let ((c list))
    (while (and c (cdr c))
      (while (apply function (list (car c) (cadr c)))
        (setcdr c (cddr c)))
      (setq c (cdr c))))
  list)

(defun cpp-include--list-all (file dir-base)
  "List all the completions of FILE in DIR-BASE.
Unlike `file-name-all-completions' this support subdirectories properly."
  (let ((dirpart (file-name-directory file)))
    (if dirpart
        (let ((effective-dir (concat dir-base "/" dirpart)))
          (if (file-exists-p effective-dir)
              (mapcar (lambda (x) (concat dirpart x))
                      (file-name-all-completions (file-name-nondirectory file)
                                                 effective-dir))
            nil))
      (file-name-all-completions file dir-base))))

;; Determine the unique set of possible filename completions in all
;; the directories on `cpp-include-completion-path'.
(defun cpp-include--get-completion-set (path-so-far)
  (cpp-include--uniq
   (sort (apply 'append
                (mapcar (lambda (x) (cpp-include--list-all path-so-far x))
                        cpp-include-completion-path))
         'string-lessp)
   'string=))

;; Returns a list containing the best value so far and a completion
;; prompt string.  If it is an exact match, the prompt string will be
;; nil.
(defun cpp-include--complete (path-so-far)
  (let ((completion-set (cpp-include--get-completion-set path-so-far)))
    (if (not completion-set)
        (list path-so-far "[no match]")
      ;; If there's only one possible complete, just use that.
      (if (= 1 (length completion-set))
          (list (car completion-set) nil)
        (let* ((completable (try-completion path-so-far
                                            (mapcar (lambda (x) (list x))
                                                    completion-set)))
               (prompt (concat "{" (mapconcat
                                    (lambda (x)
                                      (substring x (length completable)))
                                    completion-set ",")
                               "}")))
          (when (> (length prompt) 70)
            (setq prompt (concat (substring prompt 0 70) "...}")))
          (list completable prompt))))))

(defun cpp-include-completion-electric-tab ()
  "Facilitates completion of include directives or does a normal tab."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at
         "\\([ \t]*#[ \t]*include\\) \\([\"<]\\)\\([[:alnum:]\\.\\/_]*\\)"))
      (progn
        (let* ((directive (match-string 1))
               (start-quote (match-string 2))
               (end-quote (if (string= start-quote "<") ">" start-quote))
               (path-so-far (match-string 3))
               (complete-info (cpp-include--complete path-so-far)))
          (beginning-of-line)
          (delete-region (point) (save-excursion (end-of-line) (point)))
          (insert (concat directive " " start-quote (car complete-info)))
          (save-excursion
            (insert end-quote))
          (when (cadr complete-info)
            (momentary-string-display (cadr complete-info) (1+ (point))))))
    (cpp-include--call-normal-tab-key)))

(defvar cpp-include-completion-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\t" 'cpp-include-completion-electric-tab)
    m))

(define-minor-mode cpp-include-completion-mode
  "Minor mode to do filename completion on #include lines.
With ARG, turn mode on if ARG is postive, off otherwise.

The list of directories to look in for files is stored in the variable
`cpp-include-completion-path'."
  :lighter " inc")

(provide 'cpp-include-completion-mode)

;;; cpp-include-completion-mode.el ends here
