;;; review-guide-local.el --- Changes in a local git repository -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A local git repository as a review-guide source: the changes since
;; a base revision, up to another revision or the working tree.  With
;; no forge to post to, comments stay in the source until submitted,
;; which turns them into one Markdown document, each comment with the
;; lines it is about, for whoever is making the change; typically an
;; agent, before its work is pushed.  The document can be sent straight
;; to an agent working in the repository: see `review-guide-send.el'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'review-guide-diff)
(require 'review-guide-source)
(require 'review-guide-send)

(declare-function review-guide-open "review-guide")

(cl-defstruct (review-guide-local (:constructor review-guide-local-create)
                                  (:copier nil))
  "Changes in the git repository at DIRECTORY since BASE."
  (directory nil :documentation "The repository's top level.")
  (base nil :documentation "The commit the changes are measured from.")
  (base-label nil :documentation "What BASE was chosen as, for display.")
  (head nil :documentation "The revision the changes end at, or nil for the
working tree, including uncommitted and untracked files.")
  (threads nil :documentation "Pending comments, newest first."))

;;;; Git

(defun review-guide-local--git (directory ok &rest args)
  "Run git with ARGS in DIRECTORY and return its output.
Signal an error unless git exits with a status in OK."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil '(t nil) nil args)))
        (unless (memq status ok)
          (error "git %s exited with status %s" (string-join args " ") status))
        (buffer-string)))))

(defun review-guide-local--rev (directory rev)
  "Return the commit REV names in DIRECTORY, or nil if it names none."
  (let ((out (review-guide-local--git directory '(0 1 128) "rev-parse" "--verify"
                                      "--quiet" "--end-of-options"
                                      (concat rev "^{commit}"))))
    (unless (string-empty-p out) (string-trim out))))

(defconst review-guide-local--diff-args
  ;; Explicit a/ and b/ prefixes, plain text, and real file contents,
  ;; whatever the user's git configuration says.
  '("-c" "core.quotePath=false" "diff" "--no-color" "--no-ext-diff"
    "--no-textconv" "--src-prefix=a/" "--dst-prefix=b/"))

(defun review-guide-local--untracked-diff (directory)
  "Return a diff adding each untracked, unignored file in DIRECTORY."
  (mapconcat
   (lambda (file)
     (apply #'review-guide-local--git directory '(0 1)
            (append review-guide-local--diff-args
                    (list "--no-index" "--" "/dev/null" file))))
   (split-string (review-guide-local--git directory '(0) "ls-files" "-z" "--others"
                                          "--exclude-standard")
                 "\0" t)
   ""))

;;;; The source

(cl-defmethod review-guide-source-diff ((source review-guide-local))
  (let ((directory (review-guide-local-directory source))
        (head (review-guide-local-head source)))
    (concat (apply #'review-guide-local--git directory '(0)
                   (append review-guide-local--diff-args
                           (list (review-guide-local-base source))
                           (and head (list head))
                           '("--")))
            (unless head
              (review-guide-local--untracked-diff directory)))))

(cl-defmethod review-guide-source-describe ((source review-guide-local))
  (let* ((directory (review-guide-local-directory source))
         (head (review-guide-local-head source))
         (branch (string-trim (review-guide-local--git
                               directory '(0 128) "rev-parse" "--abbrev-ref" "HEAD"))))
    (list (format "%s %s..%s"
                  (file-name-nondirectory (directory-file-name directory))
                  (review-guide-local-base-label source)
                  (or head "working tree"))
          (format "Local changes on %s" (or head branch))
          (let ((log (review-guide-local--git
                      directory '(0) "log" "--reverse" "--format=* %s%n%n%b"
                      (format "%s..%s" (review-guide-local-base source)
                              (or head "HEAD")))))
            (unless (string-empty-p (string-trim log))
              (concat "Commit messages:\n\n" log))))))

(cl-defmethod review-guide-source-threads ((source review-guide-local))
  (review-guide-local-threads source))

(cl-defmethod review-guide-source-add-thread
  ((source review-guide-local) thread initial on-add)
  (review-guide-local--edit
   (format "Comment on %s." (alist-get 'path thread))
   initial
   (lambda (body)
     (push (append thread `((body . ,body))) (review-guide-local-threads source))
     (funcall on-add))))

(cl-defmethod review-guide-source-delete-thread
  ((source review-guide-local) thread)
  (setf (review-guide-local-threads source)
        (delq thread (review-guide-local-threads source))))

(cl-defmethod review-guide-source-visit ((source review-guide-local) path side line)
  (let* ((directory (review-guide-local-directory source))
         (rev (if (equal side "LEFT")
                  (review-guide-local-base source)
                (review-guide-local-head source))))
    (pop-to-buffer
     (if rev
         (review-guide-local--revision-buffer directory rev path)
       (find-file-noselect (expand-file-name path directory))))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun review-guide-local--revision-buffer (directory rev path)
  "Return a read-only buffer holding PATH as of REV in DIRECTORY."
  (let ((name (format "%s@%s" path (substring rev 0 (min 10 (length rev))))))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (insert (review-guide-local--git directory '(0) "show"
                                           (format "%s:%s" rev path)))
          (let ((buffer-file-name (expand-file-name path directory)))
            (set-auto-mode))
          (setq buffer-read-only t)
          (set-buffer-modified-p nil)
          (current-buffer)))))

(defvar-local review-guide-local--feedback nil
  "(SOURCE THREADS ON-DONE WINDOWS) for a feedback buffer.")

(cl-defmethod review-guide-source-submit ((source review-guide-local) on-done)
  "Show the pending comments as one Markdown document to edit and send.
Sending it, with \\<review-guide-feedback-mode-map>\\[review-guide-feedback-send], \
clears the comments it holds and calls ON-DONE."
  (let ((threads (review-guide-local-threads source))
        (name (car (review-guide-source-describe source)))
        (windows (current-window-configuration)))
    (unless threads
      (user-error "No pending comments"))
    (pop-to-buffer
     (with-current-buffer (get-buffer-create (format "*review-guide feedback %s*" name))
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (review-guide-local-feedback
                  name (review-guide-diff-parse (review-guide-source-diff source))
                  threads)))
       (if (fboundp 'gfm-mode) (gfm-mode) (text-mode))
       (review-guide-feedback-mode)
       (setq review-guide-local--feedback (list source threads on-done windows)
             header-line-format
             (substitute-command-keys
              (concat "Send with \\[review-guide-feedback-send],"
                      " cancel with \\[review-guide-feedback-cancel]")))
       (goto-char (point-min))
       (current-buffer)))))

;;;; Sending feedback

(defun review-guide-local--read-target ()
  "Read where to send this buffer's feedback, as (LABEL . SEND)."
  (let* ((source (car review-guide-local--feedback))
         (targets (review-guide-send-targets (review-guide-local-directory source)))
         (label (completing-read
                 (format-prompt "Send feedback to" (caar targets))
                 targets nil t nil nil (caar targets))))
    (assoc label targets)))

(defun review-guide-feedback-send (target)
  "Send this buffer's feedback to TARGET, a (LABEL . SEND).
The comments it was made from are then cleared from the guide.
Interactively, choose among the agents and terminals working in the
repository, or the kill ring; see `review-guide-send-target-functions'."
  (interactive (list (review-guide-local--read-target)))
  (pcase-let ((`(,source ,threads ,on-done ,windows) review-guide-local--feedback)
              (text (string-trim (buffer-string))))
    (funcall (cdr target) text)
    (setf (review-guide-local-threads source)
          (cl-set-difference (review-guide-local-threads source) threads :test #'eq))
    (kill-buffer)
    (when windows
      (set-window-configuration windows))
    (funcall on-done)
    (message "Sent %d comments to %s" (length threads) (car target))))

(defun review-guide-feedback-cancel ()
  "Discard this feedback buffer, keeping the comments in the guide."
  (interactive)
  (let ((windows (nth 3 review-guide-local--feedback)))
    (kill-buffer)
    (when windows
      (set-window-configuration windows))))

(defvar-keymap review-guide-feedback-mode-map
  "C-c C-c" #'review-guide-feedback-send
  "C-c C-k" #'review-guide-feedback-cancel)

(define-minor-mode review-guide-feedback-mode
  "Minor mode for review feedback on its way to whoever is making the change."
  :keymap review-guide-feedback-mode-map)

;;;; Feedback

(defun review-guide-local--describe-lines (thread)
  "Return where THREAD's lines are, in words."
  (let-alist thread
    (let ((start-side (or .startSide .side))
          (start (or .startLine .line))
          (removed (lambda (side) (if (equal side "LEFT") "removed line" "line"))))
      (cond
       ((and (equal start-side .side) (eql start .line))
        (format "%s %d" (funcall removed .side) .line))
       ((equal start-side .side)
        (format "%ss %d-%d" (funcall removed .side) start .line))
       (t
        (format "%s %d to %s %d" (funcall removed start-side) start
                (funcall removed .side) .line))))))

(defun review-guide-local--excerpt (hunks thread)
  "Return the diff lines THREAD covers in HUNKS, or nil if none hold them."
  (let-alist thread
    (let ((first (list (or .startSide .side) (or .startLine .line)))
          (last (list .side .line)))
      (cl-some
       (lambda (hunk)
         (when (equal (review-guide-hunk-path hunk) .path)
           (let* ((coords (review-guide-hunk-coordinates hunk))
                  (from (cl-position first coords :key #'cdr :test #'equal))
                  (to (cl-position last coords :key #'cdr :test #'equal)))
             (when (and from to (<= from to))
               (mapcar #'car (cl-subseq coords from (1+ to)))))))
       hunks))))

(defun review-guide-local--fence (text)
  "Return a code fence longer than any run of backticks in TEXT."
  (let ((longest 2) (start 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (make-string (1+ longest) ?`)))

(defun review-guide-local-feedback (name hunks threads)
  "Return THREADS on the change NAME as a Markdown document.
Each comment is headed by its file and lines and quotes the lines of
HUNKS it covers, so that it stands on its own when pasted elsewhere."
  (concat
   (format "Review comments on %s.\n" name)
   (mapconcat
    (lambda (thread)
      (let ((excerpt (review-guide-local--excerpt hunks thread)))
        (concat
         (format "\n## %s, %s\n\n" (alist-get 'path thread)
                 (review-guide-local--describe-lines thread))
         (when excerpt
           (let ((code (string-join excerpt "\n")))
             (format "%sdiff\n%s\n%s\n\n" (review-guide-local--fence code) code
                     (review-guide-local--fence code))))
         (string-trim (alist-get 'body thread))
         "\n")))
    (sort (copy-sequence threads)
          (lambda (a b)
            (let ((pa (alist-get 'path a)) (pb (alist-get 'path b)))
              (if (equal pa pb)
                  (< (or (alist-get 'startLine a) (alist-get 'line a))
                     (or (alist-get 'startLine b) (alist-get 'line b)))
                (string< pa pb)))))
    "")))

;;;; Writing a comment

(defvar-local review-guide-local--on-finish nil)
(defvar-local review-guide-local--windows nil)

(defun review-guide-comment-finish ()
  "Finish the comment in this buffer and add it."
  (interactive)
  (let ((body (string-trim (buffer-string)))
        (on-finish review-guide-local--on-finish))
    (when (string-empty-p body)
      (user-error "The comment is empty; abort it instead"))
    (review-guide-comment-abort)
    (funcall on-finish body)))

(defun review-guide-comment-abort ()
  "Discard the comment in this buffer."
  (interactive)
  (let ((windows review-guide-local--windows))
    (kill-buffer)
    (when windows
      (set-window-configuration windows))))

(defvar-keymap review-guide-comment-mode-map
  "C-c C-c" #'review-guide-comment-finish
  "C-c C-k" #'review-guide-comment-abort)

(define-minor-mode review-guide-comment-mode
  "Minor mode for writing a review comment."
  :keymap review-guide-comment-mode-map)

(defun review-guide-local--edit (description initial on-finish)
  "Edit a comment, starting from INITIAL, then call ON-FINISH with it.
DESCRIPTION heads the buffer."
  (let ((windows (current-window-configuration)))
    (pop-to-buffer
     (with-current-buffer (generate-new-buffer "*review-guide comment*")
       (if (fboundp 'gfm-mode) (gfm-mode) (text-mode))
       (review-guide-comment-mode)
       (setq review-guide-local--on-finish on-finish
             review-guide-local--windows windows
             header-line-format
             (substitute-command-keys
              (concat description "  Finish with \\[review-guide-comment-finish],"
                      " abort with \\[review-guide-comment-abort]")))
       (when initial
         (insert initial)
         (goto-char (point-min)))
       (current-buffer)))))

;;;; Entry point

(defconst review-guide-local--default-bases
  '("@{upstream}" "origin/HEAD" "origin/main" "main" "origin/master" "master")
  "Revisions to measure local changes from, most preferred first.")

(defun review-guide-local--read-args ()
  "Read the arguments for `review-guide-local'.
Without a prefix argument, review the working tree against the first
of `review-guide-local--default-bases' that exists; with one, ask."
  (let* ((directory (string-trim (review-guide-local--git default-directory '(0)
                                                          "rev-parse" "--show-toplevel")))
         (default (cl-find-if (lambda (rev) (review-guide-local--rev directory rev))
                              review-guide-local--default-bases)))
    (if (not current-prefix-arg)
        (list directory (or default (user-error "No upstream or default branch to compare with")))
      (let ((refs (split-string (review-guide-local--git
                                 directory '(0) "for-each-ref" "--format=%(refname:short)")
                                "\n" t)))
        (list directory
              (completing-read (format-prompt "Review changes since" default)
                               refs nil nil nil nil default)
              (let ((head (completing-read "Up to (empty for the working tree): "
                                           refs nil nil)))
                (unless (string-empty-p head) head)))))))

;;;###autoload
(defun review-guide-local (directory base &optional head)
  "Show the review guide for changes in the git repository at DIRECTORY.
The changes are measured from where BASE and HEAD diverged, and end at
HEAD, or include the working tree when HEAD is nil: uncommitted
changes and untracked files that are not ignored.

Interactively, review the working tree of the current repository
against the branch's upstream, so as to see what is not pushed, or
when there is none, against the remote's default branch.  With a
prefix argument, ask for BASE and HEAD."
  (interactive (review-guide-local--read-args))
  (let ((merge-base (string-trim (review-guide-local--git
                                  directory '(0 1 128) "merge-base" base
                                  (or head "HEAD")))))
    (review-guide-open
     (review-guide-local-create
      :directory (file-name-as-directory directory)
      :base (if (string-empty-p merge-base)
                (or (review-guide-local--rev directory base)
                    (user-error "No such revision: %s" base))
              merge-base)
      :base-label base
      :head head))))

(provide 'review-guide-local)
;;; review-guide-local.el ends here
