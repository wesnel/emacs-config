;;; review-guide.el --- Read pull requests in model-guided chunks -*- lexical-binding: t; -*-

;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit-section "4.0") (pr-review "0.1") (gptel "0.9.8"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A guide buffer for a change under review.  A model arranges the
;; diff's hunks into chunks to read in order, each with a summary, and
;; can review the diff itself.  Its comments appear inline as
;; proposals; forwarding one, after editing it, adds it to the pending
;; comments, alongside those written in the guide.  The source of the
;; diff decides where pending comments go on submission: see
;; `review-guide-source.el'.
;;
;; Entry point: `review-guide', for a pull request open in `pr-review',
;; from a forge pull request, or from a pull request URL.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'review-guide-diff)
(require 'review-guide-ai)
(require 'review-guide-source)
(require 'review-guide-pr-review)

;;;; State

(defvar-local review-guide--source nil
  "Where this guide's diff comes from, and its comments go.")
(defvar-local review-guide--diff nil
  "The unified diff `review-guide--hunks' was parsed from.")
(defvar-local review-guide--hunks nil)
(defvar-local review-guide--overview nil)
(defvar-local review-guide--chunks nil
  "The model's chunks, or nil before it has answered.")
(defvar-local review-guide--review-summary nil)
(defvar-local review-guide--findings nil)
(defvar-local review-guide--busy nil
  "What the guide is waiting for from the model, as text, or nil.")

;;;; Rendering

(defface review-guide-summary '((t :inherit font-lock-doc-face))
  "Face for the model's summaries.")

(defface review-guide-finding '((t :inherit font-lock-warning-face))
  "Face for the heading of a comment the model proposes.")

(defface review-guide-finding-settled '((t :inherit shadow))
  "Face for a proposed comment already forwarded or dismissed.")

(defface review-guide-pending '((t :inherit success))
  "Face for the heading of a comment in the pending review.")

(defun review-guide--insert-paragraph (text face &optional indent)
  "Insert TEXT in FACE, indented by INDENT columns.
TEXT is Markdown that may hold code, so it is wrapped for display
rather than filled."
  (let ((prefix (make-string (or indent 0) ?\s))
        (start (point)))
    (dolist (line (split-string (string-trim text) "\n"))
      (insert prefix line "\n"))
    (add-text-properties start (point) `(wrap-prefix ,prefix))
    (add-face-text-property start (point) face)))

(defun review-guide--insert-finding (finding)
  "Insert the proposed comment FINDING."
  (magit-insert-section (review-guide-finding finding)
    (let ((state (review-guide-finding-state finding)))
      (magit-insert-heading
        (propertize
         (format "    AI %s on %s%s%s"
                 (review-guide-finding-severity finding)
                 (review-guide-finding-side finding)
                 (if (< (review-guide-finding-start-line finding)
                        (review-guide-finding-line finding))
                     (format " %d-%d" (review-guide-finding-start-line finding)
                             (review-guide-finding-line finding))
                   (format " %d" (review-guide-finding-line finding)))
                 (if (eq state 'proposed) "" (format " (%s)" state)))
         'face (if (eq state 'proposed)
                   'review-guide-finding
                 'review-guide-finding-settled)))
      (review-guide--insert-paragraph (review-guide-finding-body finding)
                                      'default 6))))

(defun review-guide--insert-thread (thread)
  "Insert the pending review THREAD."
  (magit-insert-section (review-guide-thread thread)
    (magit-insert-heading
      (propertize (format "    Pending comment on %s %s"
                          (alist-get 'side thread) (alist-get 'line thread))
                  'face 'review-guide-pending))
    (review-guide--insert-paragraph (alist-get 'body thread) 'default 6)))

(defun review-guide--diff-face (text)
  "Return the face for the diff line TEXT."
  (pcase (and (> (length text) 0) (aref text 0))
    (?+ 'magit-diff-added)
    (?- 'magit-diff-removed)
    (_ 'magit-diff-context)))

(defun review-guide--insert-hunk (hunk threads)
  "Insert HUNK with the findings and pending THREADS that end on its lines."
  (let ((path (review-guide-hunk-path hunk)))
    (magit-insert-section (review-guide-hunk hunk)
      (magit-insert-heading
        (propertize (format "%s  %s" path (review-guide-hunk-header hunk))
                    'face 'magit-diff-hunk-heading))
      (pcase-dolist (`(,text ,side ,line) (review-guide-hunk-coordinates hunk))
        (insert (propertize (concat text "\n")
                            'face (review-guide--diff-face text)
                            'review-guide-line (list path side line)))
        (dolist (thread threads)
          (when (and (equal (alist-get 'path thread) path)
                     (equal (alist-get 'side thread) side)
                     (eql (alist-get 'line thread) line))
            (review-guide--insert-thread thread)))
        (dolist (finding review-guide--findings)
          (when (and (equal (review-guide-finding-hunk finding)
                            (review-guide-hunk-id hunk))
                     (equal (review-guide-finding-side finding) side)
                     (eql (review-guide-finding-line finding) line))
            (review-guide--insert-finding finding)))))))

(defun review-guide--insert-chunk (index chunk threads)
  "Insert CHUNK, numbered INDEX, with the pending THREADS on its lines."
  (magit-insert-section (review-guide-chunk chunk)
    (magit-insert-heading
      (propertize (format "%d. %s" index (review-guide-chunk-title chunk))
                  'face 'magit-section-heading))
    (when-let* ((summary (review-guide-chunk-summary chunk)))
      (review-guide--insert-paragraph summary 'review-guide-summary 3))
    (dolist (hunk (review-guide-chunk-hunks chunk))
      (review-guide--insert-hunk hunk threads))
    (insert "\n")))

(defun review-guide--render ()
  "Redraw the guide from its state, keeping point near where it was."
  (pcase-let ((`(,name ,title ,_) (review-guide-source-describe review-guide--source))
              (threads (review-guide-source-threads review-guide--source))
              (line (line-number-at-pos))
              (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (review-guide-root)
      (insert (propertize (format "%s  %s\n" name (or title ""))
                          'face 'magit-section-heading))
      (insert (format "%d pending comments, %d proposed by the model%s\n\n"
                      (length threads)
                      (cl-count 'proposed review-guide--findings
                                :key #'review-guide-finding-state)
                      (if review-guide--busy
                          (format "; %s" review-guide--busy)
                        "")))
      (when review-guide--overview
        (review-guide--insert-paragraph review-guide--overview 'review-guide-summary)
        (insert "\n"))
      (when review-guide--review-summary
        (insert (propertize "Model review\n" 'face 'magit-section-heading))
        (review-guide--insert-paragraph review-guide--review-summary 'review-guide-summary)
        (insert "\n"))
      (cl-loop for chunk in (or review-guide--chunks
                                (list (review-guide-chunk-create
                                       :title "All changes, in diff order"
                                       :hunks review-guide--hunks)))
               for index from 1
               do (review-guide--insert-chunk index chunk threads)))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun review-guide--rerender (buffer)
  "Redraw the guide BUFFER if it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (review-guide--render))))

;;;; Commands

(defun review-guide--line-at (pos)
  "Return (PATH SIDE LINE) for the diff line at POS, or nil."
  (get-text-property (save-excursion (goto-char pos) (line-beginning-position))
                     'review-guide-line))

(defun review-guide--section-value (type)
  "Return the value of the current section when it has TYPE."
  (when-let* ((section (magit-current-section)))
    (when (eq (oref section type) type)
      (oref section value))))

(defun review-guide--failed (what)
  "Return a function that reports WHAT failing, for a model request."
  (lambda (reason)
    (setq review-guide--busy nil)
    (review-guide--render)
    (message "review-guide: %s failed: %s" what reason)))

(defun review-guide-refresh ()
  "Re-read the diff from the guide's source and redraw.
For a pull request, refresh the `pr-review' buffer first to fetch new
commits.  When the diff has changed, the model's chunks and comments
refer to hunks that no longer exist, so they are discarded."
  (interactive)
  (let ((diff (review-guide-source-diff review-guide--source)))
    (unless (equal diff review-guide--diff)
      (setq review-guide--diff diff
            review-guide--hunks (review-guide-diff-parse diff)
            review-guide--overview nil
            review-guide--chunks nil
            review-guide--review-summary nil
            review-guide--findings nil)))
  (review-guide--render))

(defun review-guide-arrange ()
  "Ask the model to arrange the diff into chunks to read in order."
  (interactive)
  (pcase-let ((`(,_ ,title ,description) (review-guide-source-describe review-guide--source))
              (buffer (current-buffer)))
    (setq review-guide--busy "arranging chunks")
    (review-guide--render)
    (review-guide-ai-group
     title description review-guide--hunks buffer
     (lambda (overview chunks)
       (setq review-guide--overview overview
             review-guide--chunks chunks
             review-guide--busy nil)
       (review-guide--render))
     (review-guide--failed "arranging chunks"))))

(defun review-guide-review ()
  "Ask the model to review the diff, and show its comments as proposals.
The comments stay local until forwarded with `review-guide-forward'."
  (interactive)
  (pcase-let ((`(,_ ,title ,description) (review-guide-source-describe review-guide--source))
              (buffer (current-buffer)))
    (setq review-guide--busy "reviewing")
    (review-guide--render)
    (review-guide-ai-review
     title description review-guide--hunks buffer
     (lambda (summary findings dropped)
       (setq review-guide--review-summary summary
             review-guide--findings findings
             review-guide--busy nil)
       (review-guide--render)
       (when (> dropped 0)
         (message "Dropped %d model comments that cite lines outside the diff"
                  dropped)))
     (review-guide--failed "reviewing"))))

(defun review-guide-visit ()
  "Show the diff line at point where the guide's source keeps it."
  (interactive)
  (pcase (review-guide--line-at (point))
    (`(,path ,side ,line) (review-guide-source-visit review-guide--source path side line))
    (_ (user-error "Not on a diff line"))))

(defun review-guide-comment (beg end)
  "Add a comment on the diff line at point to the pending review.
With an active region from BEG to END, comment on the range of lines
it covers, which must lie in one file."
  (interactive (if (use-region-p)
                   (list (region-beginning) (1- (region-end)))
                 (list (point) (point))))
  (pcase-let ((`(,path ,side ,line) (review-guide--line-at end))
              (`(,start-path ,start-side ,start-line) (review-guide--line-at beg))
              (buffer (current-buffer)))
    (unless (and path start-path)
      (user-error "The comment must start and end on diff lines"))
    (unless (equal path start-path)
      (user-error "A comment on a range of lines must stay in one file"))
    (deactivate-mark)
    (review-guide-source-add-thread
     review-guide--source
     (append (unless (and (equal side start-side) (eql line start-line))
               `((startLine . ,start-line) (startSide . ,start-side)))
             `((path . ,path) (line . ,line) (side . ,side)))
     nil
     (lambda () (review-guide--rerender buffer)))))

(defun review-guide-forward ()
  "Edit the model's comment at point, then add it to the pending review."
  (interactive)
  (let ((finding (or (review-guide--section-value 'review-guide-finding)
                     (user-error "Not on a comment the model proposed")))
        (buffer (current-buffer)))
    (when (eq (review-guide-finding-state finding) 'forwarded)
      (user-error "Already forwarded"))
    (review-guide-source-add-thread
     review-guide--source
     (review-guide-finding-thread finding)
     (review-guide-finding-body finding)
     (lambda ()
       (setf (review-guide-finding-state finding) 'forwarded)
       (review-guide--rerender buffer)))))

(defun review-guide-dismiss ()
  "Dismiss the model's comment at point, or restore a dismissed one.
On a pending comment, delete it."
  (interactive)
  (if-let* ((thread (review-guide--section-value 'review-guide-thread)))
      (when (y-or-n-p "Delete this pending comment? ")
        (review-guide-source-delete-thread review-guide--source thread))
    (let ((finding (or (review-guide--section-value 'review-guide-finding)
                       (user-error "Not on a comment"))))
      (setf (review-guide-finding-state finding)
            (pcase (review-guide-finding-state finding)
              ('proposed 'dismissed)
              ('dismissed 'proposed)
              (_ (user-error "Already forwarded"))))))
  (review-guide--render))

(defun review-guide-submit ()
  "Send the pending comments on to wherever the guide's source sends them.
For a pull request, that is a review submitted from `pr-review'."
  (interactive)
  (review-guide-source-submit review-guide--source))

(defvar-keymap review-guide-mode-map
  :parent magit-section-mode-map
  "g" #'review-guide-refresh
  "A" #'review-guide-arrange
  "R" #'review-guide-review
  "RET" #'review-guide-visit
  "c" #'review-guide-comment
  "f" #'review-guide-forward
  "k" #'review-guide-dismiss
  "s" #'review-guide-submit
  "q" #'quit-window)

(define-derived-mode review-guide-mode magit-section-mode "Review-Guide"
  "Read a change in chunks, and review it with a model.

\\{review-guide-mode-map}"
  :interactive nil
  :group 'review-guide
  (setq-local font-lock-defaults nil))

;;;; Entry point

(defun review-guide-open (source)
  "Show the review guide for SOURCE, creating it if needed.
A new guide asks the model to arrange the diff into chunks straight
away.  A guide is identified by its source's name, so opening the same
change again shows the existing guide with its source replaced."
  (let* ((name (car (review-guide-source-describe source)))
         (buffer (get-buffer-create (format "*review-guide %s*" name)))
         (new (not (buffer-local-value 'review-guide--source buffer))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'review-guide-mode)
        (review-guide-mode))
      (setq review-guide--source source)
      (review-guide-refresh)
      (when new (review-guide-arrange)))
    (pop-to-buffer buffer)))

(provide 'review-guide)
;;; review-guide.el ends here
