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
;; Entry points: `review-guide', for a pull request open in `pr-review',
;; from a forge pull request, or from a pull request URL;
;; `review-guide-local', for changes in a local repository that are not
;; pushed yet; `review-guide-loop', for the fixups an automated review
;; loop committed on a branch, one chunk each; and
;; `review-guide-record', for a pull request with the findings of the
;; review pr-loop recorded for it.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'review-guide-diff)
(require 'review-guide-ai)
(require 'review-guide-source)
(require 'review-guide-pr-review)
(require 'review-guide-local)
(require 'review-guide-loop)
(require 'review-guide-record)

;;;; State

(defvar-local review-guide--source nil
  "Where this guide's diff comes from, and its comments go.")
(defvar-local review-guide--diff nil
  "The unified diff `review-guide--hunks' was parsed from.")
(defvar-local review-guide--hunks nil)
(defvar-local review-guide--overview nil)
(defvar-local review-guide--chunks nil
  "The source's chunks or the model's, or nil for the diff in order.")
(defvar-local review-guide--review-summary nil)
(defvar-local review-guide--findings nil)
(defvar-local review-guide--record nil
  "The `review-guide-record' whose findings the guide proposes, or nil.")
(defvar-local review-guide--record-actions nil
  "What the operator did with review records' findings in this guide.
An alist from (FILE . ID) to the `review-guide-finding-state' they
gave the finding by forwarding, dismissing or restoring it.  It
outlives the record's findings, so opening the record again keeps
those states, but not the states triage gave.")
(defvar-local review-guide--findings-generation 0
  "Counts the changes of `review-guide--findings' that a pending model
review must not overwrite: a reply to an earlier generation is
dropped.")
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

(defun review-guide--finding-id (finding)
  "Return FINDING's id in its review record, to follow its severity."
  (if-let* ((id (review-guide-finding-id finding))) (concat " " id) ""))

(defun review-guide--finding-lines (finding)
  "Return FINDING's line or range of lines, after a space, or \"\"."
  (let ((start (review-guide-finding-start-line finding))
        (end (review-guide-finding-line finding)))
    (cond ((not end) "")
          ((and start (< start end)) (format " %d-%d" start end))
          (t (format " %d" end)))))

(defun review-guide--finding-outcome (finding)
  "Return how FINDING's review record settled it, in brackets, or \"\"."
  (if (review-guide-finding-id finding)
      (format " [%s]" (or (review-guide-finding-outcome finding) "not triaged"))
    ""))

(defun review-guide--insert-finding (finding)
  "Insert the proposed comment FINDING."
  (magit-insert-section (review-guide-finding finding)
    (let ((state (review-guide-finding-state finding)))
      (magit-insert-heading
        (propertize
         (format "    AI %s%s on %s%s%s%s"
                 (review-guide-finding-severity finding)
                 (review-guide--finding-id finding)
                 (review-guide-finding-side finding)
                 (review-guide--finding-lines finding)
                 (review-guide--finding-outcome finding)
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

(defun review-guide--insert-record (record)
  "Insert what the review RECORD says beyond its findings in the diff.
Its text was cleaned when it was read; see `review-guide-record-text'."
  (let* ((findings (review-guide-record-findings record))
         (outside (cl-remove-if #'review-guide-finding-hunk findings)))
    (magit-insert-section (review-guide-record record)
      (magit-insert-heading
        (propertize (format "Review record of %s at %s"
                            (review-guide-record-key record)
                            (substring (review-guide-record-head record) 0 12))
                    'face 'magit-section-heading))
      (review-guide--insert-paragraph
       (concat
        (if-let* ((round (review-guide-record-round record)))
            (format "Round %d by %s%s.%s\n" round
                    (or (review-guide-record-reviewer record) "an unnamed reviewer")
                    (if-let* ((model (review-guide-record-model record)))
                        (format " (%s)" model)
                      "")
                    (if (review-guide-record-rerun record)
                        "  A later run at this head has not finished a round yet."
                      ""))
          "No review round yet.\n")
        (if (review-guide-record-ran record)
            "The pull request's code ran in the VM.\n"
          (format "The pull request's code did not run%s\n"
                  (if-let* ((why (review-guide-record-not-run-because record)))
                      (concat ": " why)
                    ".")))
        (format "%d to propose, %d for the operator, %d not triaged, %d settled; %d outside the diff."
                (cl-count "propose" findings
                          :key #'review-guide-finding-outcome :test #'equal)
                (cl-count "needs-operator" findings
                          :key #'review-guide-finding-outcome :test #'equal)
                (cl-count nil findings :key #'review-guide-finding-outcome)
                (cl-count-if (lambda (outcome)
                               (and outcome (string-match-p (rx bos (or "rebut" "duplicate-of"))
                                                            outcome)))
                             findings :key #'review-guide-finding-outcome)
                (length outside)))
       'review-guide-summary)
      (dolist (finding outside)
        (magit-insert-section (review-guide-record-finding finding)
          (magit-insert-heading
            (propertize (format "  Outside the diff: %s%s on %s%s%s%s"
                                (review-guide-finding-severity finding)
                                (review-guide--finding-id finding)
                                (or (review-guide-finding-path finding) "no file")
                                (review-guide--finding-lines finding)
                                (review-guide--finding-outcome finding)
                                (if (eq (review-guide-finding-state finding) 'dismissed)
                                    " (dismissed)"
                                  ""))
                        'face (if (eq (review-guide-finding-state finding) 'dismissed)
                                  'review-guide-finding-settled
                                'review-guide-finding)))
          (review-guide--insert-paragraph (review-guide-finding-body finding) 'default 4)))
      (when-let* ((unverified (review-guide-record-unverified record)))
        (insert "  Unverified:\n")
        (dolist (text unverified)
          (review-guide--insert-paragraph (concat "- " text) 'review-guide-summary 4)))
      (insert "\n"))))

(defun review-guide--diff-face (text)
  "Return the face for the diff line TEXT."
  (pcase (and (> (length text) 0) (aref text 0))
    (?+ 'magit-diff-added)
    (?- 'magit-diff-removed)
    (_ 'magit-diff-context)))

(defun review-guide--insert-hunk (hunk threads)
  "Insert HUNK with the findings and pending THREADS that end on its lines."
  (let ((path (review-guide-hunk-path hunk))
        (commit (review-guide-hunk-commit hunk)))
    (magit-insert-section (review-guide-hunk hunk)
      (magit-insert-heading
        (propertize (format "%s  %s" path (review-guide-hunk-header hunk))
                    'face 'magit-diff-hunk-heading))
      (pcase-dolist (`(,text ,side ,line) (review-guide-hunk-coordinates hunk))
        (insert (propertize (concat text "\n")
                            'face (review-guide--diff-face text)
                            'review-guide-line (list path side line commit)))
        (dolist (thread threads)
          (when (and (equal (alist-get 'path thread) path)
                     (equal (alist-get 'commit thread) commit)
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
      (insert (format "%d pending comments, %d proposed by %s%s\n\n"
                      (length threads)
                      (cl-count 'proposed review-guide--findings
                                :key #'review-guide-finding-state)
                      (if review-guide--record "the review record" "the model")
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
      (when review-guide--record
        (review-guide--insert-record review-guide--record)
        (run-hook-with-args 'review-guide-record-section-functions review-guide--record))
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
  "Return (PATH SIDE LINE COMMIT) for the diff line at POS, or nil.
COMMIT is the `review-guide-hunk-commit' of the line's hunk."
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
and a review record's findings refer to hunks that no longer exist,
so they are discarded, along with the reply to a model review still
under way, and the source's own chunks, if it has any, take their
place."
  (interactive)
  (let ((diff (review-guide-source-diff review-guide--source)))
    (unless (equal diff review-guide--diff)
      (setq review-guide--diff diff
            review-guide--hunks (review-guide-diff-parse diff)
            review-guide--overview nil
            review-guide--chunks (review-guide-source-chunks review-guide--source
                                                             review-guide--hunks)
            review-guide--review-summary nil
            review-guide--findings nil
            review-guide--record nil)
      (review-guide--drop-pending-review)))
  (review-guide--render))

(defun review-guide--drop-pending-review ()
  "Have the reply to a model review under way, if any, dropped."
  (cl-incf review-guide--findings-generation)
  (when (equal review-guide--busy "reviewing")
    (setq review-guide--busy nil)))

(defun review-guide--action-key (finding)
  "Return FINDING's key in `review-guide--record-actions', or nil.
Only a finding of the guide's review record has one."
  (when-let* ((record review-guide--record)
              (id (review-guide-finding-id finding)))
    (when (memq finding (review-guide-record-findings record))
      (cons (review-guide-record-file record) id))))

(defun review-guide--note-action (key state)
  "Remember that the operator gave the record finding KEY the STATE."
  (when key
    (setf (alist-get key review-guide--record-actions nil nil #'equal) state)))

(defun review-guide-show-record (record)
  "Propose the findings of the review RECORD in place of the guide's.
Those with no lines in the guide's diff are listed with the record.
A finding the operator forwarded, dismissed or restored in this guide
keeps that state; the others take the state of their outcome in
RECORD.  A model review still under way is dropped."
  (dolist (finding (review-guide-record-findings record))
    (when-let* ((id (review-guide-finding-id finding))
                (state (alist-get (cons (review-guide-record-file record) id)
                                  review-guide--record-actions nil nil #'equal)))
      (setf (review-guide-finding-state finding) state)))
  (review-guide--drop-pending-review)
  (setq review-guide--record record
        review-guide--review-summary nil
        review-guide--findings (review-guide-record-anchor record review-guide--hunks))
  (review-guide--render)
  (let ((outside (- (length (review-guide-record-findings record))
                    (length review-guide--findings))))
    (when (> outside 0)
      (message "%d of the record's findings have no lines in the diff; they are listed with it"
               outside))))

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
They take the place of a review record's findings, and stay local
until forwarded with `review-guide-forward'.  The reply is dropped if
the diff changes or a review record is opened before it comes."
  (interactive)
  (pcase-let ((`(,_ ,title ,description) (review-guide-source-describe review-guide--source))
              (buffer (current-buffer))
              (generation (cl-incf review-guide--findings-generation)))
    (setq review-guide--busy "reviewing")
    (review-guide--render)
    (review-guide-ai-review
     title description review-guide--hunks buffer
     (lambda (summary findings dropped)
       (when (eql generation review-guide--findings-generation)
         (setq review-guide--review-summary summary
               review-guide--findings findings
               review-guide--record nil
               review-guide--busy nil)
         (review-guide--render)
         (when (> dropped 0)
           (message "Dropped %d model comments that cite lines outside the diff"
                    dropped))))
     (let ((failed (review-guide--failed "reviewing")))
       (lambda (reason)
         (when (eql generation review-guide--findings-generation)
           (funcall failed reason)))))))

(defun review-guide-visit ()
  "Show the diff line at point where the guide's source keeps it."
  (interactive)
  (pcase (review-guide--line-at (point))
    (`(,path ,side ,line ,commit)
     (review-guide-source-visit review-guide--source path side line commit))
    (_ (user-error "Not on a diff line"))))

(defun review-guide-comment (beg end)
  "Add a comment on the diff line at point to the pending review.
With an active region from BEG to END, comment on the range of lines
it covers, which must lie in one file, and in one commit when the
diff is a series of them."
  (interactive (if (use-region-p)
                   (list (region-beginning) (1- (region-end)))
                 (list (point) (point))))
  (pcase-let ((`(,path ,side ,line ,commit) (review-guide--line-at end))
              (`(,start-path ,start-side ,start-line ,start-commit)
               (review-guide--line-at beg))
              (buffer (current-buffer)))
    (unless (and path start-path)
      (user-error "The comment must start and end on diff lines"))
    (unless (equal path start-path)
      (user-error "A comment on a range of lines must stay in one file"))
    (unless (equal commit start-commit)
      (user-error "A comment on a range of lines must stay in one commit"))
    (deactivate-mark)
    (review-guide-source-add-thread
     review-guide--source
     (append (unless (and (equal side start-side) (eql line start-line))
               `((startLine . ,start-line) (startSide . ,start-side)))
             `((path . ,path) (line . ,line) (side . ,side))
             (when commit `((commit . ,commit))))
     nil
     (lambda () (review-guide--rerender buffer)))))

(defun review-guide-forward ()
  "Edit the proposed comment at point, then add it to the pending review.
The comment is the model's, or a finding of the guide's review
record; a record's finding with no lines in the diff cannot be one."
  (interactive)
  (when (review-guide--section-value 'review-guide-record-finding)
    (user-error "This finding has no lines in the diff to comment on; quote it in the review's own comment"))
  (let* ((finding (or (review-guide--section-value 'review-guide-finding)
                      (user-error "Not on a proposed comment")))
         (key (review-guide--action-key finding))
         (buffer (current-buffer)))
    (when (or (eq (review-guide-finding-state finding) 'forwarded)
              (and key (eq (alist-get key review-guide--record-actions nil nil #'equal)
                           'forwarded)))
      (user-error "Already forwarded"))
    (review-guide-source-add-thread
     review-guide--source
     (review-guide-finding-thread finding)
     (review-guide-finding-body finding)
     (lambda ()
       (setf (review-guide-finding-state finding) 'forwarded)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (review-guide--note-action key 'forwarded)
           ;; The record may have been opened again while the comment
           ;; was edited: its finding with the same key is this one.
           (when-let* ((record review-guide--record)
                       ((equal (car key) (review-guide-record-file record)))
                       (current (cl-find (cdr key) (review-guide-record-findings record)
                                         :key #'review-guide-finding-id :test #'equal)))
             (setf (review-guide-finding-state current) 'forwarded))))
       (review-guide--rerender buffer)))))

(defun review-guide-dismiss ()
  "Dismiss the model's comment at point, or restore a dismissed one.
This works on a review record's findings outside the diff too.  On a
pending comment, delete it."
  (interactive)
  (if-let* ((thread (review-guide--section-value 'review-guide-thread)))
      (when (y-or-n-p "Delete this pending comment? ")
        (review-guide-source-delete-thread review-guide--source thread))
    (let ((finding (or (review-guide--section-value 'review-guide-finding)
                       (review-guide--section-value 'review-guide-record-finding)
                       (user-error "Not on a comment"))))
      (setf (review-guide-finding-state finding)
            (pcase (review-guide-finding-state finding)
              ('proposed 'dismissed)
              ('dismissed 'proposed)
              (_ (user-error "Already forwarded"))))
      (review-guide--note-action (review-guide--action-key finding)
                                 (review-guide-finding-state finding))))
  (review-guide--render))

(defun review-guide-submit ()
  "Send the pending comments on to wherever the guide's source sends them.
For a pull request, that is a review submitted from `pr-review'."
  (interactive)
  (let ((buffer (current-buffer)))
    (review-guide-source-submit review-guide--source
                                (lambda () (review-guide--rerender buffer)))))

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

;;;###autoload
(defun review-guide-open (source)
  "Show the review guide for SOURCE, creating it if needed.
A new guide asks the model to arrange the diff into chunks straight
away, unless SOURCE supplies its own; see `review-guide-source-chunks'.
A guide is identified by its source's name, so opening the same
change again shows the existing guide, which keeps its own source and
the pending comments in it, unless that source is a killed buffer."
  (let* ((name (car (review-guide-source-describe source)))
         (buffer (get-buffer-create (format "*review-guide %s*" name)))
         (new (not (buffer-local-value 'review-guide--source buffer))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'review-guide-mode)
        (review-guide-mode))
      (when (or new (and (bufferp review-guide--source)
                         (not (buffer-live-p review-guide--source))))
        (setq review-guide--source source))
      (review-guide-refresh)
      (when (and new (not review-guide--chunks))
        (review-guide-arrange)))
    (pop-to-buffer buffer)))

(provide 'review-guide)
;;; review-guide.el ends here
