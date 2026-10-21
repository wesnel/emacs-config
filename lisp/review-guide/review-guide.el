;;; review-guide.el --- Read the review records git delegate writes -*- lexical-binding: t; -*-

;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (magit-section "4.0") (pr-review "0.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A read-only view of one review, from the Org files `git delegate'
;; and `pr-loop' write under `review-guide-records-directory':
;;
;;   <owner>/<repo>/<BASE>..<HEAD>.org   one reviewed range: its
;;                                       walkthrough, diagrams, findings
;;                                       and rounds
;;   <owner>/<repo>/pr-<N>.org           one pull request's threads and
;;                                       drafted replies
;;
;; The view shows the walkthrough's chunks in order, each with its hunks
;; from `git diff BASE HEAD', the findings at the lines they are on, the
;; diagrams, and a pull request's threads and drafts.  It writes nothing
;; and runs no model.  Comments are made in `pr-review', and drafts are
;; posted with `pr-loop post' from a terminal, which asks the operator
;; to confirm each one.
;;
;; A record's free text comes from models, GitHub and test runs.  It is
;; read without visiting the file, so no file variables apply, and
;; inserted as plain text, never as a format string or markup.
;;
;; Entry point: `review-guide'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'magit-section)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'review-guide-diff)

(declare-function eieio-oref "eieio-core" (obj slot))
(declare-function forge-get-repository "forge-core")
(declare-function forge-get-pullreq "forge-pullreq")
(declare-function pr-review "pr-review" (url &optional new-window))
(declare-function term-char-mode "term" ())
(defvar pr-review--pr-info)

(defgroup review-guide nil
  "Read the review records git delegate writes."
  :group 'tools)

(defcustom review-guide-records-directory nil
  "The directory `git delegate' and `pr-loop' keep review records in.
Nil means agent-tools/reviews under $XDG_STATE_HOME, else under
~/.local/state, which is where they compute it."
  :type '(choice (const :tag "Under $XDG_STATE_HOME" nil) directory))

(defcustom review-guide-cache-directory nil
  "The directory `git delegate' keeps pull requests' clones in.
A pull request's bare clone is <owner>/<repo>/<N>/clone under it.  Nil
means pr-review under $XDG_CACHE_HOME, else under ~/.cache."
  :type '(choice (const :tag "Under $XDG_CACHE_HOME" nil) directory))

(defcustom review-guide-mermaid-program "mmdc"
  "The program that renders a diagram's Mermaid to SVG, or nil for none.
It is run as PROGRAM --quiet --input IN --output OUT.  Without it, or
on a display without SVG images, a diagram is shown as its source."
  :type '(choice (const :tag "Show the source" nil) string))

;;;; Where records are

(defun review-guide--xdg (variable fallback)
  "Return the absolute directory in environment VARIABLE, else FALLBACK.
The XDG specification ignores a relative one."
  (let ((value (getenv variable)))
    (expand-file-name (if (and value (file-name-absolute-p value)) value fallback))))

(defun review-guide-records-directory ()
  "Return the directory of review records."
  (file-name-as-directory
   (or review-guide-records-directory
       (expand-file-name "agent-tools/reviews"
                         (review-guide--xdg "XDG_STATE_HOME" "~/.local/state")))))

(defun review-guide--clone-directory (owner repo number)
  "Return the bare clone of OWNER/REPO's pull request NUMBER, or nil if none."
  (let ((clone (expand-file-name
                (format "%s/%s/%d/clone" owner repo number)
                (or review-guide-cache-directory
                    (expand-file-name "pr-review"
                                      (review-guide--xdg "XDG_CACHE_HOME" "~/.cache"))))))
    (and (file-directory-p clone) (file-name-as-directory clone))))

(defconst review-guide--name-regexp "\\`[A-Za-z0-9_][A-Za-z0-9_.-]*\\'"
  "Match a GitHub owner or repository name.
A path made of them stays inside the records directory.")

(defconst review-guide--sha-regexp "\\(?:[0-9a-f]\\{40\\}\\(?:[0-9a-f]\\{24\\}\\)?\\)"
  "Match a full SHA-1 or SHA-256 object name.")

(defun review-guide--repo-directory (owner repo)
  "Return OWNER/REPO's directory of records, refusing names that leave it."
  (unless (and (string-match-p review-guide--name-regexp owner)
               (string-match-p review-guide--name-regexp repo))
    (user-error "%s/%s is not a GitHub owner and repository" owner repo))
  (file-name-as-directory
   (expand-file-name (concat owner "/" repo) (review-guide-records-directory))))

;;;; Reading a record
;;
;; The format is version 2 of agent-tools' internal/record, which the
;; files under tests/fixtures are copies of.  Nothing derived is stored:
;; the counts are computed here.

(cl-defstruct (review-guide-range (:constructor review-guide-range--create)
                                  (:copier nil))
  "A reviewed range's record."
  file repo base head pr title updated chunks diagrams findings rounds)

(cl-defstruct (review-guide-chunk (:constructor review-guide-chunk--create)
                                  (:copier nil))
  "A part of the walkthrough.
HUNKS are plists of :path, :new-start, :new-lines, :old-start and
:old-lines, as `@@ -old +new @@' numbers them."
  id title summary hunks)

(cl-defstruct (review-guide-diagram (:constructor review-guide-diagram--create)
                                    (:copier nil))
  "A diagram, by its JSON: Mermaid is made from it when read."
  id title drawn-by critiqued-by verdict json)

(cl-defstruct (review-guide-finding (:constructor review-guide-finding--create)
                                    (:copier nil))
  "A reviewer's finding.
LINE and END-LINE are on SIDE's tree, \"new\" or \"old\".  REASON is the
outcome's, nil while the finding is open."
  id state title reviewer round kind path line end-line side outcome-target
  evidence reason)

(cl-defstruct (review-guide-round (:constructor review-guide-round--create)
                                  (:copier nil))
  "One reviewer's run over the range."
  id reviewer model at red-check unverified dropped)

(cl-defstruct (review-guide-pr (:constructor review-guide-pr--create)
                               (:copier nil))
  "A pull request's record of threads."
  file repo number updated threads)

(cl-defstruct (review-guide-thread (:constructor review-guide-thread--create)
                                   (:copier nil))
  "A review thread.  AUTHORS are \"login:Type\", one per comment.
DRAFT is nil or a plist of :outcome, :posted and :body."
  id authors resolved path line comments draft)

(defconst review-guide--header-lines
  '("#+title: review"
    "#+TODO: OPEN DEFER OPERATOR | FIX PROPOSE REBUT DUPLICATE BELONGS")
  "The first two lines of every record.")

(defconst review-guide--block-begin-regexp "\\`[ \t]*#\\+begin_example[ \t]*\\'"
  "Match the line that starts an example block.")

(defconst review-guide--block-end-regexp "\\`[ \t]*#\\+end_example[ \t]*\\'"
  "Match the line that ends an example block.")

(defun review-guide--check-lines (file)
  "Refuse the current buffer, FILE's text, where it is not a record's.
Outside example blocks, a line starting with #+ is one of the two
header lines or a block's start: Org acts on other keywords, such as
#+SETUPFILE, as soon as the buffer is in `org-mode'."
  (goto-char (point-min))
  (let ((in-block nil)
        (number 1))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (pos-bol) (pos-eol))))
        (cond
         (in-block
          (when (string-match-p review-guide--block-end-regexp line)
            (setq in-block nil)))
         ((string-match-p review-guide--block-begin-regexp line)
          (setq in-block t))
         ((and (string-match-p "\\`[ \t]*#\\+" line)
               (not (and (<= number 2)
                         (equal line (nth (1- number) review-guide--header-lines)))))
          (user-error "%s:%d is not a line of a review record; git delegate writes it again once the file is deleted"
                      (abbreviate-file-name file) number))))
      (forward-line 1)
      (cl-incf number))
    (when in-block
      (user-error "%s ends inside an example block" (abbreviate-file-name file)))))

(defun review-guide--parse-here (file)
  "Read the record FILE into the current buffer, and return its Org tree.
The buffer is to be a temporary one, which the tree's positions point
into.  The file is read rather than visited, so no file variables,
find-file hooks or mode hooks apply."
  (let ((enable-local-variables nil)
        (enable-dir-local-variables nil))
    (insert-file-contents file)
    (review-guide--check-lines file)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))
    (org-element-parse-buffer)))

(defun review-guide--value (node property)
  "Return NODE's PROPERTY, a keyword such as :TITLE, or nil when empty."
  (let ((value (org-element-property property node)))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun review-guide--number (node property)
  "Return NODE's PROPERTY as a number, or nil when it holds none."
  (let ((value (review-guide--value node property)))
    (and value (string-match-p "\\`[0-9]+\\'" value) (string-to-number value))))

(defun review-guide--headings (node)
  "Return NODE's child headings, or the top headings when NODE is the tree."
  (org-element-map (org-element-contents node) 'headline #'identity nil nil 'headline))

(defun review-guide--heading (node title)
  "Return NODE's child heading titled TITLE, or nil."
  (seq-find (lambda (heading) (equal (org-element-property :raw-value heading) title))
            (review-guide--headings node)))

(defun review-guide--blocks (node)
  "Return the text of each example block in NODE's own section.
Org has taken away the commas that escape lines inside them."
  (org-element-map (org-element-contents node) 'example-block
    (lambda (block) (string-remove-suffix "\n" (org-element-property :value block)))
    nil nil 'headline))

(defun review-guide--section-text (node)
  "Return the text of NODE's own section, before its first child heading."
  (let ((section (org-element-map (org-element-contents node) 'section
                   #'identity nil t 'headline)))
    (if section
        (buffer-substring-no-properties (org-element-property :contents-begin section)
                                        (org-element-property :contents-end section))
      "")))

;; A path no Org link can hold is written as its path text: each byte
;; outside [A-Za-z0-9_.@+/-] as %XX.  These follow internal/record's
;; validPath, pathText and readPath, so a record reads back here as it
;; does there.

(defun review-guide--linkable-path-p (path)
  "Return non-nil when the record's writer names PATH in a link, as it is."
  (let ((case-fold-search nil))
    (and (string-match-p "\\`[A-Za-z0-9_.@+-][A-Za-z0-9_.@+/ -]*\\'" path)
         (not (string-suffix-p " " path))
         (not (string-suffix-p "/" path))
         (not (seq-some (lambda (part) (member part '("" "." "..")))
                        (split-string path "/"))))))

(defun review-guide--path-text (path)
  "Return PATH as the record's writer puts it in a property or a hunk's line."
  (if (review-guide--linkable-path-p path)
      path
    (mapconcat (lambda (byte)
                 (if (or (<= ?A byte ?Z) (<= ?a byte ?z) (<= ?0 byte ?9)
                         (memq byte '(?_ ?. ?@ ?+ ?/ ?-)))
                     (string byte)
                   (format "%%%02X" byte)))
               (encode-coding-string path 'utf-8-unix)
               "")))

(defun review-guide--read-path (text file)
  "Return the path TEXT, from the record FILE, names.
TEXT without a %XX is the path itself.  Refuse any other TEXT that
`review-guide--path-text' does not give for the path it decodes to."
  (if (not (string-search "%" text))
      text
    (let* ((case-fold-search nil)
           (path (and (string-match-p "\\`\\(?:[A-Za-z0-9_.@+/-]\\|%[0-9A-F]\\{2\\}\\)*\\'" text)
                      (decode-coding-string
                       (apply #'unibyte-string
                              (let ((at 0) bytes)
                                (while (< at (length text))
                                  (if (eq (aref text at) ?%)
                                      (progn (push (string-to-number
                                                    (substring text (1+ at) (+ at 3)) 16)
                                                   bytes)
                                             (cl-incf at 3))
                                    (push (aref text at) bytes)
                                    (cl-incf at)))
                                (nreverse bytes)))
                       'utf-8-unix))))
      (unless (and path (equal (review-guide--path-text path) text))
        (user-error "%s: %s is not a path as the record's writer puts it"
                    (abbreviate-file-name file) text))
      path)))

(defun review-guide--path (node property file)
  "Return the path in NODE's PROPERTY, from the record FILE, or nil when empty."
  (let ((text (review-guide--value node property)))
    (and text (review-guide--read-path text file))))

(defun review-guide--shown-path (path)
  "Return PATH as the view shows it.
A control or bidirectional character in it is shown as %XX, as the
record writes it, so the path cannot read as other than it is.  The
record's writer removes them from free text, but keeps them in paths."
  (replace-regexp-in-string
   "[\u0000-\u001f\u007f-\u009f\u061c\u200e\u200f\u202a-\u202e\u2066-\u2069]"
   (lambda (char)
     (mapconcat (lambda (byte) (format "%%%02X" byte))
                (encode-coding-string char 'utf-8-unix) ""))
   path t t))

(defconst review-guide--hunk-regexp
  (concat "\\`- \\(?:\\[\\[file:\\([^]\n]+\\)::[0-9]+@[0-9a-f]\\{12\\}\\]\\[[^]\n]*\\]\\]"
          "\\|=\\([^ =\n]*\\)=\\)"
          " new \\([0-9]+\\),\\([0-9]+\\) old \\([0-9]+\\),\\([0-9]+\\)[ \t]*\\'")
  "Match a walkthrough chunk's hunk line.
Its path is in a link, or as its path text in verbatim markup.")

(defun review-guide--chunk-hunks (text file)
  "Return the hunks a walkthrough chunk's section TEXT, from FILE, lists.
A line inside an example block is free text, whatever it looks like."
  (let (in-block hunks)
    (dolist (line (split-string text "\n"))
      (cond
       (in-block
        (when (string-match-p review-guide--block-end-regexp line)
          (setq in-block nil)))
       ((string-match-p review-guide--block-begin-regexp line)
        (setq in-block t))
       ((string-match review-guide--hunk-regexp line)
        (push (list :path (or (match-string 1 line)
                              (review-guide--read-path (match-string 2 line) file))
                    :new-start (string-to-number (match-string 3 line))
                    :new-lines (string-to-number (match-string 4 line))
                    :old-start (string-to-number (match-string 5 line))
                    :old-lines (string-to-number (match-string 6 line)))
              hunks))))
    (nreverse hunks)))

(defun review-guide--top (tree file title)
  "Return the top heading TITLE of TREE, read from FILE, if of version 2."
  (let ((top (car (review-guide--headings tree))))
    (unless (and top (equal (org-element-property :raw-value top) title))
      (user-error "%s holds no \"* %s\" heading" (abbreviate-file-name file) title))
    (unless (equal (review-guide--value top :VERSION) "2")
      (user-error "%s is not a version 2 review record" (abbreviate-file-name file)))
    top))

(defun review-guide-read-range (file)
  "Return the range record in FILE as a `review-guide-range'."
  (with-temp-buffer
    (let* ((tree (review-guide--parse-here file))
           (top (review-guide--top tree file "Review"))
           (finding
            (lambda (heading)
              (let ((outcome (review-guide--heading heading "Outcome")))
                (review-guide-finding--create
                 :id (org-element-property :raw-value heading)
                 :state (org-element-property :todo-keyword heading)
                 :title (review-guide--value heading :TITLE)
                 :reviewer (review-guide--value heading :REVIEWER)
                 :round (review-guide--number heading :ROUND)
                 :kind (review-guide--value heading :KIND)
                 :path (review-guide--path heading :PATH file)
                 :line (review-guide--number heading :LINE)
                 :end-line (review-guide--number heading :END_LINE)
                 :side (review-guide--value heading :SIDE)
                 :outcome-target (review-guide--value heading :OUTCOME_TARGET)
                 :evidence (string-join (review-guide--blocks heading) "\n\n")
                 :reason (and outcome (string-join (review-guide--blocks outcome) "\n\n")))))))
      (review-guide-range--create
       :file file
       :repo (review-guide--value top :REPO)
       :base (review-guide--value top :BASE)
       :head (review-guide--value top :HEAD)
       :pr (review-guide--number top :PR)
       :title (review-guide--value top :TITLE)
       :updated (review-guide--value top :UPDATED)
       :chunks (mapcar (lambda (heading)
                         (review-guide-chunk--create
                          :id (org-element-property :raw-value heading)
                          :title (review-guide--value heading :TITLE)
                          :summary (string-join (review-guide--blocks heading) "\n\n")
                          :hunks (review-guide--chunk-hunks
                                  (review-guide--section-text heading) file)))
                       (review-guide--headings (review-guide--heading top "Walkthrough")))
       :diagrams (mapcar (lambda (heading)
                           (review-guide-diagram--create
                            :id (org-element-property :raw-value heading)
                            :title (review-guide--value heading :TITLE)
                            :drawn-by (review-guide--value heading :DRAWN_BY)
                            :critiqued-by (review-guide--value heading :CRITIQUED_BY)
                            :verdict (review-guide--value heading :VERDICT)
                            :json (car (review-guide--blocks heading))))
                         (review-guide--headings (review-guide--heading top "Diagrams")))
       :findings (mapcar finding
                         (review-guide--headings (review-guide--heading top "Findings")))
       :rounds (mapcar (lambda (heading)
                         (review-guide-round--create
                          :id (org-element-property :raw-value heading)
                          :reviewer (review-guide--value heading :REVIEWER)
                          :model (review-guide--value heading :MODEL)
                          :at (review-guide--value heading :AT)
                          :red-check (review-guide--value heading :RED_CHECK)
                          :unverified (let ((h (review-guide--heading heading "Unverified")))
                                        (and h (review-guide--blocks h)))
                          :dropped (let ((h (review-guide--heading heading "Dropped")))
                                     (and h (review-guide--blocks h)))))
                       (review-guide--headings (review-guide--heading top "Rounds")))))))

(defun review-guide-read-pr (file)
  "Return the pull request record in FILE as a `review-guide-pr'."
  (with-temp-buffer
    (let* ((tree (review-guide--parse-here file))
           (top (review-guide--top tree file "Pull request")))
      (review-guide-pr--create
       :file file
       :repo (review-guide--value top :REPO)
       :number (review-guide--number top :PR)
       :updated (review-guide--value top :UPDATED)
       :threads
       (mapcar (lambda (heading)
                 (let ((draft (review-guide--heading heading "Draft")))
                   (review-guide-thread--create
                    :id (string-remove-prefix "T-" (org-element-property :raw-value heading))
                    :authors (split-string (or (review-guide--value heading :AUTHORS) ""))
                    :resolved (equal (review-guide--value heading :RESOLVED) "t")
                    :path (review-guide--path heading :PATH file)
                    :line (review-guide--number heading :LINE)
                    :comments (review-guide--blocks heading)
                    :draft (and draft
                                (list :outcome (review-guide--value draft :OUTCOME)
                                      :posted (review-guide--value draft :POSTED)
                                      :body (string-join (review-guide--blocks draft) "\n\n"))))))
               (review-guide--headings top))))))

(defun review-guide-counts (range pr)
  "Return the counts of RANGE's findings and PR's threads, as a plist.
:open, :deferred and :operator count findings with those keywords;
:unanswered counts unresolved threads with no draft, and :drafted
drafts not yet posted.  Either record may be nil."
  (let ((states (and range (mapcar #'review-guide-finding-state
                                   (review-guide-range-findings range))))
        (threads (and pr (review-guide-pr-threads pr))))
    (list :open (cl-count "OPEN" states :test #'equal)
          :deferred (cl-count "DEFER" states :test #'equal)
          :operator (cl-count "OPERATOR" states :test #'equal)
          :unanswered (cl-count-if (lambda (thread)
                                     (and (null (review-guide-thread-draft thread))
                                          (not (review-guide-thread-resolved thread))))
                                   threads)
          :drafted (cl-count-if (lambda (thread)
                                  (let ((draft (review-guide-thread-draft thread)))
                                    (and draft (null (plist-get draft :posted)))))
                                threads))))

;;;; Finding the record

(defun review-guide--git (directory &rest args)
  "Return the output of git ARGS in DIRECTORY, trimmed, or nil on failure."
  (let ((default-directory directory))
    (with-temp-buffer
      (when (eq 0 (apply #'process-file "git" nil (list t nil) nil args))
        (string-trim (buffer-string))))))

(defun review-guide--github-name (directory)
  "Return (OWNER REPO) of DIRECTORY's origin on GitHub, or nil."
  (let ((url (review-guide--git directory "remote" "get-url" "origin")))
    (and url
         (string-match "github\\.com[:/]\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?/?\\'" url)
         (list (match-string 1 url) (match-string 2 url)))))

(defun review-guide--range-files (directory)
  "Return the range records in DIRECTORY as (FILE BASE HEAD)."
  (let ((pattern (concat "\\`\\(" review-guide--sha-regexp "\\)\\.\\.\\("
                         review-guide--sha-regexp "\\)\\.org\\'")))
    (delq nil (mapcar (lambda (name)
                        (and (string-match pattern name)
                             (list (expand-file-name name directory)
                                   (match-string 1 name) (match-string 2 name))))
                      (and (file-directory-p directory)
                           (directory-files directory nil "\\.org\\'"))))))

(defun review-guide--header-value (file property)
  "Return PROPERTY of the record FILE's top heading, read from its first lines.
It is taken only from the property drawer straight after the heading
on line 3, where the record's writer puts it: a line like it further
on, in an example block, is free text."
  (with-temp-buffer
    (let ((enable-local-variables nil))
      (insert-file-contents file nil 0 4096))
    (goto-char (point-min))
    (forward-line 2)
    (when (looking-at-p "\\* [^\n]*\n:PROPERTIES:$")
      (forward-line 2)
      (catch 'value
        (while (looking-at "^:\\([A-Z_]+\\):[ \t]*\\(.*?\\)[ \t]*$")
          (pcase (match-string-no-properties 1)
            ("END" (throw 'value nil))
            ((pred (equal property)) (throw 'value (match-string-no-properties 2))))
          (forward-line 1))))))

(defun review-guide--newest (files)
  "Return the record among FILES updated last, by its UPDATED timestamp."
  (car (sort (copy-sequence files)
             (lambda (a b)
               (string> (or (review-guide--header-value a "UPDATED") "")
                        (or (review-guide--header-value b "UPDATED") ""))))))

(defun review-guide--forge-head (owner repo number)
  "Return the head forge has for OWNER/REPO's pull request NUMBER, or nil."
  (ignore-errors
    (when (require 'forge nil t)
      (let* ((repository (forge-get-repository (list "github.com" owner repo) nil :tracked?))
             (pullreq (and repository (forge-get-pullreq repository number))))
        ;; forge is loaded at run time only, so its slots are unknown here.
        (and pullreq (with-no-warnings (eieio-oref pullreq 'head-rev)))))))

(defun review-guide--checkout (owner repo)
  "Return the checkout of OWNER/REPO that `default-directory' is in, or nil."
  (let ((top (review-guide--git default-directory "rev-parse" "--show-toplevel")))
    (and top
         (equal (review-guide--github-name top) (list owner repo))
         (file-name-as-directory top))))

(defun review-guide--resolve-pr (owner repo number)
  "Return the source plist of OWNER/REPO's pull request NUMBER."
  (let* ((directory (review-guide--repo-directory owner repo))
         (pr-file (expand-file-name (format "pr-%d.org" number) directory))
         (ranges (seq-filter (lambda (file)
                               (equal (review-guide--header-value file "PR")
                                      (number-to-string number)))
                             (mapcar #'car (review-guide--range-files directory))))
         (head (review-guide--forge-head owner repo number))
         (at-head (and head (seq-filter (lambda (file)
                                          (string-suffix-p (concat ".." head ".org") file))
                                        ranges)))
         (range (review-guide--newest (or at-head ranges))))
    (unless (or range (file-exists-p pr-file))
      (user-error "No review record of %s/%s#%d under %s; git delegate %s/%s#%d makes one"
                  owner repo number (abbreviate-file-name directory) owner repo number))
    (when (and head range (not at-head))
      (message "No review of %s/%s#%d at its head %s; showing the newest, of %s"
               owner repo number (substring head 0 12)
               (file-name-base range)))
    (list :owner owner :repo repo :number number
          :range-file range
          :pr-file (and (file-exists-p pr-file) pr-file)
          :directory (review-guide--clone-directory owner repo number)
          :checkout (review-guide--checkout owner repo)
          :show (format "%s/%s#%d" owner repo number))))

(defun review-guide--resolve-revisions (base head)
  "Return the source plist of the range BASE..HEAD.
With BASE nil, HEAD is a branch, and its newest review is taken.
Either is read in the checkout `default-directory' is in."
  (let* ((top (or (review-guide--git default-directory "rev-parse" "--show-toplevel")
                  (user-error "A range or branch is read in its checkout, and %s is in none"
                              (abbreviate-file-name default-directory))))
         (checkout (file-name-as-directory top))
         (name (or (review-guide--github-name checkout)
                   (user-error "%s has no GitHub origin, so no review records"
                               (abbreviate-file-name checkout))))
         (directory (review-guide--repo-directory (car name) (cadr name)))
         (sha (lambda (rev)
                (or (review-guide--git checkout "rev-parse" "--verify" "--quiet"
                                       (concat rev "^{commit}"))
                    (user-error "%s is not a commit here" rev))))
         (head-sha (funcall sha head))
         (base-sha (and base (funcall sha base)))
         (file (if base
                   (expand-file-name (concat base-sha ".." head-sha ".org") directory)
                 (review-guide--newest
                  (mapcar #'car (seq-filter (lambda (range) (equal (nth 2 range) head-sha))
                                            (review-guide--range-files directory)))))))
    (unless (and file (file-exists-p file))
      (user-error "No review of %s under %s; git delegate %s makes one"
                  (if base (concat base ".." head) head)
                  (abbreviate-file-name directory)
                  (if base (concat base ".." head) (concat "BASE.." head))))
    (let* ((pr (review-guide--header-value file "PR"))
           (number (and pr (string-match-p "\\`[1-9][0-9]*\\'" pr) (string-to-number pr)))
           (pr-file (and number (expand-file-name (format "pr-%d.org" number) directory))))
      (list :owner (car name) :repo (cadr name) :number number
            :range-file file
            :pr-file (and pr-file (file-exists-p pr-file) pr-file)
            :directory checkout
            :checkout checkout
            :show (file-name-base file)))))

(defun review-guide--parse-target (target)
  "Return what TARGET names.
That is (pr OWNER REPO N), (range BASE HEAD) or (branch REV)."
  (let ((target (string-trim target)))
    (cond
     ((string-match "\\`https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)/pull/\\([0-9]+\\)\\(?:[/?#].*\\)?\\'"
                    target)
      (list 'pr (match-string 1 target) (match-string 2 target)
            (string-to-number (match-string 3 target))))
     ((string-match "\\`\\([^/#[:space:]]+\\)/\\([^/#[:space:]]+\\)#\\([0-9]+\\)\\'" target)
      (list 'pr (match-string 1 target) (match-string 2 target)
            (string-to-number (match-string 3 target))))
     ((and (not (string-search "..." target))
           (string-match "\\`\\([^.[:space:]-][^[:space:]]*?\\)\\.\\.\\([^.[:space:]-][^[:space:]]*\\)\\'"
                         target))
      (list 'range (match-string 1 target) (match-string 2 target)))
     ((string-match-p "\\`[^.[:space:]-][^[:space:]]*\\'" target)
      (list 'branch target))
     (t (user-error "Not a pull request, range or branch: %s" target)))))

(defun review-guide--resolve (target)
  "Return the source plist of TARGET, as `review-guide' takes it."
  (pcase (review-guide--parse-target target)
    (`(pr ,owner ,repo ,number) (review-guide--resolve-pr owner repo number))
    (`(range ,base ,head) (review-guide--resolve-revisions base head))
    (`(branch ,rev) (review-guide--resolve-revisions nil rev))))

(defun review-guide--load (source)
  "Read SOURCE's records, and return (RANGE PR), refusing any that name others."
  (let* ((name (concat (plist-get source :owner) "/" (plist-get source :repo)))
         (range (and (plist-get source :range-file)
                     (review-guide-read-range (plist-get source :range-file))))
         (pr (and (plist-get source :pr-file)
                  (review-guide-read-pr (plist-get source :pr-file)))))
    (when range
      (let ((file (review-guide-range-file range)))
        (unless (and (equal (review-guide-range-repo range) name)
                     (equal (concat (review-guide-range-base range) ".."
                                    (review-guide-range-head range))
                            (file-name-base file)))
          (user-error "%s names %s %s..%s, not what its path does"
                      (abbreviate-file-name file) (review-guide-range-repo range)
                      (review-guide-range-base range) (review-guide-range-head range)))))
    (when pr
      (unless (and (equal (review-guide-pr-repo pr) name)
                   (equal (review-guide-pr-number pr) (plist-get source :number)))
        (user-error "%s names %s#%s, not what its path does"
                    (abbreviate-file-name (review-guide-pr-file pr))
                    (review-guide-pr-repo pr) (review-guide-pr-number pr))))
    (list range pr)))

;;;; Git and git delegate

(defun review-guide--diff (directory base head)
  "Return the hunks of `git diff BASE HEAD' in DIRECTORY.
Return a string saying why instead when there are none.  No external
diff or text conversion runs, since a pull request's .gitattributes
can name either."
  (if (not directory)
      "There is no clone of the pull request to diff in"
    (let ((default-directory directory))
      (with-temp-buffer
        (let ((status (process-file "git" nil (list t nil) nil
                                    "-c" "core.quotePath=false" "diff" "--no-color"
                                    "--no-ext-diff" "--no-textconv" base head)))
          (if (eq status 0)
              (review-guide-diff-parse (buffer-string))
            (format "git diff %s %s failed in %s" (substring base 0 12) (substring head 0 12)
                    (abbreviate-file-name directory))))))))

(defun review-guide--mermaid-sources (json)
  "Return an alist from diagram ID to Mermaid, from JSON.
JSON is the parsed output of `git delegate show'.  Every object holding
string \"id\" and \"mermaid\" members gives one."
  (let (found)
    (cl-labels ((walk (value)
                  (cond
                   ((and (consp value) (consp (car value)) (symbolp (caar value)))
                    (let ((id (alist-get 'id value))
                          (mermaid (alist-get 'mermaid value)))
                      (when (and (stringp id) (stringp mermaid))
                        (push (cons id mermaid) found)))
                    (dolist (member value) (walk (cdr member))))
                   ((listp value) (mapc #'walk value)))))
      (walk json))
    (nreverse found)))

(defun review-guide--delegate-show (source)
  "Return `git delegate show' of SOURCE's review as parsed JSON, or nil.
It is run only where git-delegate is installed."
  (when (executable-find "git-delegate")
    (let ((default-directory (or (plist-get source :checkout) (expand-file-name "~/"))))
      (with-temp-buffer
        (when (eq 0 (process-file "git" nil (list t nil) nil
                                  "delegate" "show" (plist-get source :show)))
          (ignore-errors
            (json-parse-string (buffer-string) :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil)))))))

(defun review-guide--render-mermaid (mermaid)
  "Return an SVG image of MERMAID, or nil where it cannot be rendered here."
  (when (and review-guide-mermaid-program
             (display-images-p)
             (image-type-available-p 'svg)
             (executable-find review-guide-mermaid-program))
    (let ((input (make-temp-file "review-guide-" nil ".mmd" mermaid))
          (output (make-temp-file "review-guide-" nil ".svg")))
      (unwind-protect
          (and (eq 0 (call-process review-guide-mermaid-program nil nil nil
                                   "--quiet" "--input" input "--output" output))
               (> (file-attribute-size (file-attributes output)) 0)
               (create-image (with-temp-buffer
                               (set-buffer-multibyte nil)
                               (insert-file-contents-literally output)
                               (buffer-string))
                             'svg t))
        (delete-file input)
        (delete-file output)))))

;;;; The view

(defface review-guide-summary '((t :inherit font-lock-doc-face))
  "Face for a chunk's summary.")

(defface review-guide-finding '((t :inherit font-lock-warning-face))
  "Face for the heading of a finding still open, deferred or for the operator.")

(defface review-guide-finding-settled '((t :inherit shadow))
  "Face for the heading of a settled finding.")

(defface review-guide-thread '((t :inherit success))
  "Face for the heading of a review thread.")

(defvar-local review-guide--source nil
  "Where this view's records and diff are: see `review-guide--resolve'.")
(defvar-local review-guide--range nil)
(defvar-local review-guide--pr nil)

(defconst review-guide--open-states '("OPEN" "DEFER" "OPERATOR")
  "The keywords of a finding not yet settled, before the record's #+TODO bar.")

(defun review-guide--insert-text (text face &optional indent)
  "Insert TEXT, as it is, in FACE, indented by INDENT columns."
  (let ((prefix (make-string (or indent 0) ?\s))
        (start (point)))
    (dolist (line (split-string (or text "") "\n"))
      (insert prefix line "\n"))
    (add-text-properties start (point) `(wrap-prefix ,prefix))
    (add-face-text-property start (point) face)))

(defun review-guide--side (finding)
  "Return the diff side FINDING's lines are on, \"LEFT\" or \"RIGHT\", or nil."
  (pcase (review-guide-finding-side finding)
    ("new" "RIGHT")
    ("old" "LEFT")))

(defun review-guide-anchor (findings hunks)
  "Return an alist from each of FINDINGS anchored in HUNKS to (HUNK-ID SIDE LINE).
A finding is anchored to the first hunk of its path that holds every
one of its lines on its side, at its last line; one with no such hunk,
or no path or line, is not in the alist."
  (cl-loop
   for finding in findings
   for path = (review-guide-finding-path finding)
   for side = (review-guide--side finding)
   for start = (review-guide-finding-line finding)
   for end = (or (review-guide-finding-end-line finding) start)
   for hunk = (and path side start
                   (cl-find-if (lambda (hunk)
                                 (and (equal (review-guide-hunk-path hunk) path)
                                      (cl-loop for line from start to end
                                               always (review-guide-hunk-covers-p
                                                       hunk side line))))
                               hunks))
   when hunk collect (list finding (review-guide-hunk-id hunk) side end)))

(defun review-guide--lines (finding)
  "Return where FINDING is, as text, or nil for a finding with no path."
  (let ((path (review-guide-finding-path finding))
        (line (review-guide-finding-line finding))
        (end (review-guide-finding-end-line finding)))
    (and path
         (concat (review-guide--shown-path path)
                 (cond ((and line end (/= line end)) (format ":%d-%d" line end))
                       (line (format ":%d" line))
                       (t ""))
                 (if (review-guide-finding-side finding)
                     (concat " (" (review-guide-finding-side finding) ")")
                   "")))))

(defun review-guide--insert-finding (finding indent &optional where)
  "Insert FINDING, indented by INDENT columns, with WHERE it is when non-nil."
  (magit-insert-section (review-guide-finding finding)
    (let ((state (or (review-guide-finding-state finding) "")))
      (magit-insert-heading
        (propertize (concat (make-string indent ?\s)
                            state " " (review-guide-finding-id finding)
                            (if (review-guide-finding-kind finding)
                                (concat " " (review-guide-finding-kind finding))
                              "")
                            (if where (concat " on " where) "")
                            "  " (or (review-guide-finding-title finding) ""))
                    'face (if (member state review-guide--open-states)
                              'review-guide-finding
                            'review-guide-finding-settled)))
      (review-guide--insert-text
       (concat (or (review-guide-finding-reviewer finding) "unknown reviewer")
               (if (review-guide-finding-round finding)
                   (format ", round %d" (review-guide-finding-round finding))
                 ""))
       'shadow (+ indent 2))
      (review-guide--insert-text (review-guide-finding-evidence finding) 'default (+ indent 2))
      (when (or (review-guide-finding-reason finding)
                (review-guide-finding-outcome-target finding))
        (review-guide--insert-text
         (concat "Outcome"
                 (if (review-guide-finding-outcome-target finding)
                     (concat ", as " (review-guide-finding-outcome-target finding))
                   "")
                 ":")
         'shadow (+ indent 2))
        (when (review-guide-finding-reason finding)
          (review-guide--insert-text (review-guide-finding-reason finding)
                                     'default (+ indent 4)))))))

(defun review-guide--diff-face (text)
  "Return the face for the diff line TEXT."
  (pcase (and (> (length text) 0) (aref text 0))
    (?+ 'magit-diff-added)
    (?- 'magit-diff-removed)
    (_ 'magit-diff-context)))

(defun review-guide--insert-hunk (hunk anchors)
  "Insert HUNK with the findings ANCHORS places on its lines."
  (let ((id (review-guide-hunk-id hunk)))
    (magit-insert-section (review-guide-hunk hunk)
      (magit-insert-heading
        (propertize (concat "   " (review-guide--shown-path (review-guide-hunk-path hunk))
                            "  " (review-guide-hunk-header hunk))
                    'face 'magit-diff-hunk-heading))
      (pcase-dolist (`(,text ,side ,line) (review-guide-hunk-coordinates hunk))
        (insert (propertize (concat text "\n") 'face (review-guide--diff-face text)))
        (pcase-dolist (`(,finding ,hunk-id ,finding-side ,end) anchors)
          (when (and (equal hunk-id id) (equal finding-side side) (eql end line))
            (review-guide--insert-finding finding 4)))))))

(defun review-guide--find-hunk (cited hunks)
  "Return the hunk among HUNKS that the walkthrough's CITED plist names, or nil."
  (cl-find-if (lambda (hunk)
                (and (equal (review-guide-hunk-path hunk) (plist-get cited :path))
                     (eql (review-guide-hunk-new-start hunk) (plist-get cited :new-start))
                     (eql (review-guide-hunk-old-start hunk) (plist-get cited :old-start))))
              hunks))

(defun review-guide--insert-chunk (index title summary entries anchors)
  "Insert chunk INDEX, TITLE, with SUMMARY and ENTRIES.
Each entry is a hunk of the diff, or a walkthrough's plist for a hunk
the diff lacks.  ANCHORS places findings at the lines of the hunks."
  (magit-insert-section (review-guide-chunk index)
    (magit-insert-heading
      (propertize (format "%d. %s" index (or title "")) 'face 'magit-section-heading))
    (when (and summary (not (string-empty-p summary)))
      (review-guide--insert-text summary 'review-guide-summary 3))
    (dolist (entry entries)
      (if (review-guide-hunk-p entry)
          (review-guide--insert-hunk entry anchors)
        (insert (propertize
                 (format "   %s  @@ -%d,%d +%d,%d @@, which the diff here does not hold\n"
                         (review-guide--shown-path (plist-get entry :path))
                         (plist-get entry :old-start) (plist-get entry :old-lines)
                         (plist-get entry :new-start) (plist-get entry :new-lines))
                 'face 'shadow))))
    (insert "\n")))

(defun review-guide--insert-walkthrough (range hunks anchors)
  "Insert RANGE's walkthrough over HUNKS, the diff's.
Hunks no chunk names follow, in diff order.  ANCHORS places findings
at their lines."
  (let ((placed nil)
        (index 0))
    (dolist (chunk (review-guide-range-chunks range))
      (let ((entries (mapcar (lambda (cited)
                               (let ((hunk (review-guide--find-hunk cited hunks)))
                                 (when hunk (push hunk placed))
                                 (or hunk cited)))
                             (review-guide-chunk-hunks chunk))))
        (review-guide--insert-chunk (cl-incf index) (review-guide-chunk-title chunk)
                                    (review-guide-chunk-summary chunk) entries anchors)))
    (let ((rest (cl-remove-if (lambda (hunk) (memq hunk placed)) hunks)))
      (when rest
        (review-guide--insert-chunk (cl-incf index) "Not in the walkthrough" nil
                                    rest anchors)))))

(defun review-guide--insert-diagrams (range sources)
  "Insert RANGE's diagrams, each from its Mermaid in SOURCES where it is there."
  (when (review-guide-range-diagrams range)
    (magit-insert-section (review-guide-diagrams)
      (magit-insert-heading (propertize "Diagrams" 'face 'magit-section-heading))
      (dolist (diagram (review-guide-range-diagrams range))
        (magit-insert-section (review-guide-diagram diagram)
          (magit-insert-heading
            (concat "  " (review-guide-diagram-id diagram) "  "
                    (or (review-guide-diagram-title diagram) "")))
          (review-guide--insert-text
           (format "drawn by %s, critiqued by %s: %s"
                   (or (review-guide-diagram-drawn-by diagram) "?")
                   (or (review-guide-diagram-critiqued-by diagram) "?")
                   (or (review-guide-diagram-verdict diagram) "?"))
           'shadow 4)
          (let* ((mermaid (cdr (assoc (review-guide-diagram-id diagram) sources)))
                 (image (and mermaid (review-guide--render-mermaid mermaid))))
            (cond
             (image (insert "    ")
                    (insert-image image "[diagram]")
                    (insert "\n"))
             (mermaid (review-guide--insert-text mermaid 'default 4))
             (t (review-guide--insert-text
                 "git delegate show gave no Mermaid for it; its JSON:" 'shadow 4)
                (review-guide--insert-text (review-guide-diagram-json diagram)
                                           'default 4)))))
        (insert "\n")))))

(defun review-guide--insert-findings (findings)
  "Insert FINDINGS, those not shown at a line of the diff."
  (when findings
    (magit-insert-section (review-guide-other-findings)
      (magit-insert-heading
        (propertize "Findings outside the diff here" 'face 'magit-section-heading))
      (dolist (finding findings)
        (review-guide--insert-finding finding 2 (review-guide--lines finding)))
      (insert "\n"))))

(defun review-guide--insert-threads (pr)
  "Insert PR's threads and their drafts."
  (magit-insert-section (review-guide-threads)
    (magit-insert-heading
      (propertize (format "Threads of #%d" (review-guide-pr-number pr))
                  'face 'magit-section-heading))
    (dolist (thread (review-guide-pr-threads pr))
      (magit-insert-section (review-guide-thread thread)
        (magit-insert-heading
          (propertize (concat "  " (review-guide-thread-id thread)
                              (if (review-guide-thread-path thread)
                                  (concat "  " (review-guide--shown-path
                                                (review-guide-thread-path thread))
                                          (if (review-guide-thread-line thread)
                                              (format ":%d" (review-guide-thread-line thread))
                                            ""))
                                "")
                              (if (review-guide-thread-resolved thread)
                                  "  resolved"
                                "  unresolved"))
                      'face 'review-guide-thread))
        (let ((authors (review-guide-thread-authors thread))
              (comments (review-guide-thread-comments thread)))
          (cl-loop for comment in comments
                   for index from 0
                   do (review-guide--insert-text
                       (concat (if (= (length authors) (length comments))
                                   (nth index authors)
                                 "comment")
                               ":")
                       'shadow 4)
                   (review-guide--insert-text comment 'default 6)))
        (let ((draft (review-guide-thread-draft thread)))
          (when draft
            (review-guide--insert-text
             (concat "Draft, " (or (plist-get draft :outcome) "no outcome") ", "
                     (if (plist-get draft :posted)
                         (concat "posted " (plist-get draft :posted))
                       "not posted"))
             'shadow 4)
            (review-guide--insert-text (plist-get draft :body) 'default 6)))))
    (insert "\n")))

(defun review-guide--insert-rounds (range)
  "Insert RANGE's rounds."
  (when (review-guide-range-rounds range)
    (magit-insert-section (review-guide-rounds)
      (magit-insert-heading (propertize "Rounds" 'face 'magit-section-heading))
      (dolist (round (review-guide-range-rounds range))
        (magit-insert-section (review-guide-round round)
          (magit-insert-heading
            (concat "  " (review-guide-round-id round) "  "
                    (or (review-guide-round-reviewer round) "?")
                    (if (review-guide-round-model round)
                        (concat " (" (review-guide-round-model round) ")")
                      "")
                    (if (review-guide-round-at round)
                        (concat ", " (review-guide-round-at round))
                      "")
                    ", red-check " (or (review-guide-round-red-check round) "absent")))
          (dolist (part `(("Unverified" . ,(review-guide-round-unverified round))
                          ("Dropped" . ,(review-guide-round-dropped round))))
            (when (cdr part)
              (review-guide--insert-text (concat (car part) ":") 'shadow 4)
              (dolist (text (cdr part))
                (review-guide--insert-text text 'default 6))))))
      (insert "\n"))))

(defun review-guide--counts-line (range pr)
  "Return the line of RANGE's and PR's counts."
  (let ((counts (review-guide-counts range pr)))
    (string-join
     (delq nil
           (list (and range (format "Findings: %d open, %d deferred, %d for the operator"
                                    (plist-get counts :open) (plist-get counts :deferred)
                                    (plist-get counts :operator)))
                 (and pr (format "Threads: %d unanswered, %d drafts unposted"
                                 (plist-get counts :unanswered)
                                 (plist-get counts :drafted)))))
     "; ")))

(defun review-guide--render ()
  "Draw the view from its records, keeping point near where it was."
  (let* ((source review-guide--source)
         (range review-guide--range)
         (pr review-guide--pr)
         (line (line-number-at-pos))
         (inhibit-read-only t)
         (hunks (and range (review-guide--diff (plist-get source :directory)
                                               (review-guide-range-base range)
                                               (review-guide-range-head range))))
         (anchors (and (listp hunks)
                       (review-guide-anchor (review-guide-range-findings range) hunks)))
         (sources (and range (review-guide-range-diagrams range)
                       (review-guide--mermaid-sources (review-guide--delegate-show source)))))
    (erase-buffer)
    (magit-insert-section (review-guide-root)
      (insert (propertize
               (concat (plist-get source :owner) "/" (plist-get source :repo)
                       (if range
                           (concat " " (substring (review-guide-range-base range) 0 12) ".."
                                   (substring (review-guide-range-head range) 0 12))
                         "")
                       (if (plist-get source :number)
                           (format " #%d" (plist-get source :number))
                         "")
                       "  " (or (and range (review-guide-range-title range)) "")
                       "\n")
               'face 'magit-section-heading))
      (insert (review-guide--counts-line range pr) "\n")
      (insert (propertize
               (substitute-command-keys
                (concat "\\<review-guide-mode-map>\\[review-guide-pr-review]: comment in pr-review"
                        "  \\[review-guide-post]: pr-loop post in a terminal"
                        "  \\[review-guide-visit-record]: the record file"
                        "  \\[review-guide-refresh]: read again\n"))
               'face 'shadow))
      (when (stringp hunks)
        (insert (propertize (concat hunks ", so hunks are shown by their lines alone\n")
                            'face 'warning)))
      (insert "\n")
      (when range
        (review-guide--insert-walkthrough range (and (listp hunks) hunks) anchors)
        (review-guide--insert-diagrams range sources)
        (review-guide--insert-findings
         (cl-remove-if (lambda (finding) (assq finding anchors))
                       (review-guide-range-findings range))))
      (when pr
        (review-guide--insert-threads pr))
      (when range
        (review-guide--insert-rounds range)))
    (goto-char (point-min))
    (forward-line (1- line))))

;;;; Commands

(defun review-guide-refresh ()
  "Read the view's records and diff again, and redraw it."
  (interactive)
  (pcase-let ((`(,range ,pr) (review-guide--load review-guide--source)))
    (setq review-guide--range range
          review-guide--pr pr))
  (review-guide--render))

(defun review-guide--pr-number ()
  "Return the view's pull request number, refusing a view without one."
  (or (plist-get review-guide--source :number)
      (user-error "This review is of no pull request")))

(defun review-guide-pr-review ()
  "Open the view's pull request in `pr-review', to comment on it."
  (interactive)
  (let ((number (review-guide--pr-number)))
    (pr-review (format "https://github.com/%s/%s/pull/%d"
                       (plist-get review-guide--source :owner)
                       (plist-get review-guide--source :repo)
                       number))))

(defun review-guide-post ()
  "Open a terminal running `pr-loop post' on the view's pull request.
`pr-loop' shows each draft there and asks before posting it; nothing
is posted from Emacs.  It runs in the operator's checkout of the
repository, which is asked for when the view was not opened in one."
  (interactive)
  (let* ((number (review-guide--pr-number))
         (owner (plist-get review-guide--source :owner))
         (repo (plist-get review-guide--source :repo))
         (checkout (or (plist-get review-guide--source :checkout)
                       (let ((default-directory
                              (read-directory-name (format "Checkout of %s/%s: " owner repo))))
                         (or (review-guide--checkout owner repo)
                             (user-error "That is no checkout of %s/%s" owner repo)))))
         (default-directory checkout)
         (buffer (make-term (format "pr-loop post %s/%s#%d" owner repo number)
                            "pr-loop" nil "post" (number-to-string number))))
    (with-current-buffer buffer
      (term-char-mode))
    (pop-to-buffer buffer)))

(defun review-guide-visit-record (&optional pr-file)
  "Visit the view's range record read-only, or with PR-FILE its thread record.
No file variables apply in it.  Its `default-directory' is the
repository the view diffs in, so its file: links resolve there."
  (interactive "P")
  (let* ((file (if (or pr-file (null (plist-get review-guide--source :range-file)))
                   (or (plist-get review-guide--source :pr-file)
                       (user-error "This review has no thread record"))
                 (plist-get review-guide--source :range-file)))
         (directory (plist-get review-guide--source :directory))
         (buffer (let ((enable-local-variables nil)
                       (enable-dir-local-variables nil))
                   (find-file-noselect file))))
    (with-current-buffer buffer
      (unless buffer-read-only
        (read-only-mode 1))
      (when directory
        (setq default-directory directory)))
    (pop-to-buffer buffer)))

(defvar-keymap review-guide-mode-map
  :parent magit-section-mode-map
  "g" #'review-guide-refresh
  "p" #'review-guide-pr-review
  "P" #'review-guide-post
  "o" #'review-guide-visit-record
  "q" #'quit-window)

(define-derived-mode review-guide-mode magit-section-mode "Review-Guide"
  "Read a review record of `git delegate' and `pr-loop'.

\\{review-guide-mode-map}"
  :interactive nil
  :group 'review-guide
  (setq-local font-lock-defaults nil))

;;;; Entry point

(defun review-guide--default-target ()
  "Return the pull request of the `pr-review' buffer, if this is one."
  (and (boundp 'pr-review--pr-info)
       (derived-mode-p 'pr-review-mode)
       (alist-get 'url pr-review--pr-info)))

;;;###autoload
(defun review-guide (target)
  "Show the review record of TARGET, read-only.
TARGET is a pull request, as its URL or OWNER/REPO#N, a range
BASE..HEAD, or a branch, whose newest review is shown.  A range or
branch is read in the checkout `default-directory' is in."
  (interactive
   (let ((default (review-guide--default-target)))
     (list (read-string (format-prompt "Pull request, range or branch" default)
                        nil nil default))))
  (let* ((source (review-guide--resolve target))
         (records (review-guide--load source))
         (name (format "*review-guide %s/%s %s*" (plist-get source :owner)
                       (plist-get source :repo)
                       (if (car records)
                           (let ((range (car records)))
                             (concat (substring (review-guide-range-base range) 0 12) ".."
                                     (substring (review-guide-range-head range) 0 12)))
                         (format "#%d" (plist-get source :number)))))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'review-guide-mode)
        (review-guide-mode))
      (setq review-guide--source source
            review-guide--range (car records)
            review-guide--pr (cadr records)
            default-directory (or (plist-get source :checkout)
                                  (plist-get source :directory)
                                  default-directory))
      (review-guide--render))
    (pop-to-buffer buffer)))

(provide 'review-guide)
;;; review-guide.el ends here
