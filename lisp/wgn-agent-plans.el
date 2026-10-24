;;; wgn-agent-plans.el --- Read agents' plan files -*- lexical-binding: t; -*-

;; Plans are flat files in the plans directory, written by `agent-plan':
;;   <date>-<slug>.<host>.org         the plan
;;   <date>-<slug>.<host>.<part>.org  a subagent's part
;;   <date>-<slug>.<host>.log.org     the log
;; <date> is YYYY-MM-DD, or YYYYMMDDHHMMSS in plans named the org-roam
;; way; plans of both names are on disk, so both are read.
;; Only the plan files are listed here.  Nothing here adds them to
;; `org-agenda-files' or `org-refile-targets', or writes any other Org file.

(require 'filenotify)
(require 'json)
(require 'seq)

(declare-function org-create-dblock "org" (plist))
(declare-function org-update-dblock "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-entry-is-done-p "org" ())
(declare-function org-mode "org" ())
(declare-function org-mode-restart "org" ())
(defvar org-block-entry-blocking)
(defvar org-done-keywords)
(defvar org-inhibit-startup)

(defconst wgn/agent-plans--date-regexp
  "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-"
  "Match the date that starts a plan's name as `agent-plan' writes it today.
Groups 1 to 3 are its year, month and day.")

(defconst wgn/agent-plans--name-regexp
  (concat "\\(\\(?:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\|[0-9]\\{14\\}\\)"
          "-[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\)"
          "\\.\\([A-Za-z0-9][A-Za-z0-9_-]*\\)")
  "Match a plan's name and host, the start of each of its files' names.
Group 1 is the name, YYYY-MM-DD-slug or, named the org-roam way,
YYYYMMDDHHMMSS-slug; group 2 is the host.")

(defconst wgn/agent-plans--suffix-regexp "P[0-9]+\\|log"
  "Match what follows the host in a part's or a log's file name.")

(defconst wgn/agent-plans--file-regexp
  (concat "\\`" wgn/agent-plans--name-regexp
          "\\(?:\\.\\(" wgn/agent-plans--suffix-regexp "\\)\\)?\\.org\\'")
  "Match a plan's, a part's or a log's file name, as `agent-plan' names them.
Group 1 is the plan's name, 2 its host, and 3 the part or \"log\".
A plan's own file has no group 3.")

(defun wgn/agent-plans--newer-p (a b)
  "Return non-nil when the plan file A sorts before B, the newer first.
Plans are dated by their names, whether written YYYY-MM-DD or, named the
org-roam way, YYYYMMDDHHMMSS."
  (let ((key (lambda (file)
               (replace-regexp-in-string wgn/agent-plans--date-regexp "\\1\\2\\3-"
                                         (file-name-nondirectory file)))))
    (string> (funcall key a) (funcall key b))))

;; The keywords, and which are done, come from each file's own #+TODO
;; lines, so nothing here names one.  Entries are told apart by their ID
;; patterns, the contract with `agent-plan' that the plan-work skill
;; documents:

(defconst wgn/agent-plans-question-id "^\\(P[0-9]+\\.\\)?Q[0-9]+$"
  "Match the ID of a question for the operator.")

(defconst wgn/agent-plans-part-id "^P[0-9]+$"
  "Match the ID of a part.")

(defconst wgn/agent-plans-step-id "^\\(P[0-9]+\\.\\)?S[0-9]+$"
  "Match the ID of a step.")

(defconst wgn/agent-plans-followup-id "^\\(P[0-9]+\\.\\)?F[0-9]+$"
  "Match the ID of a follow-up.")

(defconst wgn/agent-plans--id-properties '("CUSTOM_ID" "ID")
  "The properties that hold an entry's ID, the first one set winning.
`agent-plan' writes items' IDs to ID; plans that are org-roam nodes hold
them in CUSTOM_ID, since org-id and org-roam take every ID as global.
Plans of both forms are on disk, so both are read.")

(defun wgn/agent-plans--id-line (id)
  "Return a regexp matching the property line that gives an entry the ID ID."
  (format "^[ \t]*:%s:[ \t]+%s[ \t]*$"
          (regexp-opt wgn/agent-plans--id-properties) (regexp-quote id)))

(defun wgn/agent-plans--entry-id ()
  "Return the ID of the entry at point, or nil."
  (seq-some (lambda (property) (org-entry-get nil property))
            wgn/agent-plans--id-properties))

(defun wgn/agent-plans--id-match (&rest patterns)
  "Return an agenda match for entries whose ID matches one of PATTERNS."
  (mapconcat (lambda (pattern)
               (mapconcat (lambda (property) (concat property "={" pattern "}"))
                          wgn/agent-plans--id-properties "|"))
             patterns "|"))

(defun wgn/agent-plans--config-file ()
  "Return the file `agent-plan' reads its settings from."
  (or (getenv "AGENT_TOOLS_CONFIG")
      (expand-file-name "agent-tools/config.json"
                        (or (getenv "XDG_CONFIG_HOME") "~/.config"))))

(defvar wgn/agent-plans--config-cache nil
  "(FILE ATTRIBUTES PLANS): the \"plans\" object of FILE, read when it had ATTRIBUTES.")

(defun wgn/agent-plans--plans-config ()
  "Return the \"plans\" object of `agent-plan''s config file, or nil.
It is read again only once the file, its modification time or its size
changes, since every change to a plan buffer, and every write of any
file, asks for it.  A file that is no JSON object is no config, so that
those writes go on."
  (let* ((file (wgn/agent-plans--config-file))
         (attributes (file-attributes file))
         (stamp (and attributes (list (file-attribute-modification-time attributes)
                                      (file-attribute-size attributes)))))
    (unless (equal (list file stamp) (butlast wgn/agent-plans--config-cache))
      (setq wgn/agent-plans--config-cache
            (list file stamp
                  (when (and stamp (file-readable-p file))
                    (with-temp-buffer
                      (insert-file-contents file)
                      (condition-case err
                          (let ((plans (gethash "plans" (json-parse-buffer))))
                            (and (hash-table-p plans) plans))
                        ((json-error wrong-type-argument)
                         (message "Ignoring %s, which is no JSON object: %s"
                                  file (error-message-string err))
                         nil)))))))
    (nth 2 wgn/agent-plans--config-cache)))

(defun wgn/agent-plans--config-path (key)
  "Return \"plans\".KEY in `agent-plan''s config file, or nil.
It is nil unless the file sets KEY to an absolute path, as `agent-plan'
requires: one starting with /, not ~."
  (let ((path (and (wgn/agent-plans--plans-config)
                   (gethash key (wgn/agent-plans--plans-config)))))
    (and (stringp path) (string-prefix-p "/" path) (expand-file-name path))))

(defun wgn/agent-plans--missing-config (key)
  "Return the message that `agent-plan''s config file sets no \"plans\".KEY."
  (format "agent-tools config is missing: %s sets no plans.%s"
          (wgn/agent-plans--config-file) key))

(defun wgn/agent-plans-directory ()
  "Return the directory holding the plans, \"plans.dir\" in `agent-plan''s
config file, or nil where the file does not set it."
  (wgn/agent-plans--config-path "dir"))

(defun wgn/agent-plans--require-directory ()
  "Return `wgn/agent-plans-directory', or refuse, saying the config is missing."
  (or (wgn/agent-plans-directory)
      (user-error "%s" (wgn/agent-plans--missing-config "dir"))))

(defun wgn/agent-plans-files ()
  "Return every plan file in the plans directory, newest first.
Other hosts' plans, which are mirrors, are included.  Part and log files
are not."
  (sort (let ((dir (wgn/agent-plans-directory)))
          (when (and dir (file-directory-p dir))
            (seq-filter (lambda (file)
                          (let ((base (file-name-nondirectory file)))
                            (and (string-match wgn/agent-plans--file-regexp base)
                                 (not (match-string 3 base)))))
                        (directory-files dir t wgn/agent-plans--file-regexp))))
        #'wgn/agent-plans--newer-p))

(defun wgn/agent-plans--run (plan &rest args)
  "Return the output of `agent-plan' ARGS for the plan file PLAN."
  (unless (executable-find "agent-plan")
    (user-error "agent-plan is not on `exec-path'"))
  (with-temp-buffer
    (let ((status (apply #'call-process "agent-plan" nil (list (current-buffer) nil) nil
                         (append args (list (expand-file-name plan))))))
      (unless (eq status 0)
        (error "agent-plan %s failed: %s" (car args) (buffer-string)))
      (buffer-string))))

(defun wgn/agent-plans-summary (plan)
  "Return the title, status and progress of the plan file PLAN."
  (let ((json (json-parse-string
               (wgn/agent-plans--run plan "status" "--format" "json"))))
    (list :title (gethash "title" json)
          :status (gethash "status" json)
          :progress (gethash "implemented" json))))

(defun wgn/agent-plans--candidate (plan)
  "Return a completion candidate describing the plan file PLAN."
  (let ((name (file-name-base plan)))
    (condition-case nil
        (let ((summary (wgn/agent-plans-summary plan)))
          (format "%s  %s  [%s]  %s" name (plist-get summary :title)
                  (plist-get summary :status) (plist-get summary :progress)))
      (error (format "%s  (status unavailable)" name)))))

(defun wgn/agent-plans--read ()
  "Ask for one of the plans, and return its plan file."
  (let ((candidates (mapcar (lambda (plan)
                              (cons (wgn/agent-plans--candidate plan) plan))
                            (or (wgn/agent-plans-files)
                                (user-error "No plans under %s"
                                            (wgn/agent-plans--require-directory))))))
    (cdr (assoc (completing-read
                 "Plan: "
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity))
                     (complete-with-action action candidates string predicate)))
                 nil t)
                candidates))))

;;;###autoload
(defun wgn/agent-plans ()
  "Choose among the agents' plans, and visit the chosen plan file."
  (interactive)
  (find-file (wgn/agent-plans--read)))

(defun org-dblock-write:agent-plan (params)
  "Write the status of a plan as Org tables, for a dynamic block.
PARAMS' :plan is a plan file; without it, the buffer's file."
  (let ((plan (or (plist-get params :plan)
                  buffer-file-name
                  (user-error "The agent-plan block needs a :plan"))))
    (insert
     (replace-regexp-in-string
      ;; A heading would end the section the block sits in.
      "\\`\\* [^\n]*\n" ""
      (wgn/agent-plans--run plan "status" "--format" "org")))))

;;;###autoload
(defun wgn/agent-plans-insert-status (plan)
  "Insert, and fill, an `agent-plan' dynamic block for the plan file PLAN.
Refresh it later with `org-update-dblock', \\[org-update-all-dblocks] for all."
  (interactive (list (wgn/agent-plans--read)))
  (org-create-dblock (list :name "agent-plan" :plan plan))
  (org-update-dblock))

(defun wgn/agent-plans-agenda-files ()
  "Return every plan's and part's file, newest plan first.
Logs are not."
  (seq-filter
   (lambda (file)
     (let ((parts (wgn/agent-plans--file-parts file)))
       (and parts (not (equal (nth 2 parts) "log")))))
   (let ((dir (wgn/agent-plans-directory)))
     (when (and dir (file-directory-p dir))
       (sort (directory-files dir t "\\.org\\'") #'wgn/agent-plans--newer-p)))))

(defun wgn/agent-plans-agenda-command ()
  "Return the `org-agenda-custom-commands' entry for key \"A\".
Its blocks are what waits on the operator, `wgn/agent-plans-agenda-waiting',
in this host's plans alone since another host's are answered there; the
parts and steps ready to work on; and the open follow-ups.  An entry
waiting on an :AFTER: item is hidden, as is one scheduled for a later
day.  `org-agenda-files' is bound to the plan files for this view alone."
  `("A" "Agent plans"
    ((wgn/agent-plans-agenda-waiting "")
     ;; Two matches rather than one regexp: the matcher splits at `|'.
     (tags-todo ,(wgn/agent-plans--id-match wgn/agent-plans-part-id wgn/agent-plans-step-id)
                ((org-agenda-overriding-header "Ready")))
     (tags-todo ,(wgn/agent-plans--id-match wgn/agent-plans-followup-id)
                ((org-agenda-overriding-header "Follow-ups"))))
    ((org-agenda-files (wgn/agent-plans-agenda-files))
     (org-agenda-dim-blocked-tasks 'invisible)
     (org-agenda-todo-ignore-scheduled 'future)
     (org-agenda-tags-todo-honor-ignore-options t))))

;;;; Open-questions indicator

(defgroup wgn/agent-plans nil
  "Read agents' plan files."
  :group 'tools)

(defcustom wgn/agent-plans-questions-interval 30
  "Seconds before `agent-plan statusline' runs again for a directory."
  :type 'number)

(defface wgn/agent-plans-questions
  '((t :inherit (warning bold)))
  "Face of the open-questions indicator.")

(defvar wgn/agent-plans-questions--string ""
  "The indicator, in `global-mode-string'; empty when nothing is open.")

(defvar wgn/agent-plans-questions--cache (make-hash-table :test #'equal)
  "Map a directory to (TIME . HINT), TIME being when its hint was requested.")

(defvar wgn/agent-plans-questions--last-directory nil
  "The directory the indicator currently describes.")

(defvar wgn/agent-plans-questions--map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'wgn/agent-plans)
    map)
  "Keymap of the indicator.")

(defun wgn/agent-plans-questions--propertize (hint)
  "Return HINT as the indicator, or the empty string when HINT is empty."
  (if (string-empty-p hint)
      ""
    (propertize (concat " " hint)
                'face 'wgn/agent-plans-questions
                'mouse-face 'mode-line-highlight
                'help-echo "mouse-1: choose an agent plan"
                'local-map wgn/agent-plans-questions--map)))

(defun wgn/agent-plans-questions--set (directory hint)
  "Show HINT if DIRECTORY is the one the indicator describes."
  (when (equal directory wgn/agent-plans-questions--last-directory)
    (setq wgn/agent-plans-questions--string
          (wgn/agent-plans-questions--propertize hint))
    (force-mode-line-update t)))

(defun wgn/agent-plans-questions--refresh (directory executable)
  "Run EXECUTABLE's `statusline' for DIRECTORY, without waiting for it."
  (let ((buffer (generate-new-buffer " *agent-plan statusline*"))
        (default-directory directory))
    (condition-case nil
        (let ((process
               (make-process
                :name "agent-plan-statusline"
                :buffer buffer
                :command (list executable "statusline")
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (process _event)
                  (unless (process-live-p process)
                    (let ((hint (if (eq (process-exit-status process) 0)
                                    (string-trim (with-current-buffer buffer
                                                   (buffer-string)))
                                  "")))
                      (kill-buffer buffer)
                      (puthash directory (cons (float-time) hint)
                               wgn/agent-plans-questions--cache)
                      (wgn/agent-plans-questions--set directory hint)))))))
          (process-send-string process (json-serialize `((cwd . ,directory))))
          (process-send-eof process))
      (error (kill-buffer buffer)))))

(defun wgn/agent-plans-questions--update ()
  "Show the open questions of the selected buffer's directory.
Never waits: a stale or missing hint is requested in the background.
Remote directories are skipped."
  (let ((directory (expand-file-name default-directory))
        (executable (executable-find "agent-plan")))
    (if (or (file-remote-p directory) (null executable))
        (progn (setq wgn/agent-plans-questions--last-directory nil)
               (wgn/agent-plans-questions--set nil ""))
      (let ((entry (gethash directory wgn/agent-plans-questions--cache)))
        (setq wgn/agent-plans-questions--last-directory directory)
        (wgn/agent-plans-questions--set directory (or (cdr entry) ""))
        (when (or (null entry)
                  (>= (- (float-time) (car entry))
                      wgn/agent-plans-questions-interval))
          (puthash directory (cons (float-time) (or (cdr entry) ""))
                   wgn/agent-plans-questions--cache)
          (wgn/agent-plans-questions--refresh directory executable))))))

(defun wgn/agent-plans-questions--post-command ()
  "Update the indicator when the selected buffer's directory changed or aged."
  (unless (minibufferp)
    (wgn/agent-plans-questions--update)))

;;;###autoload
(define-minor-mode wgn/agent-plans-questions-mode
  "Show a hint in the mode line when agent plans have open questions.
The hint is `agent-plan statusline' for the selected buffer's directory,
refreshed every `wgn/agent-plans-questions-interval' seconds.  Click it to
choose a plan."
  :global t
  (if wgn/agent-plans-questions-mode
      (progn
        (unless (memq 'wgn/agent-plans-questions--string global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '(""))
                        '(wgn/agent-plans-questions--string))))
        (add-hook 'post-command-hook #'wgn/agent-plans-questions--post-command))
    (remove-hook 'post-command-hook #'wgn/agent-plans-questions--post-command)
    (setq global-mode-string
          (delq 'wgn/agent-plans-questions--string global-mode-string)
          wgn/agent-plans-questions--string ""
          wgn/agent-plans-questions--last-directory nil)))

;;;; Plan files in Org

(defun wgn/agent-plans--config-value (key)
  "Return \"plans\".KEY in `agent-plan''s config file, or nil."
  (let ((plans (wgn/agent-plans--plans-config)))
    (and plans (gethash key plans))))

(defun wgn/agent-plans-host ()
  "Return this host's name in plan file names, as `agent-plan' takes it.
It is \"plans.host\" in its config file, else `system-name' up to the
first dot."
  (let ((host (wgn/agent-plans--config-value "host")))
    (if (and (stringp host) (not (string-empty-p host)))
        host
      (car (split-string (system-name) "\\.")))))

(defun wgn/agent-plans--file-parts (file)
  "Return (NAME HOST PART) of FILE in the plans directory, or nil.
PART is a part's ID, \"log\", or nil for the plan's own file."
  (when (and file
             (wgn/agent-plans-directory)
             (file-equal-p (file-name-directory file)
                           (file-name-as-directory (wgn/agent-plans-directory))))
    (let ((base (file-name-nondirectory file)))
      (when (string-match wgn/agent-plans--file-regexp base)
        (list (match-string 1 base) (match-string 2 base) (match-string 3 base))))))

(defun wgn/agent-plans--mirror-host (file)
  "Return the host whose plan FILE is, if not this host's."
  (let ((host (nth 1 (wgn/agent-plans--file-parts file))))
    (and host (not (equal host (wgn/agent-plans-host))) host)))

(defun wgn/agent-plans--own-files (file)
  "Return the plan's and parts' files of the plan FILE belongs to."
  (let ((parts (wgn/agent-plans--file-parts file)))
    (directory-files (file-name-directory file) t
                     (concat "\\`" (regexp-quote (concat (nth 0 parts) "." (nth 1 parts)))
                             "\\(\\.P[0-9]+\\)?\\.org\\'"))))

(defun wgn/agent-plans--settled-p (file id)
  "Return non-nil when ID is settled in the plan FILE belongs to.
Settled is Org's rule: the entry's keyword is a done one of its file.  An
ID the plan does not hold never settles."
  (let ((pattern (wgn/agent-plans--id-line id)))
    (seq-some
     (lambda (f)
       (let ((visiting (find-buffer-visiting f)))
         (with-current-buffer (or visiting (generate-new-buffer " *agent-plan*" t))
           (unwind-protect
               (progn
                 (unless visiting
                   (insert-file-contents f)
                   (let ((org-inhibit-startup t)) (delay-mode-hooks (org-mode))))
                 (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (and (re-search-forward pattern nil t)
                          (progn (org-back-to-heading t) (org-entry-is-done-p))
                          t))))
             (unless visiting (kill-buffer))))))
     (wgn/agent-plans--own-files file))))

(defun wgn/agent-plans--log-p (file)
  "Return non-nil when FILE is a plan's log, which `agent-plan log' alone writes."
  (equal (nth 2 (wgn/agent-plans--file-parts file)) "log"))

;; Review records, which `git delegate' and `pr-loop' alone write, are
;; held by the same lock as this host's plans: a deliberate edit, after
;; C-x C-q, is how the operator repairs one that either refuses.

(defvar review-guide-records-directory)

(defun wgn/agent-plans-records-directories ()
  "Return the directories of review records.
`git delegate' and `pr-loop' write them under agent-tools/reviews in
$XDG_STATE_HOME, else in ~/.local/state; `review-guide-records-directory',
when set, is where review-guide reads them."
  (let ((state (getenv "XDG_STATE_HOME")))
    (delete-dups
     (delq nil
           (list (file-name-as-directory
                  (expand-file-name "agent-tools/reviews"
                                    (if (and state (file-name-absolute-p state))
                                        state
                                      "~/.local/state")))
                 (and (bound-and-true-p review-guide-records-directory)
                      (file-name-as-directory
                       (expand-file-name review-guide-records-directory))))))))

(defun wgn/agent-plans--in-records-p (file)
  "Return non-nil when FILE is under a directory of review records."
  (and file
       (let ((truename (file-truename file)))
         (seq-some (lambda (directory)
                     (string-prefix-p (file-truename directory) truename))
                   (wgn/agent-plans-records-directories)))))

(defun wgn/agent-plans--record-p (file)
  "Return non-nil when FILE is a review record, an Org file in their directory.
Only records are locked: a save's backup and an auto-save file beside
one are Emacs's own."
  (and file
       (string-suffix-p ".org" file)
       (wgn/agent-plans--in-records-p file)))

(defun wgn/agent-plans--guarded-parts (file)
  "Return (NAME HOST PART) of FILE when the lock holds it, else nil.
That is a plan's, part's or log's file, as `wgn/agent-plans--file-parts'
gives it, or a review record, whose HOST is nil and PART \"record\"."
  (or (wgn/agent-plans--file-parts file)
      (and (wgn/agent-plans--record-p file)
           (list (file-name-nondirectory file) nil "record"))))

(defun wgn/agent-plans--record-locals (function &rest args)
  "Call FUNCTION with ARGS; under the records, with no file variables.
FUNCTION is `hack-local-variables', or `set-auto-mode', which reads a
-*- line's mode: itself.  A review record, and a backup of one, holds
text from models, GitHub and test runs, and Emacs's scan for a Local
Variables list reads the end of the file, example blocks and all."
  (if (wgn/agent-plans--in-records-p buffer-file-name)
      (let ((enable-local-variables nil)
            (enable-dir-local-variables nil))
        (apply function args))
    (apply function args)))

(defun wgn/agent-plans--locked-p (buffer)
  "Return non-nil when BUFFER visits a plan file that only `agent-plan' writes.
That is a log or another host's plan always, and this host's plan or
part, or a review record, while its buffer is read-only:
\\[read-only-mode] there is the operator's deliberate edit."
  (let* ((base (or (buffer-base-buffer buffer) buffer))
         (file (buffer-file-name base)))
    (and (wgn/agent-plans--guarded-parts file)
         (or (wgn/agent-plans--mirror-host file)
             (wgn/agent-plans--log-p file)
             (buffer-local-value 'buffer-read-only base)))))

(defun wgn/agent-plans--refusal (file &optional elsewhere)
  "Return why an edit of, or a write onto, the plan file FILE is refused.
It names what to use instead, and \\[read-only-mode] only where that
lets the edit through.  ELSEWHERE is for a write from anything but
FILE's own buffer, which \\[read-only-mode] there does not let through."
  (let ((host (wgn/agent-plans--mirror-host file)))
    (cond
     ((wgn/agent-plans--log-p file)
      (format "agent-plan log alone writes %s" (file-name-nondirectory file)))
     (host (format "%s is %s's plan; answer it there" (file-name-nondirectory file) host))
     ((and (wgn/agent-plans--record-p file) elsewhere)
      (format "git delegate and pr-loop write %s, and it is edited by hand only from its own buffer, after C-x C-q there"
              (file-name-nondirectory file)))
     ((wgn/agent-plans--record-p file)
      (format "git delegate and pr-loop write %s: C-x C-q to edit it by hand"
              (file-name-nondirectory file)))
     (elsewhere
      (format "agent-plan writes %s, and it is edited by hand only from its own buffer, after C-x C-q there"
              (file-name-nondirectory file)))
     (t
      (format (concat "agent-plan writes %s: answer its questions with /questions in a"
                      " Claude session, or C-x C-q to edit it by hand")
              (file-name-nondirectory file))))))

;; A save of every buffer, as `save-some-buffers' and
;; `org-save-all-org-buffers' make, stops at the first buffer whose save
;; signals, leaving every later one unsaved.  So while one runs, a save
;; this file refuses is skipped, with a warning, its buffer left
;; modified for `save-buffers-kill-emacs' to ask about; any other
;; failure stops it as before.

(defvar wgn/agent-plans--skipped 'none
  "While `save-some-buffers' runs, the buffers whose refused saves it skipped.")

(defvar wgn/agent-plans--refused nil
  "While a save of every buffer saves one, the data of this file's refusal.")

(defun wgn/agent-plans--refuse (format &rest args)
  "Signal a `user-error' with FORMAT and ARGS, refusing a write onto a plan file.
Only a refusal signalled here is skipped by a save of every buffer,
which tells it apart by its data: `wgn/agent-plans--skip-refused'."
  (let ((data (list (apply #'format-message format args))))
    (unless (eq wgn/agent-plans--skipped 'none)
      (setq wgn/agent-plans--refused data))
    (signal 'user-error data)))

(defun wgn/agent-plans--save-all (save-some-buffers &rest args)
  "Call SAVE-SOME-BUFFERS with ARGS, skipping the saves this file refuses."
  (let ((wgn/agent-plans--skipped nil))
    (if (called-interactively-p 'any)
        (apply #'funcall-interactively save-some-buffers args)
      (apply save-some-buffers args))))

(defun wgn/agent-plans--skip-refused (basic-save-buffer &rest args)
  "Call BASIC-SAVE-BUFFER with ARGS; in a save of every buffer, skip a refusal.
Only `wgn/agent-plans--refuse''s refusal of a save of a plan, part or
log file is caught, and warned about once per buffer; any other error,
and every refusal outside `save-some-buffers', is signalled as it is."
  (if (eq wgn/agent-plans--skipped 'none)
      (apply basic-save-buffer args)
    (let ((buffer (current-buffer))
          (wgn/agent-plans--refused nil))
      (condition-case err
          (apply basic-save-buffer args)
        (user-error
         (unless (and wgn/agent-plans--refused
                      (eq (cdr err) wgn/agent-plans--refused)
                      (wgn/agent-plans--guarded-parts (buffer-file-name buffer)))
           (signal (car err) (cdr err)))
         (unless (memq buffer wgn/agent-plans--skipped)
           (push buffer wgn/agent-plans--skipped)
           ;; `save-some-buffers' runs in `save-window-excursion', which
           ;; takes a displayed warning's window away again.
           (delay-warning 'wgn/agent-plans
                          (format-message "%s is left unsaved: %s"
                                          (buffer-name buffer) (cadr err))))
         nil)))))

(defun wgn/agent-plans--confirm-exit ()
  "Ask before Emacs exits with an auto-saved plan buffer's changes unsaved.
For `kill-emacs-query-functions'.  `save-buffers-kill-emacs' asks only
about a buffer whose `buffer-modified-p' is t, not `autosaved', as a
recovered buffer's is, and its save of every buffer skips the refused."
  (let ((unsaved (seq-filter (lambda (buffer)
                               (and (eq (buffer-modified-p buffer) 'autosaved)
                                    (with-current-buffer buffer
                                      (wgn/agent-plans--plan-buffer-p))))
                             (buffer-list))))
    (or (null unsaved)
        (yes-or-no-p (format "%s left unsaved; exit anyway? "
                             (mapconcat #'buffer-name unsaved ", "))))))

(defun wgn/agent-plans-blocker (change)
  "Refuse the Org state CHANGE where a plan does not allow it.
In a plan file `wgn/agent-plans--locked-p' holds, every change; in one
opened for a deliberate edit, a change to done while an ID in the
entry's :AFTER: is not settled.  Outside plan files, allow everything.
The agenda's dim check, whose :from is the symbol `todo', is answered
for :AFTER: alone, so locked entries stay listed."
  (let ((file (buffer-file-name (buffer-base-buffer)))
        (to (plist-get change :to)))
    (cond
     ((not (wgn/agent-plans--guarded-parts file)) t)
     ((and (wgn/agent-plans--locked-p (current-buffer))
           (not (eq (plist-get change :from) 'todo)))
      (setq org-block-entry-blocking (wgn/agent-plans--refusal file))
      nil)
     ((not (or (eq to 'done) (member to org-done-keywords))) t)
     (t
      (let ((open (seq-remove (lambda (id) (wgn/agent-plans--settled-p file id))
                              (split-string
                               (or (org-entry-get (plist-get change :position) "AFTER") "")))))
        (when open
          (setq org-block-entry-blocking (string-join open " ")))
        (null open))))))

(defvar-local wgn/agent-plans--protected nil
  "The plan file this buffer was visited on, or nil.
It survives a mode change; \\[write-file] keeps it while renaming the
buffer, which is how a write to another plan file is told apart.")
(put 'wgn/agent-plans--protected 'permanent-local t)

;; A plan or part file is never written from Emacs over content its
;; buffer has not read, since agent-plan writes it whenever no lock file
;; stops it.  A save compares the file with what its buffer last read or
;; wrote of it, by hash: the visited modification time cannot tell, as
;; `set-visited-file-name' and dired's renames clear it.

(defvar-local wgn/agent-plans--seen nil
  "(FILE . HASH): what the buffer last read or wrote of the plan file FILE.
HASH is the SHA-256 of the file's bytes then, or nil when no version
of FILE is known to be the base of the buffer's text, as for text
recovered from an auto-save file: `wgn/agent-plans--note-unknown-base'.
It survives a mode change and a rename away and back.")
(put 'wgn/agent-plans--seen 'permanent-local t)

(defun wgn/agent-plans--buffer-hash ()
  "Return the SHA-256 of the current buffer's text, as saving encodes it.
After a visit, a revert or a save, that is the file's bytes."
  (save-restriction
    (widen)
    (secure-hash 'sha256 (encode-coding-string
                          (buffer-substring-no-properties (point-min) (point-max))
                          (or save-buffer-coding-system buffer-file-coding-system)
                          t))))

(defun wgn/agent-plans--file-hash (file)
  "Return the SHA-256 of FILE's bytes, or nil if there is no FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun wgn/agent-plans--unknown-base-p ()
  "Return non-nil when the current buffer's text has no known base in its file.
See `wgn/agent-plans--note-unknown-base'."
  (equal wgn/agent-plans--seen (list buffer-file-name)))

(defun wgn/agent-plans--note-unknown-base (&rest _)
  "Note that no version of its file is known to be a plan buffer's base.
Before `after-find-file', which `recover-file' and a revert from the
auto-save file call on a buffer holding the auto-save file's text, and
so modified; a visit or a revert from the file itself leaves it
unmodified.  Its save is then refused until it reads the file again,
since `agent-plan' may have written the file after the text's base."
  (when (and (buffer-modified-p) (wgn/agent-plans--plan-buffer-p))
    (setq wgn/agent-plans--seen (list buffer-file-name))))

(defun wgn/agent-plans--note-seen (&optional from-file)
  "Note that the current buffer holds what its plan file holds.
With FROM-FILE, note the file's bytes rather than the buffer's: for a
buffer whose changes are not the file's, which it last read when its
visited modification time says.  Text with no known base keeps that
until the buffer is unmodified, as the hooks after `after-find-file'
would otherwise note the file it never read."
  (unless (and (buffer-modified-p) (wgn/agent-plans--unknown-base-p))
    (setq wgn/agent-plans--seen
          (cons buffer-file-name
                (if from-file
                    (wgn/agent-plans--file-hash buffer-file-name)
                  (wgn/agent-plans--buffer-hash))))))

(defun wgn/agent-plans--seen-current-p (file)
  "Return non-nil when FILE holds what the current buffer last read or wrote of it."
  (and (equal (car wgn/agent-plans--seen) file)
       (cdr wgn/agent-plans--seen)
       (equal (cdr wgn/agent-plans--seen) (wgn/agent-plans--file-hash file))))

(defun wgn/agent-plans--refuse-unseen (file)
  "Refuse a write onto the plan file FILE unless it holds what this buffer read.
The buffer is left as it is, its changes unsaved."
  (unless (wgn/agent-plans--seen-current-p file)
    (wgn/agent-plans--refuse
     (if (cdr wgn/agent-plans--seen)
         (concat "%s holds what this buffer has not read, likely agent-plan's write:"
                 " M-x diff-buffer-with-file shows it, and M-x revert-buffer reads it,"
                 " dropping this buffer's changes")
       (concat "Which version of %s this buffer's text is based on is unknown, as for"
               " text recovered from an auto-save file, and agent-plan may have written"
               " it since: M-x diff-buffer-with-file or M-x ediff-current-file compares"
               " them, and M-x revert-buffer reads the file, dropping this buffer's changes"))
     (file-name-nondirectory file))))

(defun wgn/agent-plans--note-read ()
  "Note what a plan buffer read, for `after-revert-hook', auto-revert's too."
  (when (wgn/agent-plans--plan-buffer-p)
    (wgn/agent-plans--note-seen)))

(defun wgn/agent-plans--after-save ()
  "Note what a plan buffer wrote and arm it again, for `after-save-hook'.
\\[write-file] makes the buffer writable bypassing `read-only-mode-hook'."
  (when (wgn/agent-plans--plan-buffer-p)
    (wgn/agent-plans--note-seen)
    (wgn/agent-plans--arm)))

(defun wgn/agent-plans--guard-write ()
  "Refuse to write a buffer to a plan, part or log file it may not write.
For `write-file-functions'.  A log's own buffer discards its changes
instead, and writes nothing, since only `agent-plan' writes a log and a
signal would stop `save-some-buffers' for every other file.  A plan's
or part's file is written only from the buffer visiting it, opened for
a deliberate edit, only for this host, and only over what that buffer
last read of it; so \\[write-file] from elsewhere onto one is refused
too."
  (let ((parts (wgn/agent-plans--guarded-parts buffer-file-name))
        (elsewhere (not (equal wgn/agent-plans--protected buffer-file-name))))
    (cond
     ((not parts) nil)
     ((and (wgn/agent-plans--log-p buffer-file-name) (not elsewhere))
      (wgn/agent-plans--settle-changes)
      t)
     ((or (wgn/agent-plans--log-p buffer-file-name)
          (wgn/agent-plans--mirror-host buffer-file-name)
          elsewhere
          buffer-read-only)
      (wgn/agent-plans--refuse "%s" (wgn/agent-plans--refusal buffer-file-name elsewhere)))
     (t
      (wgn/agent-plans--refuse-unseen buffer-file-name)
      nil))))

(defun wgn/agent-plans--refuse-write-onto (file)
  "Refuse a write onto FILE, an absolute name, if a plan's, part's or log's.
Only the save of this host's plan or part, from the buffer visiting it,
is let through, `wgn/agent-plans--guard-write' having decided it; and
only over what that buffer last read of the file, if it is there.  It is
not there once a backup has renamed it away."
  (when (wgn/agent-plans--guarded-parts file)
    (if (or (wgn/agent-plans--log-p file)
            (wgn/agent-plans--mirror-host file)
            (not (and (equal file buffer-file-name)
                      (equal wgn/agent-plans--protected buffer-file-name))))
        (wgn/agent-plans--refuse "%s" (wgn/agent-plans--refusal file t))
      (when (file-exists-p file)
        (wgn/agent-plans--refuse-unseen file)))))

(defun wgn/agent-plans--guard-region-write (_start _end filename &rest _)
  "Refuse a `write-region' onto a plan, part or log file FILENAME.
As `append-to-file' does; see `wgn/agent-plans--refuse-write-onto'."
  (when (stringp filename)
    (wgn/agent-plans--refuse-write-onto (expand-file-name filename))))

(defun wgn/agent-plans--guard-file-copy (file newname &rest _)
  "Refuse to copy or move FILE onto NEWNAME, a plan, part or log file.
Before `copy-file' and `rename-file', as dired's C and R call them.  A
save with `file-precious-flag' moves a file onto the one it saves; see
`wgn/agent-plans--refuse-write-onto'."
  (wgn/agent-plans--refuse-write-onto
   (if (directory-name-p newname)
       (expand-file-name (file-name-nondirectory (directory-file-name file)) newname)
     (expand-file-name newname))))

(defun wgn/agent-plans--refuse-visit (filename &rest _)
  "Refuse to have the current buffer visit FILENAME, a plan, part or log file.
Before `set-visited-file-name', as \\[write-file], `rename-visited-file'
and \\[save-buffer] in a buffer visiting no file call it.  Only visiting
a file guards its buffer, so a buffer may take back the plan file it was
visited on, `wgn/agent-plans--protected', and no other.  Refused before
it runs, the buffer keeps its name, mode, auto-save file and lock."
  (when (and (stringp filename) (not (string-empty-p filename)))
    (let ((file (expand-file-name filename)))
      (when (and (wgn/agent-plans--guarded-parts file)
                 (not (equal file wgn/agent-plans--protected)))
        (user-error "%s" (wgn/agent-plans--refusal file t))))))

(defun wgn/agent-plans--undo-refused-rename (write-file filename &rest args)
  "Call WRITE-FILE with FILENAME and ARGS; if refused, unrename the buffer.
\\[write-file] renames the buffer onto FILENAME before saving it, so a
refused save onto a plan file would leave the buffer visiting the plan.
It also unlocks the buffer, marks it modified, sets its major mode and
auto-save file by the plan's name, and renaming it back forgets when its
file was last read, which is how a save notices that `agent-plan' wrote
it since; all are put back.  Renaming a buffer onto the file it already
visits changes no name to put back, so a locked buffer's write onto a
plan, part or log file, its own file too, is refused before WRITE-FILE
runs."
  (let* ((own (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
         (target (cond ((or (null filename) (equal filename "")) own)
                       ((directory-name-p filename)
                        (expand-file-name (file-name-nondirectory (or own (buffer-name)))
                                          filename))
                       (t (expand-file-name filename)))))
    (when (and (wgn/agent-plans--guarded-parts target)
               (wgn/agent-plans--locked-p (current-buffer)))
      (user-error "%s" (wgn/agent-plans--refusal target (not (equal target own))))))
  (let ((file buffer-file-name)
        (name (buffer-name))
        (mode major-mode)
        (auto-save buffer-auto-save-file-name)
        (read-only buffer-read-only)
        (modified (buffer-modified-p))
        (modtime (visited-file-modtime)))
    (condition-case err
        (apply write-file filename args)
      (error
       (when (and (not (equal buffer-file-name file))
                  (wgn/agent-plans--guarded-parts buffer-file-name))
         (set-visited-file-name file t)
         (rename-buffer name t)
         (set-buffer-modified-p modified)
         (unless (eq major-mode mode)
           (funcall mode))
         (if file
             (unless (numberp modtime)
               (set-visited-file-modtime modtime))
           (setq buffer-auto-save-file-name auto-save))
         (setq buffer-read-only read-only))
       (signal (car err) (cdr err))))))

(defun wgn/agent-plans--rearm (buffer hooks)
  "Give BUFFER back its change functions HOOKS, guard and Org's alike.
Emacs sets the buffer's whole `before-change-functions' to nil when one
of them signals, as the guard does to refuse a change."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (memq #'wgn/agent-plans--guard-change before-change-functions)
        (setq-local before-change-functions hooks)))))

(defun wgn/agent-plans--guard-change (&rest _)
  "Refuse a change to a locked plan buffer, for `before-change-functions'.
Code that binds `inhibit-read-only', as Org's property and ID commands
do, gets past the buffer's read-only state but not this; a revert is
let through.  The buffer's change functions are put back once the
refused command is over.  The match data is kept, as `replace-match'
still needs it."
  (when (and (not revert-buffer-in-progress-p)
             (save-match-data (wgn/agent-plans--locked-p (current-buffer))))
    (run-at-time 0 nil #'wgn/agent-plans--rearm (current-buffer)
                 (copy-sequence before-change-functions))
    (wgn/agent-plans--refuse "%s" (wgn/agent-plans--refusal
                                   (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))))

(defconst wgn/agent-plans--agenda-edits
  '(org-agenda-todo org-agenda-todo-nextset org-agenda-todo-previousset
    org-agenda-priority org-agenda-priority-up org-agenda-priority-down
    org-agenda-set-tags org-agenda-schedule org-agenda-deadline
    org-agenda-date-later org-agenda-date-earlier org-agenda-date-prompt
    org-agenda-set-property org-agenda-set-effort org-agenda-toggle-archive-tag
    org-agenda-archive org-agenda-archive-default org-agenda-archive-to-archive-sibling
    org-agenda-refile org-agenda-kill org-agenda-add-note org-agenda-clock-in)
  "Agenda commands that write the entry at point.
They bind `inhibit-read-only', which a read-only buffer does not stop.")

(defun wgn/agent-plans--agenda-guard (command &rest args)
  "Run the agenda COMMAND with ARGS unless its entry is a locked plan's."
  (let* ((marker (or (get-text-property (line-beginning-position) 'org-hd-marker)
                     (get-text-property (line-beginning-position) 'org-marker)))
         (buffer (and (markerp marker) (marker-buffer marker))))
    (when (and buffer (wgn/agent-plans--locked-p buffer))
      (user-error "%s" (wgn/agent-plans--refusal
                        (buffer-file-name (or (buffer-base-buffer buffer) buffer)))))
    (apply command args)))

(defun wgn/agent-plans--refuse-move (&rest _)
  "Refuse to archive an entry out of a locked plan, before `org-archive-subtree'.
It writes the entry into its new place before cutting it from the plan,
so the cut's refusal would leave a copy behind."
  (when (wgn/agent-plans--locked-p (current-buffer))
    (user-error "%s" (wgn/agent-plans--refusal
                      (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))))

(defun wgn/agent-plans--refuse-refile-out (&optional arg &rest _)
  "Refuse to refile out of a locked plan, before `org-refile' with ARG.
As `wgn/agent-plans--refuse-move'; a refile that copies (ARG 3) or only
jumps (a prefix list, or 0) leaves the plan as it is."
  (unless (or (equal arg 3) (equal arg 0) (consp arg))
    (wgn/agent-plans--refuse-move)))

(defun wgn/agent-plans--header (file)
  "Return the header line of the buffer visiting FILE.
FILE is a plan's, part's or log's, or a review record."
  (let ((host (wgn/agent-plans--mirror-host file)))
    (cond
     (host (format " %s's plan, written there · C-c C-o follows a link" host))
     ((equal (nth 2 (wgn/agent-plans--file-parts file)) "log")
      " Written by agent-plan log alone")
     ((wgn/agent-plans--record-p file)
      " Written by git delegate and pr-loop · M-x review-guide shows it with its diff")
     (t (concat " Written by agent-plan · answer questions with /questions in a Claude session"
                " · C-c C-o follows a link · org-capture A f or A q adds a follow-up or question")))))

(defun wgn/agent-plans--plan-buffer-p ()
  "Return non-nil when the current buffer was visited on the plan file it visits.
A buffer showing a file's old revision, as magit's blob buffers do,
binds `buffer-file-name' while it sets its mode but visits nothing, and
one \\[write-file] renamed onto a plan file was not visited on it."
  (and buffer-file-name
       (equal wgn/agent-plans--protected buffer-file-name)
       (wgn/agent-plans--guarded-parts buffer-file-name)))

(defvar wgn/agent-plans--locking nil
  "Non-nil while this file locks a buffer, which has nothing to settle.")

(defun wgn/agent-plans--settle-changes ()
  "Have a modified plan buffer saved or its changes discarded, before it locks.
A locked buffer that is modified could not be saved, and every
`save-some-buffers' would skip it with a warning.  The buffer is writable while
it is saved or reverted.  Return nil, saying why, when the operator
chooses neither or the choice fails, as a save the operator does not
confirm does; the buffer is then to stay unlocked.  A log's changes,
which no command lets through, are discarded without asking."
  (or (not (buffer-modified-p))
      (let* ((log (wgn/agent-plans--log-p buffer-file-name))
             (failure
              (condition-case err
                  (let ((buffer-read-only nil))
                    (pcase (if log
                               ?d
                             (car (read-multiple-choice
                                   (format "%s has unsaved changes; before it locks" (buffer-name))
                                   '((?s "save" "Save the changes, then lock")
                                     (?d "discard" "Revert to the file, then lock")))))
                      (?s (save-buffer))
                      (?d (revert-buffer t t t)))
                    (and (buffer-modified-p) "the changes are still unsaved"))
                (quit "neither was chosen")
                (error (error-message-string err)))))
        (cond
         (failure (message (if log "%s keeps its changes: %s" "%s stays unlocked: %s")
                           (buffer-name) failure))
         (log (message "Discarded the changes to %s: %s" (buffer-name)
                       (wgn/agent-plans--refusal buffer-file-name))))
        (not failure))))

(defun wgn/agent-plans--sync-lockfiles ()
  "Make lock files of the current plan buffer while it takes changes alone.
`agent-plan' refuses to write a plan whose lock file says a buffer holds
unsaved changes of it.  A change refused while the buffer is locked
would leave a lock file behind, since Emacs makes one before the guard
refuses.  Emacs makes a lock file only as a buffer first changes, so a
buffer unlocked with changes already, as a rename leaves it, is locked
here."
  (if (wgn/agent-plans--locked-p (current-buffer))
      (setq-local create-lockfiles nil)
    (kill-local-variable 'create-lockfiles)
    (when (and buffer-file-truename (buffer-modified-p))
      (lock-buffer))))

(defun wgn/agent-plans--arm ()
  "Give the current plan buffer its header line and change guard.
They refuse nothing while it is unlocked, but must be there once it
locks, whichever way it does.  Its indirect buffers are locked or
unlocked with it."
  (setq header-line-format (wgn/agent-plans--header buffer-file-name))
  (wgn/agent-plans--sync-lockfiles)
  (add-hook 'before-change-functions #'wgn/agent-plans--guard-change nil t)
  (let ((base (current-buffer))
        (read-only buffer-read-only))
    (dolist (buffer (buffer-list))
      (when (eq (buffer-base-buffer buffer) base)
        (with-current-buffer buffer
          (setq buffer-read-only read-only)
          (setq header-line-format (with-current-buffer base header-line-format))
          (wgn/agent-plans--sync-lockfiles)
          (add-hook 'before-change-functions #'wgn/agent-plans--guard-change nil t))))))

(defun wgn/agent-plans--protect-file ()
  "Make a plan, part or log file's buffer read-only, naming the commands.
`agent-plan' writes them, so a change is refused while
`wgn/agent-plans--locked-p' holds.  Run from `org-mode-hook' too, since
a mode change drops the buffer's local hooks and header line, and after
every revert, which would restore an earlier unlocked state.  Unsaved
changes are saved or discarded first, else the buffer is unlocked."
  (when (wgn/agent-plans--plan-buffer-p)
    (if (not (wgn/agent-plans--settle-changes))
        (when buffer-read-only
          (read-only-mode -1))
      (let ((wgn/agent-plans--locking t))
        (read-only-mode 1)))
    (wgn/agent-plans--arm)))

(defun wgn/agent-plans--on-read-only ()
  "Settle a plan buffer's changes when \\[read-only-mode] locks it.
For `read-only-mode-hook'.  Without a choice the buffer stays unlocked.
Either way, it is armed again: `wgn/agent-plans--arm'.  In an indirect
buffer of a plan buffer, which `wgn/agent-plans--locked-p' answers for
by its base, \\[read-only-mode] locks or unlocks the base."
  (let ((base (buffer-base-buffer)))
    (cond
     ((and base (with-current-buffer base (wgn/agent-plans--plan-buffer-p)))
      (let ((read-only buffer-read-only))
        (with-current-buffer base
          (if (eq (not buffer-read-only) (not read-only))
              (wgn/agent-plans--arm)
            (read-only-mode (if read-only 1 -1))))))
     ((wgn/agent-plans--plan-buffer-p)
      (cond
       ((and (not buffer-read-only) (wgn/agent-plans--log-p buffer-file-name))
        (wgn/agent-plans--protect-file)
        (message "%s" (wgn/agent-plans--refusal buffer-file-name)))
       ((and buffer-read-only
             (not wgn/agent-plans--locking)
             (not (wgn/agent-plans--settle-changes)))
        (read-only-mode -1)))
      (wgn/agent-plans--arm)))))

(defun wgn/agent-plans--protect-visited ()
  "Protect a plan, part or log file's buffer just visited, for `find-file-hook'.
A buffer changed as it is visited, as `require-final-newline' can leave
it, last read the file itself; one holding an auto-save file's text
read none of it: `wgn/agent-plans--note-unknown-base'."
  (when (wgn/agent-plans--guarded-parts buffer-file-name)
    (setq wgn/agent-plans--protected buffer-file-name)
    (wgn/agent-plans--note-seen (buffer-modified-p))
    (wgn/agent-plans--protect-file)))

(defun wgn/agent-plans--protect-all ()
  "Protect every plan, part or log file's buffer, as visiting it does.
For the buffers visited before `wgn/agent-plans-protect-mode' was on.
One with unsaved changes is left unlocked, rather than asked about, and
is taken to have read its file only if the file is unchanged since."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (not (buffer-base-buffer))
                 (wgn/agent-plans--guarded-parts buffer-file-name))
        (setq wgn/agent-plans--protected buffer-file-name)
        (cond
         ((not (buffer-modified-p))
          (wgn/agent-plans--note-seen)
          (wgn/agent-plans--protect-file))
         (t
          (unless (wgn/agent-plans--unknown-base-p)
            (setq wgn/agent-plans--seen nil))
          (when (and (not (eql (visited-file-modtime) 0)) (verify-visited-file-modtime))
            (wgn/agent-plans--note-seen t))
          (when buffer-read-only
            (read-only-mode -1))
          (wgn/agent-plans--arm)))))))

(defun wgn/agent-plans--revisit ()
  "Protect a buffer `set-visited-file-name' left on its plan file again.
For `after-set-visited-file-name-hook'.  That function, which
\\[write-file] and dired's renames of a directory call, marks the buffer
modified and forgets when the file was last read, so auto-revert no
longer sees the file change; under version control it also makes the
buffer writable, bypassing `read-only-mode-hook'.  A buffer holding just
what it last read, of a file that holds it still, is marked unmodified
and read then, and locked again.  Any other log is reverted and locked.
Any other plan or part is left unlocked holding its lock file, its save
refused; and if the file changed, it is marked as read before the
change, for auto-revert to read it again."
  (when (wgn/agent-plans--plan-buffer-p)
    (let ((read (and (equal (car wgn/agent-plans--seen) buffer-file-name)
                     (equal (cdr wgn/agent-plans--seen) (wgn/agent-plans--buffer-hash))))
          (current (wgn/agent-plans--seen-current-p buffer-file-name)))
      (cond
       ((and read current)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil)
        (wgn/agent-plans--protect-file))
       ((wgn/agent-plans--log-p buffer-file-name)
        (wgn/agent-plans--protect-file))
       (t
        (when buffer-read-only
          (read-only-mode -1))
        (wgn/agent-plans--arm)
        (when (and (not current) (file-exists-p buffer-file-name))
          (set-visited-file-modtime '(0 1))))))))

(defun wgn/agent-plans--protect-indirect (buffer)
  "Lock BUFFER, an indirect buffer, as its base is, if a plan's; return BUFFER.
After `make-indirect-buffer', whose buffer without CLONE has none of its
base's local hooks, though its edits are the base's."
  (let ((base (buffer-base-buffer buffer)))
    (when (and base (with-current-buffer base (wgn/agent-plans--plan-buffer-p)))
      (with-current-buffer buffer
        (setq buffer-read-only (buffer-local-value 'buffer-read-only base))
        (setq header-line-format (with-current-buffer base header-line-format))
        (wgn/agent-plans--sync-lockfiles)
        (add-hook 'before-change-functions #'wgn/agent-plans--guard-change nil t))))
  buffer)

(defconst wgn/agent-plans--has-restore-functions
  (boundp 'revert-buffer-restore-functions)
  "Non-nil where Emacs has `revert-buffer-restore-functions', from Emacs 30.
Where it has not, `add-hook' would define it without anything running it.")

(defun wgn/agent-plans--around-revert (revert &rest args)
  "Call REVERT, `revert-buffer', with ARGS, then lock a plan buffer again.
Before Emacs 30, `revert-buffer' restores the read-only state from before
the revert after `after-revert-hook' has run, so only this is late enough."
  (prog1 (apply revert args)
    (wgn/agent-plans--protect-file)))

(defun wgn/agent-plans--restore-lock ()
  "Return what locks a plan buffer again after a revert.
For `revert-buffer-restore-functions', whose first entry runs last, so
after the read-only state from before the revert is restored."
  (when (wgn/agent-plans--plan-buffer-p)
    (let ((buffer (current-buffer)))
      (lambda ()
        (with-current-buffer buffer
          (wgn/agent-plans--protect-file))))))

(defvar wgn/agent-plans--auto-reverted 'none
  "While `auto-revert-handler' runs, the plan buffers it reverted.")

(defun wgn/agent-plans--around-auto-revert (handler &rest args)
  "Run HANDLER, `auto-revert-handler', with ARGS, then lock what it reverted.
It binds `buffer-read-only' around the revert, so the lock the revert
put back ends with it."
  (let ((wgn/agent-plans--auto-reverted nil))
    (prog1 (apply handler args)
      (dolist (buffer wgn/agent-plans--auto-reverted)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (wgn/agent-plans--protect-file)))))))

(defvar-local wgn/agent-plans--folds nil
  "The plan buffer's folds before a revert, with markers.")
(put 'wgn/agent-plans--folds 'permanent-local t)

(defvar auto-revert--global-mode)
(declare-function org-fold-core-get-regions "org-fold-core")
(declare-function org-fold-core-regions "org-fold-core" (regions &rest args))

(defun wgn/agent-plans--before-revert ()
  "Keep a plan buffer's folds, for `before-revert-hook'."
  (when (and (derived-mode-p 'org-mode) (wgn/agent-plans--plan-buffer-p))
    (setq wgn/agent-plans--folds (org-fold-core-get-regions :with-markers t))))

(defun wgn/agent-plans--after-revert ()
  "Read a reverted plan file's keyword lines again, keeping its folds.
A revert that keeps the buffer's modes, as auto-revert's does, keeps the
keywords Org read before; `org-mode-restart' wipes buffer-local hooks,
so this one is global, and unfolds the buffer, so the folds kept before
the revert are put back.  It also clears `global-auto-revert-mode''s mark
on the buffer, which only visiting the file would set again, so the mark
is kept."
  (when (and (derived-mode-p 'org-mode) (wgn/agent-plans--plan-buffer-p))
    (unless (eq wgn/agent-plans--auto-reverted 'none)
      (push (current-buffer) wgn/agent-plans--auto-reverted))
    (let ((global-auto-revert (bound-and-true-p auto-revert--global-mode)))
      (org-mode-restart)
      (when global-auto-revert
        (setq-local auto-revert--global-mode t)))
    (when wgn/agent-plans--folds
      (org-fold-core-regions wgn/agent-plans--folds :override t :clean-markers t)
      (setq wgn/agent-plans--folds nil))))

(defun wgn/agent-plans--unprotect-all ()
  "Take the header line and change guard from every plan buffer.
Their read-only state is left as it is, for \\[read-only-mode] to change."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (memq #'wgn/agent-plans--guard-change before-change-functions)
        (remove-hook 'before-change-functions #'wgn/agent-plans--guard-change t)
        (kill-local-variable 'create-lockfiles)
        (setq header-line-format nil)))))

;;;###autoload
(define-minor-mode wgn/agent-plans-protect-mode
  "Hold plan files to what `agent-plan' expects of them.
A plan's, part's or log's buffer is read-only, since `agent-plan' writes
them, with a header line naming the commands that do, and refuses saves,
state changes and the agenda's edits until \\[read-only-mode] there; logs
and another host's plans refuse them always; a save writes over only
what the buffer last read of its file, which text recovered from an
auto-save file did not read; saving every buffer skips, with a
warning, one whose save is refused; an entry is not done while an item in
its :AFTER: is open; a reverted plan buffer, auto-reverted too, reads
its keyword lines again, keeps its folds and is locked again; a plan
buffer's unsaved changes are saved or discarded before it locks; and
its indirect buffers are locked with it.  Review records, under
`wgn/agent-plans-records-directories', are held as this host's plans
are, since only `git delegate' and `pr-loop' write them, and no file
variables apply in them.  Turning the mode on protects the plan
buffers already visited; turning it off takes the header line and
guard from plan buffers and leaves them read-only."
  :global t
  (if wgn/agent-plans-protect-mode
      (progn
        (add-hook 'org-blocker-hook #'wgn/agent-plans-blocker)
        (add-hook 'find-file-hook #'wgn/agent-plans--protect-visited)
        (add-hook 'org-mode-hook #'wgn/agent-plans--protect-file)
        (add-hook 'before-revert-hook #'wgn/agent-plans--before-revert)
        (add-hook 'after-revert-hook #'wgn/agent-plans--after-revert)
        (add-hook 'after-revert-hook #'wgn/agent-plans--note-read)
        (add-hook 'after-save-hook #'wgn/agent-plans--after-save)
        (add-hook 'after-set-visited-file-name-hook #'wgn/agent-plans--revisit)
        (if wgn/agent-plans--has-restore-functions
            (add-hook 'revert-buffer-restore-functions #'wgn/agent-plans--restore-lock)
          (advice-add 'revert-buffer :around #'wgn/agent-plans--around-revert))
        (add-hook 'read-only-mode-hook #'wgn/agent-plans--on-read-only)
        (add-hook 'write-file-functions #'wgn/agent-plans--guard-write)
        (advice-add 'auto-revert-handler :around #'wgn/agent-plans--around-auto-revert)
        (advice-add 'after-find-file :before #'wgn/agent-plans--note-unknown-base)
        (advice-add 'make-indirect-buffer :filter-return #'wgn/agent-plans--protect-indirect)
        (advice-add 'write-region :before #'wgn/agent-plans--guard-region-write)
        (advice-add 'copy-file :before #'wgn/agent-plans--guard-file-copy)
        (advice-add 'rename-file :before #'wgn/agent-plans--guard-file-copy)
        (advice-add 'write-file :around #'wgn/agent-plans--undo-refused-rename)
        (advice-add 'save-some-buffers :around #'wgn/agent-plans--save-all)
        (advice-add 'basic-save-buffer :around #'wgn/agent-plans--skip-refused)
        (add-hook 'kill-emacs-query-functions #'wgn/agent-plans--confirm-exit)
        (advice-add 'set-visited-file-name :before #'wgn/agent-plans--refuse-visit)
        (advice-add 'org-archive-subtree :before #'wgn/agent-plans--refuse-move)
        (advice-add 'org-refile :before #'wgn/agent-plans--refuse-refile-out)
        (advice-add 'hack-local-variables :around #'wgn/agent-plans--record-locals)
        (advice-add 'set-auto-mode :around #'wgn/agent-plans--record-locals)
        (dolist (command wgn/agent-plans--agenda-edits)
          (advice-add command :around #'wgn/agent-plans--agenda-guard))
        (wgn/agent-plans--protect-all))
    (dolist (command wgn/agent-plans--agenda-edits)
      (advice-remove command #'wgn/agent-plans--agenda-guard))
    (advice-remove 'auto-revert-handler #'wgn/agent-plans--around-auto-revert)
    (advice-remove 'after-find-file #'wgn/agent-plans--note-unknown-base)
    (advice-remove 'make-indirect-buffer #'wgn/agent-plans--protect-indirect)
    (advice-remove 'write-region #'wgn/agent-plans--guard-region-write)
    (advice-remove 'copy-file #'wgn/agent-plans--guard-file-copy)
    (advice-remove 'rename-file #'wgn/agent-plans--guard-file-copy)
    (advice-remove 'write-file #'wgn/agent-plans--undo-refused-rename)
    (advice-remove 'save-some-buffers #'wgn/agent-plans--save-all)
    (advice-remove 'basic-save-buffer #'wgn/agent-plans--skip-refused)
    (remove-hook 'kill-emacs-query-functions #'wgn/agent-plans--confirm-exit)
    (advice-remove 'set-visited-file-name #'wgn/agent-plans--refuse-visit)
    (advice-remove 'org-archive-subtree #'wgn/agent-plans--refuse-move)
    (advice-remove 'org-refile #'wgn/agent-plans--refuse-refile-out)
    (advice-remove 'hack-local-variables #'wgn/agent-plans--record-locals)
    (advice-remove 'set-auto-mode #'wgn/agent-plans--record-locals)
    (when wgn/agent-plans--has-restore-functions
      (remove-hook 'revert-buffer-restore-functions #'wgn/agent-plans--restore-lock))
    (advice-remove 'revert-buffer #'wgn/agent-plans--around-revert)
    (remove-hook 'read-only-mode-hook #'wgn/agent-plans--on-read-only)
    (remove-hook 'write-file-functions #'wgn/agent-plans--guard-write)
    (remove-hook 'org-blocker-hook #'wgn/agent-plans-blocker)
    (remove-hook 'find-file-hook #'wgn/agent-plans--protect-visited)
    (remove-hook 'org-mode-hook #'wgn/agent-plans--protect-file)
    (remove-hook 'before-revert-hook #'wgn/agent-plans--before-revert)
    (remove-hook 'after-revert-hook #'wgn/agent-plans--after-revert)
    (remove-hook 'after-revert-hook #'wgn/agent-plans--note-read)
    (remove-hook 'after-save-hook #'wgn/agent-plans--after-save)
    (remove-hook 'after-set-visited-file-name-hook #'wgn/agent-plans--revisit)
    (wgn/agent-plans--unprotect-all)))

;;;; Calling agent-plan

(defun wgn/agent-plans--call (&rest args)
  "Run `agent-plan' with ARGS and return its output.
Its refusal, on standard error, is signalled as a `user-error'."
  (unless (executable-find "agent-plan")
    (user-error "agent-plan is not on `exec-path'"))
  (let ((stderr (make-temp-file "agent-plan-stderr"))
        (default-directory (let ((dir (wgn/agent-plans-directory)))
                             (if (and dir (file-directory-p dir))
                                 (file-name-as-directory dir)
                               (expand-file-name "~/")))))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process "agent-plan" nil (list t stderr) nil args)))
            (unless (eq status 0)
              (user-error "%s" (string-trim
                                (with-temp-buffer
                                  (insert-file-contents stderr)
                                  (if (zerop (buffer-size))
                                      (format "agent-plan %s exited with %s" (car args) status)
                                    (buffer-string))))))
            (buffer-string)))
      (delete-file stderr))))

;;;; What waits on the operator
;;
;; Two things in a plan wait on the operator in a form a program can
;; read: an open question, which `agent-plan questions' lists, and a
;; sign-pass an agent handed over, which `agent-plan sign-id' logs as
;; sign-requested and `git sign-pass --notify' settles with a result
;; event of the same SIGN.

(defconst wgn/agent-plans--sign-results '("signed" "sign-refused" "sign-failed")
  "The kinds of the event that ends a sign-pass.")

(defun wgn/agent-plans--log-events (log)
  "Return the events of the log file LOG, oldest first.
Each is an alist from a drawer property's name to its value."
  (when (file-readable-p log)
    (with-temp-buffer
      (insert-file-contents log)
      (goto-char (point-min))
      (let (events)
        (while (re-search-forward "^\\* " nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-max))))
                properties)
            (while (re-search-forward "^:\\([A-Z_]+\\):[ \t]*\\(.*?\\)[ \t]*$" end t)
              (unless (member (match-string 1) '("PROPERTIES" "END"))
                (push (cons (match-string 1) (match-string 2)) properties)))
            (push properties events)
            (goto-char end)))
        (nreverse events)))))

(defun wgn/agent-plans--plan-property (plan name)
  "Return the words of property NAME of PLAN's \"* Plan\" entry."
  (with-temp-buffer
    (insert-file-contents plan)
    (goto-char (point-min))
    (when (re-search-forward (format "^:%s:[ \t]*\\(.*\\)$" (regexp-quote name)) nil t)
      (split-string (match-string 1)))))

(defun wgn/agent-plans--log-file (plan)
  "Return the log file of the plan file PLAN."
  (concat (file-name-sans-extension plan) ".log.org"))

(defun wgn/agent-plans-pending-signs (plan)
  "Return the sign-passes handed over in PLAN that have no result yet.
Each is a plist of the pass's :id, its :branches, and the :command and
:directory to run it with.  The pass's own event names them: BRANCH,
WORKTREE where `git sign-pass' runs, and BASE for its --base; :directory
is nil when the event names no worktree that exists."
  (let* ((events (wgn/agent-plans--log-events (wgn/agent-plans--log-file plan)))
         (finished (delq nil (mapcar (lambda (event)
                                       (and (member (cdr (assoc "KIND" event))
                                                    wgn/agent-plans--sign-results)
                                            (cdr (assoc "SIGN" event))))
                                     events))))
    (delq nil
          (mapcar
           (lambda (event)
             (let ((id (cdr (assoc "SIGN" event))))
               (when (and (equal (cdr (assoc "KIND" event)) "sign-requested")
                          id (not (member id finished)))
                 (let ((branches (split-string (or (cdr (assoc "BRANCH" event)) "") "," t))
                       (worktree (cdr (assoc "WORKTREE" event)))
                       (base (cdr (assoc "BASE" event))))
                   (list :id id :branches branches
                         :directory (and worktree (file-directory-p worktree)
                                         (file-name-as-directory (expand-file-name worktree)))
                         :command (mapconcat
                                   #'shell-quote-argument
                                   (append '("git" "sign-pass")
                                           (and base (list "--base" base))
                                           (list "--notify" id)
                                           branches)
                                   " "))))))
           events))))

(defvar wgn/agent-plans--questions-cache (make-hash-table :test #'equal)
  "Map a plan file to (STAMP . QUESTIONS), `agent-plan questions' at STAMP.
STAMP is the modification times of the plan's and its parts' files.")

(defun wgn/agent-plans--open-questions (plan)
  "Return the open questions of PLAN and its parts, from `agent-plan questions'.
Its answer is kept until one of their files changes."
  (let ((stamp (mapcar (lambda (file)
                         (cons file (file-attribute-modification-time (file-attributes file))))
                       (wgn/agent-plans--own-files plan)))
        (cached (gethash plan wgn/agent-plans--questions-cache)))
    (if (equal (car cached) stamp)
        (cdr cached)
      (let ((questions (append (json-parse-string
                                (wgn/agent-plans--call "questions" plan "--all" "--json"))
                               nil)))
        (puthash plan (cons stamp questions) wgn/agent-plans--questions-cache)
        questions))))

(defun wgn/agent-plans--id-marker (file id)
  "Return a marker at the heading whose ID is ID in FILE, or nil."
  (when (and (stringp file) (file-readable-p file))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward (wgn/agent-plans--id-line id) nil t)
            (org-back-to-heading t)
            (point-marker)))))))

(defun wgn/agent-plans-waiting ()
  "Return what waits on the operator in this host's plans, newest plan first.
Each is a plist of its :plan file, :kind (`question', `command' or
`error'), :id, :title and the :command it shows.  A question has the
:file of its entry; a command its :directory, or nil; an error is a
plan whose questions `agent-plan' did not give.  A halted plan waits on
nothing."
  (mapcan
   (lambda (plan)
     (let ((name (file-name-base plan)))
       (append
        (condition-case err
            (mapcar
             (lambda (question)
               (let ((qid (gethash "id" question)))
                 (list :plan plan :kind 'question :id qid :file (gethash "file" question)
                       :title (gethash "question" question)
                       :recommendation (gethash "recommendation" question)
                       :command (format "answer it with /questions in a Claude session on %s"
                                        name))))
             (wgn/agent-plans--open-questions plan))
          (error (list (list :plan plan :kind 'error :id "" :title (error-message-string err)
                             :command ""))))
        (mapcar (lambda (sign)
                  (list :plan plan :kind 'command :id (plist-get sign :id)
                        :title (concat "sign-pass of " (string-join (plist-get sign :branches) " ")
                                       ", in " (if (plist-get sign :directory)
                                                   (abbreviate-file-name (plist-get sign :directory))
                                                 "a worktree its event does not name"))
                        :command (plist-get sign :command)
                        :directory (plist-get sign :directory)))
                (wgn/agent-plans-pending-signs plan)))))
   (seq-remove (lambda (plan)
                 (or (wgn/agent-plans--mirror-host plan)
                     (member "halted" (wgn/agent-plans--plan-property plan "STATUS"))))
               (wgn/agent-plans-files))))

(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-agenda-prepare "org-agenda" (&optional name))

(defun wgn/agent-plans-waiting-visit ()
  "Visit the plan entry of the \"Waiting on you\" line at point."
  (interactive)
  (let* ((item (or (get-text-property (point) 'wgn/agent-plans-item)
                   (user-error "Nothing waits on you here")))
         (marker (and (eq (plist-get item :kind) 'question)
                      (wgn/agent-plans--id-marker (plist-get item :file) (plist-get item :id)))))
    (if (null marker)
        (find-file (plist-get item :plan))
      (pop-to-buffer-same-window (marker-buffer marker))
      (widen)
      (goto-char marker)
      (org-fold-show-context 'agenda))))

(defvar wgn/agent-plans-waiting-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'wgn/agent-plans-waiting-visit)
    ;; The agenda's would open any link in the line's text, which agents wrote.
    (define-key map (kbd "C-c C-o") #'wgn/agent-plans-waiting-visit)
    map)
  "Keys of the lines of the \"Waiting on you\" agenda block.
The lines carry no entry marker, so the agenda's own commands, which
would edit the plan, find nothing to act on there.")

(defun wgn/agent-plans-agenda-waiting (&optional _match)
  "Insert the \"Waiting on you\" block of the agenda: `wgn/agent-plans-waiting'.
It is a list to read: each line shows what to run, and where, as text;
questions are answered with /questions in a Claude session, and
\\<wgn/agent-plans-waiting-map>\\[wgn/agent-plans-waiting-visit] visits the entry."
  (org-agenda-prepare "Waiting on you")
  (insert (propertize "Waiting on you" 'face 'org-agenda-structure) "\n")
  (let ((items (wgn/agent-plans-waiting)))
    (cond
     ((not (wgn/agent-plans-directory))
      (insert "  " (wgn/agent-plans--missing-config "dir") "\n"))
     ((not items)
      (insert "  nothing\n")))
    (dolist (item items)
      (let ((start (point)))
        (insert (format "  %s %s %s%s\n"
                        (file-name-base (plist-get item :plan))
                        (plist-get item :id)
                        (plist-get item :title)
                        (if (plist-get item :recommendation)
                            (format "  (recommended: %s)" (plist-get item :recommendation))
                          "")))
        (unless (string-empty-p (plist-get item :command))
          (insert (propertize (concat "    " (plist-get item :command)) 'face 'shadow) "\n"))
        (add-text-properties
         start (point)
         `(wgn/agent-plans-item ,item
           keymap ,wgn/agent-plans-waiting-map
           help-echo ,(substitute-command-keys
                       "\\<wgn/agent-plans-waiting-map>\\[wgn/agent-plans-waiting-visit]: visit")))))))

;;;; Links
;;
;;   plan:PLAN[#ITEM]       a plan by its ID, at the entry whose ID is ITEM
;;   q:[PLAN#]QID           a question, by default of the plan link is in
;;   branch:REPO:BRANCH     a branch's log, in magit
;;   wt:PATH                a worktree's status, in magit
;;   pr:REPO#N              a pull request, in forge, else in a browser
;;   commit:REPO:SHA        a commit, in magit
;;   file:PATH::LINE@SHA    a citation: Org's own file link, at LINE, with a
;;                          warning where the working tree is not at SHA
;;
;; REPO is a repository's directory, or its name under
;; `wgn/agent-plans-repository-directories'.

(defcustom wgn/agent-plans-repository-directories '("~/git/github.com/*")
  "Wildcard patterns of the directories that hold repositories.
A link names a repository by its directory's name under one of them, or
as OWNER/NAME, where OWNER is the name of the directory holding it."
  :type '(repeat string))

(defun wgn/agent-plans--need (function feature)
  "Load FEATURE unless FUNCTION is defined, and refuse if it still is not."
  (unless (fboundp function)
    (require feature nil t))
  (unless (fboundp function)
    (user-error "%s is not available" feature)))

(defun wgn/agent-plans--repository (repo &optional noerror)
  "Return the directory of the repository REPO names.
Ask which one when several match.  NOERROR returns nil for no match."
  (if (or (file-name-absolute-p repo) (string-prefix-p "~" repo))
      (if (file-directory-p (expand-file-name repo))
          (file-name-as-directory (expand-file-name repo))
        (unless noerror (user-error "No repository at %s" repo)))
    (let* ((name (file-name-nondirectory repo))
           (owner (and (string-match-p "/" repo)
                       (directory-file-name (file-name-directory repo))))
           (found (delete-dups
                   (seq-filter
                    #'file-directory-p
                    (mapcan
                     (lambda (pattern)
                       (mapcar (lambda (parent)
                                 (file-name-as-directory (expand-file-name name parent)))
                               (seq-filter
                                (lambda (parent)
                                  (or (null owner)
                                      (equal (file-name-nondirectory (directory-file-name parent))
                                             owner)))
                                (file-expand-wildcards (expand-file-name pattern)))))
                     (copy-sequence wgn/agent-plans-repository-directories))))))
      (cond
       ((cdr found) (completing-read (format "Repository %s: " repo) found nil t))
       (found (car found))
       (noerror nil)
       (t (user-error "No repository %s under `wgn/agent-plans-repository-directories'" repo))))))

(defun wgn/agent-plans--split (path separator what)
  "Split link PATH at the first SEPARATOR into two non-empty parts.
WHAT names the parts, for the refusal."
  (let ((at (string-search separator path)))
    (unless (and at (> at 0) (< (1+ at) (length path)))
      (user-error "Not %s: %s" what path))
    (cons (substring path 0 at) (substring path (1+ at)))))

(defun wgn/agent-plans--plan-file (file)
  "Return the plan's own file of FILE, a plan's, part's or log's file."
  (let ((parts (wgn/agent-plans--file-parts file)))
    (when parts
      (expand-file-name (concat (nth 0 parts) "." (nth 1 parts) ".org")
                        (wgn/agent-plans-directory)))))

(defun wgn/agent-plans--by-id (id)
  "Return the plan file of the plan whose ID is ID."
  (let ((file (expand-file-name (concat id ".org") (wgn/agent-plans--require-directory))))
    (unless (and (wgn/agent-plans--file-parts file)
                 (null (nth 2 (wgn/agent-plans--file-parts file)))
                 (file-exists-p file))
      (user-error "No plan %s under %s" id (wgn/agent-plans-directory)))
    file))

(defun wgn/agent-plans-visit (plan &optional item)
  "Visit the plan file PLAN, at the entry whose ID is ITEM in it or its parts."
  (let ((marker (and item
                     (seq-some (lambda (file) (wgn/agent-plans--id-marker file item))
                               (wgn/agent-plans--own-files plan)))))
    (when (and item (null marker))
      (user-error "%s holds no %s" (file-name-base plan) item))
    (if (null marker)
        (find-file plan)
      (pop-to-buffer-same-window (marker-buffer marker))
      (widen)
      (goto-char marker)
      (org-fold-show-context 'link-search))))

(defun wgn/agent-plans--follow-plan (path _arg)
  "Follow a plan: link to PATH, PLAN or PLAN#ITEM."
  (let ((at (string-search "#" path)))
    (wgn/agent-plans-visit (wgn/agent-plans--by-id (if at (substring path 0 at) path))
                           (and at (substring path (1+ at))))))

(defun wgn/agent-plans--holding (id)
  "Return the plan files whose plan holds an entry whose ID is ID."
  (let ((pattern (wgn/agent-plans--id-line id)))
    (seq-filter (lambda (plan)
                  (seq-some (lambda (file)
                              (with-temp-buffer
                                (insert-file-contents file)
                                (re-search-forward pattern nil t)))
                            (wgn/agent-plans--own-files plan)))
                (wgn/agent-plans-files))))

(defun wgn/agent-plans--follow-question (path _arg)
  "Follow a q: link to PATH, QID or PLAN#QID.
A bare QID is the question of the plan the link is in, else of the plan
that holds it, asking which when several do."
  (let* ((at (string-search "#" path))
         (qid (if at (substring path (1+ at)) path))
         (here (wgn/agent-plans--plan-file (buffer-file-name (buffer-base-buffer))))
         (plan (cond
                (at (wgn/agent-plans--by-id (substring path 0 at)))
                (here here)
                (t (let ((plans (progn (wgn/agent-plans--require-directory)
                                       (wgn/agent-plans--holding qid))))
                     (cond
                      ((cdr plans) (wgn/agent-plans--read-holder qid plans))
                      (plans (car plans))
                      (t (user-error "No plan holds %s" qid))))))))
    (wgn/agent-plans-visit plan qid)))

(defun wgn/agent-plans--title (plan)
  "Return the #+title of the plan file PLAN, or its name."
  (with-temp-buffer
    (insert-file-contents plan)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title:[ \t]*\\(.+?\\)[ \t]*$" nil t)
        (match-string 1)
      (file-name-base plan))))

(defun wgn/agent-plans--read-holder (qid plans)
  "Ask which of PLANS, the plan files holding QID, is meant.
They are offered by title, those where QID is open first."
  (let ((candidates
         (mapcar (lambda (entry) (cons (cdr entry) (car entry)))
                 (sort (mapcar (lambda (plan)
                                 (let ((open (not (wgn/agent-plans--settled-p plan qid))))
                                   (cons plan
                                         (format "%s (%s): %s" (wgn/agent-plans--title plan)
                                                 (file-name-base plan)
                                                 (if open "open" "settled")))))
                               plans)
                       (lambda (a b)
                         (and (string-suffix-p ": open" (cdr a))
                              (not (string-suffix-p ": open" (cdr b)))))))))
    (cdr (assoc (completing-read
                 (format "%s of plan: " qid)
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity))
                     (complete-with-action action candidates string predicate)))
                 nil t)
                candidates))))

(declare-function magit-status "magit-status" (&optional directory cache))
(declare-function magit-log-other "magit-log" (revs &optional args files))
(declare-function magit-show-commit "magit-diff" (rev &optional args files module))
(declare-function forge-get-pullreq "forge-pullreq")
(declare-function forge-topic-setup-buffer "forge-topic" (topic))
(declare-function org-export-data-with-backend "ox" (data backend info))
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(declare-function org-element-create "org-element-ast" (type &optional props &rest children))

(defun wgn/agent-plans--follow-branch (path _arg)
  "Follow a branch: link to PATH, REPO:BRANCH, showing its log in magit."
  (let* ((parts (wgn/agent-plans--split path ":" "REPO:BRANCH"))
         (default-directory (wgn/agent-plans--repository (car parts))))
    (wgn/agent-plans--need 'magit-log-other 'magit)
    (magit-log-other (list (cdr parts)))))

(defun wgn/agent-plans--follow-worktree (path _arg)
  "Follow a wt: link to PATH, showing the worktree's status in magit."
  (unless (file-directory-p (expand-file-name path))
    (user-error "No worktree at %s" path))
  (wgn/agent-plans--need 'magit-status 'magit)
  (magit-status (file-name-as-directory (expand-file-name path))))

(defun wgn/agent-plans--follow-commit (path _arg)
  "Follow a commit: link to PATH, REPO:SHA, showing the commit in magit."
  (let* ((parts (wgn/agent-plans--split path ":" "REPO:SHA"))
         (default-directory (wgn/agent-plans--repository (car parts))))
    (wgn/agent-plans--need 'magit-show-commit 'magit)
    (magit-show-commit (cdr parts))))

(defun wgn/agent-plans--git (directory &rest args)
  "Return the trimmed output of git ARGS in DIRECTORY, or nil if it fails."
  (let ((default-directory directory))
    (with-temp-buffer
      (and (eq 0 (apply #'call-process "git" nil (list t nil) nil args))
           (string-trim (buffer-string))))))

(defun wgn/agent-plans--pr-url (repo number directory)
  "Return the web URL of pull request NUMBER of REPO, cloned in DIRECTORY.
The repository is origin's on GitHub, or REPO itself as OWNER/NAME."
  (let* ((remote (and directory (wgn/agent-plans--git directory "remote" "get-url" "origin")))
         (slug (or (and remote
                        (string-match
                         "github\\.com[:/]\\([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+?\\)\\(?:\\.git\\)?/?\\'"
                         remote)
                        (match-string 1 remote))
                   (and (string-match-p "\\`[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+\\'" repo) repo))))
    (and slug (format "https://github.com/%s/pull/%s" slug number))))

(defun wgn/agent-plans--pr (path)
  "Return (REPO NUMBER DIRECTORY) of a pr: link to PATH, REPO#N."
  (let ((parts (wgn/agent-plans--split path "#" "REPO#N")))
    (unless (string-match-p "\\`[0-9]+\\'" (cdr parts))
      (user-error "Not a pull request number: %s" (cdr parts)))
    (list (car parts) (string-to-number (cdr parts))
          (wgn/agent-plans--repository (car parts) t))))

(defun wgn/agent-plans--follow-pr (path _arg)
  "Follow a pr: link to PATH, REPO#N: in forge where it has the pull
request, else in a browser."
  (pcase-let* ((`(,repo ,number ,directory) (wgn/agent-plans--pr path))
               (topic (and directory
                           (progn (wgn/agent-plans--need 'forge-get-pullreq 'forge) t)
                           (let ((default-directory directory))
                             (ignore-errors (forge-get-pullreq number))))))
    (if topic
        (forge-topic-setup-buffer topic)
      (browse-url (or (wgn/agent-plans--pr-url repo number directory)
                      (user-error "No web address for %s" path))))))

(defun wgn/agent-plans--exporter (type)
  "Return the :export function of the link TYPE.
A pr: link becomes a link to its web address; any other the link's
description, else TYPE:PATH as verbatim text.  The description arrives
exported already; TYPE:PATH is exported by BACKEND's own verbatim
transcoder, which escapes it.  The address holds only characters no
backend escapes."
  (lambda (path description backend info)
    (let* ((url (and (equal type "pr")
                     (ignore-errors
                       (pcase-let ((`(,repo ,number ,directory) (wgn/agent-plans--pr path)))
                         (wgn/agent-plans--pr-url repo number directory)))))
           (text (or description
                     (org-export-data-with-backend
                      (org-element-create 'verbatim (list :value (concat type ":" path)))
                      backend info))))
      (cond
       ((null url) text)
       ((org-export-derived-backend-p backend 'md) (format "[%s](%s)" text url))
       ((org-export-derived-backend-p backend 'html) (format "<a href=\"%s\">%s</a>" url text))
       ((org-export-derived-backend-p backend 'latex) (format "\\href{%s}{%s}" url text))
       ((org-export-derived-backend-p backend 'odt)
        (format "<text:a xlink:type=\"simple\" xlink:href=\"%s\">%s</text:a>" url text))
       (t (format "%s <%s>" text url))))))

(defun wgn/agent-plans--citation-drift (file sha)
  "Return why FILE's working tree may not show what commit SHA did, or nil."
  (let* ((directory (file-name-directory file))
         (head (wgn/agent-plans--git directory "rev-parse" "HEAD"))
         (cited (wgn/agent-plans--git directory "rev-parse" "--verify" "--quiet"
                                      (concat sha "^{commit}"))))
    (cond
     ((null head) nil)
     ((null cited) (format "%s is not a commit of the repository of %s" sha file))
     ((equal head cited) nil)
     (t (format "%s is cited at %s, but the working tree is at %s, and the file %s"
                (file-name-nondirectory file) (substring cited 0 12) (substring head 0 12)
                (if (wgn/agent-plans--git directory "diff" "--quiet" cited "--" file)
                    "is the same there"
                  "differs, so the line may have moved"))))))

(defun wgn/agent-plans--file-search (search)
  "Go to the line of a citation's SEARCH, LINE@SHA, and return t.
Warn where the working tree is not at SHA.  Return nil for any other
SEARCH, as `org-execute-file-search-functions' expects."
  (when (string-match "\\`\\([0-9]+\\)@\\([0-9a-fA-F]\\{4,40\\}\\)\\'" search)
    (let ((line (string-to-number (match-string 1 search)))
          (sha (match-string 2 search)))
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (let ((drift (and buffer-file-name
                        (wgn/agent-plans--citation-drift buffer-file-name sha))))
        (when drift
          (display-warning 'wgn/agent-plans drift)))
      t)))

(defvar org-execute-file-search-functions)
(declare-function org-link-set-parameters "ol" (type &rest parameters))

;;;###autoload
(defun wgn/agent-plans-links-setup ()
  "Register the plan link types and the LINE@SHA search of file links."
  (dolist (link `(("plan" ,#'wgn/agent-plans--follow-plan)
                  ("q" ,#'wgn/agent-plans--follow-question)
                  ("branch" ,#'wgn/agent-plans--follow-branch)
                  ("wt" ,#'wgn/agent-plans--follow-worktree)
                  ("pr" ,#'wgn/agent-plans--follow-pr)
                  ("commit" ,#'wgn/agent-plans--follow-commit)))
    (org-link-set-parameters (car link)
                             :follow (cadr link)
                             :export (wgn/agent-plans--exporter (car link))))
  (add-hook 'org-execute-file-search-functions #'wgn/agent-plans--file-search))

;;;; Org-roam
;;
;; The plans directory is inside the operator's org-roam notes
;; repository.  A plan's own file is a node; its parts and its log are
;; not, nor are the lock files `agent-plan' keeps beside them.

(defvar org-roam-file-exclude-regexp)
(defvar org-roam-db-node-include-function)
(defvar org-roam-directory)
(defvar org-roam-db-autosync-mode)
(declare-function org-roam-db-autosync-mode "org-roam-db" (&optional arg))
(declare-function org-roam-file-p "org-roam" (&optional file))
(declare-function org-roam-db-update-file "org-roam-db" (&optional file-path _deprecated-arg))
(declare-function org-roam-db-clear-file "org-roam-db" (&optional file))

(defconst wgn/agent-plans-org-roam-exclude-regexp
  (concat "\\(?:\\`\\|/\\)" wgn/agent-plans--name-regexp
          "\\.\\(?:" wgn/agent-plans--suffix-regexp "\\)\\.org\\'"
          "\\|\\.lock\\'")
  "Match a plan's part, log or lock file, by its name under `org-roam-directory'.")

(defun wgn/agent-plans-org-roam-node-p ()
  "Return nil at an entry of a plan's file, else t.
For `org-roam-db-node-include-function', which org-roam calls at each
heading with an ID and at the start of each file: a plan's file is one
node, at its file-level ID.  Its entries' IDs, the same S1 and Q1 in
every plan, are no nodes, nor is an ID org-id gives an entry."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (not (and file
              (string-match-p wgn/agent-plans--file-regexp (file-name-nondirectory file))
              (not (org-before-first-heading-p))))))

(defvar wgn/agent-plans-org-roam-directory nil
  "The notes repository that is org-roam's directory, or nil for none.
Home Manager's home.programs.wgn.emacs.org-roam.directory sets it, in
the file `wgn/agent-plans-org-roam-machine-file' names.")

(defconst wgn/agent-plans-org-roam-machine-file "etc/org-roam-notes.el"
  "The file, under `user-emacs-directory', that Home Manager writes.
It sets `wgn/agent-plans-org-roam-directory'.")

(defvar wgn/agent-plans--org-roam-watch nil
  "The file notification watch on the plans directory, or nil.
`wgn/agent-plans-org-roam-setup' adds it, removing the one before.")

(defun wgn/agent-plans--org-roam-written (file)
  "Index FILE, written outside Emacs, in org-roam's database.
A buffer visiting FILE is reverted first if it has no unsaved changes,
since org-roam reads a visited file from its buffer.  One with unsaved
changes is left alone, and so is the database: org-roam would save the
buffer over FILE.  The database takes it when the buffer is saved."
  (let ((buffer (find-buffer-visiting file)))
    (unless (and buffer (buffer-modified-p buffer))
      (when (and buffer (not (verify-visited-file-modtime buffer)))
        (with-current-buffer buffer
          (revert-buffer t t t)))
      (condition-case err
          (org-roam-db-update-file file)
        (error (message "org-roam could not index %s: %s"
                        file (error-message-string err)))))))

(defun wgn/agent-plans--org-roam-file-changed (event)
  "Keep org-roam's database in step with EVENT in the plans directory.
EVENT is a file notification, (DESCRIPTOR ACTION FILE [FILE1]).
`org-roam-db-autosync-mode' sees only what Emacs writes, and
`agent-plan' writes plans from outside, each by a temporary file
renamed into place.  Only files `org-roam-file-p' takes are touched.
An error is reported, not signalled: it would reach whatever was
reading input when the event arrived."
  (condition-case err
      (wgn/agent-plans--org-roam-handle event)
    (error (message "org-roam missed %s in the plans directory: %s"
                    (nth 2 event) (error-message-string err)))))

(defun wgn/agent-plans--org-roam-handle (event)
  "Do what `wgn/agent-plans--org-roam-file-changed' says for EVENT."
  (pcase-let ((`(,descriptor ,action ,file ,file1) event))
    (pcase action
      ('stopped
       (when (equal descriptor wgn/agent-plans--org-roam-watch)
         (setq wgn/agent-plans--org-roam-watch nil)))
      ((or 'created 'changed 'renamed)
       (when (and (eq action 'renamed) (org-roam-file-p file))
         (org-roam-db-clear-file file))
       (let ((written (if (eq action 'renamed) file1 file)))
         (when (and (org-roam-file-p written) (file-exists-p written))
           (wgn/agent-plans--org-roam-written written))))
      ('deleted
       (when (org-roam-file-p file)
         (org-roam-db-clear-file file))))))

(defun wgn/agent-plans--org-roam-watch-plans ()
  "Watch the plans directory for `agent-plan''s writes, if there is one.
Any earlier watch is removed first.  There is none to add while
`org-roam-db-autosync-mode' is off, which keeps nothing in step."
  (when wgn/agent-plans--org-roam-watch
    (file-notify-rm-watch wgn/agent-plans--org-roam-watch)
    (setq wgn/agent-plans--org-roam-watch nil))
  (let ((dir (wgn/agent-plans-directory)))
    (when (and dir org-roam-db-autosync-mode (file-directory-p dir))
      (condition-case err
          (setq wgn/agent-plans--org-roam-watch
                (file-notify-add-watch (file-truename dir) '(change)
                                       #'wgn/agent-plans--org-roam-file-changed))
        (file-notify-error
         (message "Plans written outside Emacs reach org-roam only on its next sync: %s"
                  (error-message-string err)))))))

(defun wgn/agent-plans-org-roam-setup ()
  "Make `wgn/agent-plans-org-roam-directory' org-roam's, each plan one node.
Plans' parts, logs and lock files are left out of org-roam.  If the
directory exists, `org-roam-db-autosync-mode' keeps the database in
step with it, syncing it now, and a file notification watch on the
plans directory, `wgn/agent-plans-directory', indexes the plans
`agent-plan' writes from outside Emacs.  Unless the variable is an
absolute path, one starting with /, nothing is set up: \"\" would be
Emacs's current directory.

Call this once org-roam is loaded: it adds to org-roam's own exclusions,
which may be one regexp or a list of them, and puts
`wgn/agent-plans-org-roam-node-p' before the function
`org-roam-db-node-include-function' holds, which then decides only
where that one lets it.  A later `setq' of that variable drops it, so
set your own filter before this runs, or add yours with
`add-function'.  Calling it again changes nothing."
  (let ((repo wgn/agent-plans-org-roam-directory))
    (when (and (stringp repo) (string-prefix-p "/" repo))
      (setq org-roam-directory (file-truename repo))
      (let ((exclusions (ensure-list org-roam-file-exclude-regexp)))
        (unless (member wgn/agent-plans-org-roam-exclude-regexp exclusions)
          (setq org-roam-file-exclude-regexp
                (cons wgn/agent-plans-org-roam-exclude-regexp exclusions))))
      (add-function :before-while org-roam-db-node-include-function
                    #'wgn/agent-plans-org-roam-node-p)
      (when (and (file-directory-p org-roam-directory)
                 (not org-roam-db-autosync-mode))
        (org-roam-db-autosync-mode 1))
      (wgn/agent-plans--org-roam-watch-plans))))

;;;; Capture
;;
;; A capture is written to a scratch buffer, never to a plan file; its
;; finalisation turns the entry into an `agent-plan' command, and a
;; refusal leaves the capture open to correct.

(defun wgn/agent-plans--refresh (plan)
  "Show what `agent-plan' just wrote to PLAN: revert the buffers of its files.
The mode line's hint is asked for again."
  (let ((own (wgn/agent-plans--plan-file plan)))
    (dolist (buffer (buffer-list))
      (let ((file (buffer-file-name buffer)))
        (when (and file
                   (equal (wgn/agent-plans--plan-file file) own)
                   (not (buffer-modified-p buffer)))
          (with-current-buffer buffer
            (revert-buffer t t t))))))
  (clrhash wgn/agent-plans-questions--cache))

(defun wgn/agent-plans--item-here ()
  "Return (FILE . ID) of the plan entry at point in a plan buffer, or nil."
  (let ((file (buffer-file-name (buffer-base-buffer))))
    (when (and (derived-mode-p 'org-mode)
               (wgn/agent-plans--file-parts file)
               (not (org-before-first-heading-p)))
      (save-excursion
        (org-back-to-heading t)
        (let ((id (wgn/agent-plans--entry-id)))
          (and id (cons file id)))))))

(defun wgn/agent-plans--item-at-point ()
  "Return (FILE . ID) of the plan entry at point, in a plan buffer or the agenda."
  (let ((waiting (get-text-property (point) 'wgn/agent-plans-item))
        (marker (and (derived-mode-p 'org-agenda-mode)
                     (or (get-text-property (line-beginning-position) 'org-hd-marker)
                         (get-text-property (line-beginning-position) 'org-marker)))))
    (cond
     ((eq (plist-get waiting :kind) 'question)
      (cons (plist-get waiting :file) (plist-get waiting :id)))
     ((markerp marker)
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char marker)
            (wgn/agent-plans--item-here)))))
     (t (wgn/agent-plans--item-here)))))

(declare-function org-capture-get "org-capture" (prop &optional local))
(declare-function org-capture-put "org-capture" (&rest elements))
(declare-function org-heading-components "org" ())
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function org-entry-properties "org" (&optional epom which))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-at-heading-p "org" (&optional invisible-not-ok))
(defvar org-note-abort)
(defvar org-capture-is-refiling)
(defvar org-capture-bookmark)

(defun wgn/agent-plans--capture-plan (kind)
  "Choose the plan a capture of KIND, `followup' or `ask', is for.
The plan and part of the entry at point where the capture started, else
a plan asked for; another host's plan is refused.  Org fills a template
in that buffer, before its prompts and its target, so this runs from the
template.  Return the entry's ID, or nil."
  (let* ((item (wgn/agent-plans--item-at-point))
         (file (or (car item)
                   (let ((visited (buffer-file-name (buffer-base-buffer))))
                     (and (wgn/agent-plans--file-parts visited) visited))))
         (plan (if file (wgn/agent-plans--plan-file file) (wgn/agent-plans--read)))
         (part (let ((name (nth 2 (wgn/agent-plans--file-parts file))))
                 (and name (not (equal name "log")) name))))
    (when (wgn/agent-plans--mirror-host plan)
      (user-error "%s is %s's plan; capture for it there"
                  (file-name-base plan) (wgn/agent-plans--mirror-host plan)))
    (org-capture-put :agent-plan plan :agent-part part :agent-kind kind)
    (cdr item)))

(defun wgn/agent-plans--capture-target ()
  "Go to a new scratch buffer for the capture to be written to.
Each capture has its own, so captures open at once keep their text."
  (let ((buffer (generate-new-buffer " *agent-plan capture*")))
    (org-capture-put :agent-buffer buffer)
    (set-buffer buffer)
    (org-mode)
    ;; A bookmark of the scratch buffer is useless, and Emacs 29 fails
    ;; to make one of a buffer that visits no file.
    (with-no-warnings (setq-local org-capture-bookmark nil))))

(defun wgn/agent-plans--one-line (lines)
  "Return LINES, a body's, as the one line `agent-plan' takes, or nil.
Lines of a paragraph are joined by spaces, and paragraphs by \" ¶ \"."
  (let ((paragraphs (seq-remove
                     #'string-empty-p
                     (mapcar (lambda (paragraph)
                               (string-join (split-string paragraph "\n" t "[ \t]+") " "))
                             (split-string (string-join lines "\n") "\n[ \t]*\n" t)))))
    (and paragraphs (string-join paragraphs " ¶ "))))

(defun wgn/agent-plans--captured (&optional alternatives)
  "Return the captured entry as a plist of :title, :priority, :properties,
:context, its body as one line, and with ALTERNATIVES, :alternatives, the
body's list items, which the context then leaves out.  What is indented
under an item stays with it: its text after a space, its own items
after \": \" and \"; \"."
  (save-excursion
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (user-error "The capture holds no entry"))
    (let* ((components (org-heading-components))
           (properties (org-entry-properties nil 'standard))
           context items indent nested)
      (org-end-of-meta-data t)
      (dolist (line (split-string (buffer-substring-no-properties (point) (point-max)) "\n"))
        (cond
         ((and alternatives
               (string-match "\\`\\([ \t]*\\)[-+] +\\(.+?\\)[ \t]*\\'" line)
               (or (null indent) (<= (length (match-string 1 line)) indent)))
          (setq indent (length (match-string 1 line))
                nested nil)
          (push (match-string 2 line) items))
         ((and indent
               (string-match "\\`\\([ \t]*\\)\\(\\(?:[-+*]\\|[0-9]+[.)]\\) +\\)?\\(.+?\\)[ \t]*\\'"
                             line)
               (> (length (match-string 1 line)) indent))
          (setcar items (concat (car items)
                                (cond ((not (match-string 2 line)) " ")
                                      (nested "; ")
                                      (t ": "))
                                (match-string 3 line)))
          (when (match-string 2 line)
            (setq nested t)))
         (t
          (setq indent nil)
          (push line context))))
      (list :title (or (nth 4 components) "")
            :priority (and (nth 3 components) (char-to-string (nth 3 components)))
            :properties properties
            :context (wgn/agent-plans--one-line (nreverse context))
            :alternatives (nreverse items)))))

(defun wgn/agent-plans--capture-header ()
  "Say in a capture's header line what `agent-plan' will take of its body.
For `org-capture-mode-hook'."
  (pcase (org-capture-get :agent-kind 'local)
    ('followup
     (setq header-line-format
           (concat header-line-format
                   "  The body goes to agent-plan as one line, paragraphs joined by ¶.")))
    ('ask
     (setq header-line-format
           (concat header-line-format
                   "  The body's list items become the alternatives, the rest one line,"
                   " paragraphs joined by ¶; [#A] sets the priority, :AFTER: what it waits for.")))))

(defun wgn/agent-plans--capture-run (&rest args)
  "Run the `agent-plan' subcommand ARGS for the capture's plan.
The rest of ARGS come after the plan; the part, if any, is added."
  (let ((plan (org-capture-get :agent-plan 'local))
        (part (org-capture-get :agent-part 'local)))
    (apply #'wgn/agent-plans--call (car args) plan
           (append (cdr args) (and part (list "--part" part))))
    (wgn/agent-plans--refresh plan)))

(defun wgn/agent-plans--refuse-refile ()
  "Refuse to refile a capture for `agent-plan', which would write a file."
  (when org-capture-is-refiling
    (user-error "Refiling writes the entry into a file; finish with C-c C-c, which runs agent-plan")))

(defun wgn/agent-plans--capture-followup ()
  "Record the captured follow-up with `agent-plan followup'."
  (wgn/agent-plans--refuse-refile)
  (unless org-note-abort
    (let ((entry (wgn/agent-plans--captured)))
      (apply #'wgn/agent-plans--capture-run
             "followup" "--title" (plist-get entry :title)
             "--found-in" (or (cdr (assoc "FOUND_IN" (plist-get entry :properties))) "")
             (and (plist-get entry :context) (list "--context" (plist-get entry :context)))))))

(defun wgn/agent-plans--capture-ask ()
  "Record the captured question with `agent-plan ask'."
  (wgn/agent-plans--refuse-refile)
  (unless org-note-abort
    (let* ((entry (wgn/agent-plans--captured t))
           (properties (plist-get entry :properties)))
      (apply #'wgn/agent-plans--capture-run
             "ask" "--question" (plist-get entry :title)
             "--recommendation" (or (cdr (assoc "RECOMMENDATION" properties)) "")
             (append
              (and (plist-get entry :priority) (list "--priority" (plist-get entry :priority)))
              (and (plist-get entry :context) (list "--context" (plist-get entry :context)))
              (mapcan (lambda (alternative) (list "--alternative" alternative))
                      (copy-sequence (plist-get entry :alternatives)))
              (mapcan (lambda (id) (list "--after" id))
                      (split-string (or (cdr (assoc "AFTER" properties)) ""))))))))

(defun wgn/agent-plans--capture-cleanup ()
  "Kill the buffer this capture for `agent-plan' was written to."
  (let ((buffer (org-capture-get :agent-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(defun wgn/agent-plans--followup-template ()
  "Return a follow-up's template, found in the entry the capture started at."
  (let ((item (wgn/agent-plans--capture-plan 'followup)))
    (concat "* %^{Follow-up}\n:PROPERTIES:\n"
            (if item (format ":FOUND_IN: %%^{Found in|%s}\n" item) ":FOUND_IN: %^{Found in}\n")
            ":END:\n%?")))

(defun wgn/agent-plans--ask-template ()
  "Return a question's template."
  (wgn/agent-plans--capture-plan 'ask)
  "* %^{Question}\n:PROPERTIES:\n:RECOMMENDATION: %^{Recommendation}\n:END:\n%?")

(defun wgn/agent-plans-capture-templates ()
  "Return `org-capture-templates' entries that write through `agent-plan'.
\"Af\" a follow-up, `agent-plan followup', and \"Aq\" a question,
`agent-plan ask'; plan buffers' header line names these keys.  A
question's body is its context, its list items the alternatives; [#A]
in its heading is its priority and an :AFTER: property the IDs it waits
for.  `wgn/agent-plans--capture-header', on `org-capture-mode-hook',
says so in the capture."
  (let ((common '(:no-save t :after-finalize wgn/agent-plans--capture-cleanup)))
    `(("A" "Agent plan")
      ("Af" "Follow-up" entry (function wgn/agent-plans--capture-target)
       (function wgn/agent-plans--followup-template)
       :prepare-finalize wgn/agent-plans--capture-followup ,@common)
      ("Aq" "Question" entry (function wgn/agent-plans--capture-target)
       (function wgn/agent-plans--ask-template)
       :prepare-finalize wgn/agent-plans--capture-ask ,@common))))

(provide 'wgn-agent-plans)
;;; wgn-agent-plans.el ends here
