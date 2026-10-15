;;; wgn-agent-plans.el --- Read agents' plan files -*- lexical-binding: t; -*-

;; Plans are flat files in the plans directory, written by `agent-plan':
;;   <YYYY-MM-DD>-<slug>.<host>.org         the plan
;;   <YYYY-MM-DD>-<slug>.<host>.<part>.org  a subagent's part
;;   <YYYY-MM-DD>-<slug>.<host>.log.org     the log
;; Only the plan files are listed here.  Nothing here adds them to
;; `org-agenda-files' or `org-refile-targets', or writes any other Org file.

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

(defconst wgn/agent-plans--file-regexp
  (concat "\\`\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]-[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\)"
          "\\.\\([A-Za-z0-9][A-Za-z0-9_-]*\\)\\(?:\\.\\(P[0-9]+\\|log\\)\\)?\\.org\\'")
  "Match a plan's, a part's or a log's file name, as `agent-plan' names them.
Group 1 is the plan's name, 2 its host, and 3 the part or \"log\".
A plan's own file has no group 3.")

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

(defun wgn/agent-plans--config-file ()
  "Return the file `agent-plan' reads its settings from."
  (or (getenv "AGENT_TOOLS_CONFIG")
      (expand-file-name "agent-tools/config.json"
                        (or (getenv "XDG_CONFIG_HOME") "~/.config"))))

(defun wgn/agent-plans-directory ()
  "Return the directory holding the plans.
It is \"plans.dir\" in `agent-plan''s config file, or ~/git/notes/agents."
  (let* ((file (wgn/agent-plans--config-file))
         (dir (when (file-readable-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (gethash "dir" (or (gethash "plans"
                                              (json-parse-buffer))
                                     (make-hash-table)))))))
    (expand-file-name (if (and (stringp dir) (not (string-empty-p dir)))
                          dir
                        "~/git/notes/agents"))))

(defun wgn/agent-plans-files ()
  "Return every plan file in the plans directory, newest first.
Other hosts' plans, which are mirrors, are included.  Part and log files
are not."
  (sort (when (file-directory-p (wgn/agent-plans-directory))
          (seq-filter (lambda (file)
                        (let ((base (file-name-nondirectory file)))
                          (and (string-match wgn/agent-plans--file-regexp base)
                               (not (match-string 3 base)))))
                      (directory-files (wgn/agent-plans-directory) t
                                       wgn/agent-plans--file-regexp)))
        (lambda (a b)
          (string> (file-name-nondirectory a) (file-name-nondirectory b)))))

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
                                            (wgn/agent-plans-directory))))))
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

(defun wgn/agent-plans-agenda-files (&optional own)
  "Return every plan's and part's file, newest plan first; with OWN, this host's.
Logs are not."
  (seq-filter
   (lambda (file)
     (let ((parts (wgn/agent-plans--file-parts file)))
       (and parts
            (not (equal (nth 2 parts) "log"))
            (or (not own) (equal (nth 1 parts) (wgn/agent-plans-host))))))
   (when (file-directory-p (wgn/agent-plans-directory))
     (sort (directory-files (wgn/agent-plans-directory) t "\\.org\\'")
           (lambda (a b)
             (string> (file-name-nondirectory a) (file-name-nondirectory b)))))))

(defun wgn/agent-plans-agenda-command ()
  "Return the `org-agenda-custom-commands' entry for key \"A\".
Its blocks are the questions the operator can answer, in this host's plans
alone since another host's are answered there; the parts and steps ready
to work on; and the open follow-ups.  An entry waiting on an :AFTER: item
is hidden, as is one scheduled for a later day.  `org-agenda-files' is
bound to the plan files for this view alone."
  `("A" "Agent plans"
    ((tags-todo ,(concat "ID={" wgn/agent-plans-question-id "}")
                ((org-agenda-overriding-header "Questions for the operator")
                 (org-agenda-files (wgn/agent-plans-agenda-files t))))
     ;; Two matches rather than one regexp: the matcher splits at `|'.
     (tags-todo ,(concat "ID={" wgn/agent-plans-part-id "}|ID={" wgn/agent-plans-step-id "}")
                ((org-agenda-overriding-header "Ready")))
     (tags-todo ,(concat "ID={" wgn/agent-plans-followup-id "}")
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
  (let ((file (wgn/agent-plans--config-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((plans (gethash "plans" (json-parse-buffer))))
          (and (hash-table-p plans) (gethash key plans)))))))

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
  (let ((pattern (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id))))
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

(defun wgn/agent-plans-blocker (change)
  "Refuse the Org state CHANGE where a plan does not allow it.
In another host's plan, every change, since only that host writes it;
elsewhere in a plan, a change to done while an ID in the entry's :AFTER:
is not settled.  Outside plan files, allow everything.  The agenda's dim
check, whose :from is the symbol `todo', is answered for :AFTER: alone,
so another host's entries stay listed."
  (let ((file (buffer-file-name (buffer-base-buffer)))
        (to (plist-get change :to)))
    (cond
     ((not (wgn/agent-plans--file-parts file)) t)
     ((and (wgn/agent-plans--mirror-host file)
           (not (eq (plist-get change :from) 'todo)))
      (setq org-block-entry-blocking
            (format "%s's plan; answer it there" (wgn/agent-plans--mirror-host file)))
      nil)
     ((not (or (eq to 'done) (member to org-done-keywords))) t)
     (t
      (let ((open (seq-remove (lambda (id) (wgn/agent-plans--settled-p file id))
                              (split-string
                               (or (org-entry-get (plist-get change :position) "AFTER") "")))))
        (when open
          (setq org-block-entry-blocking (string-join open " ")))
        (null open))))))

(defun wgn/agent-plans--refuse-save ()
  "Refuse to save another host's plan file."
  (user-error "%s is %s's plan; answer it there"
              (file-name-nondirectory buffer-file-name)
              (wgn/agent-plans--mirror-host buffer-file-name)))

(defun wgn/agent-plans--protect-mirror ()
  "Make the buffer of another host's plan read-only, and refuse its saves."
  (when (wgn/agent-plans--mirror-host buffer-file-name)
    (read-only-mode 1)
    (add-hook 'write-contents-functions #'wgn/agent-plans--refuse-save nil t)))

(defun wgn/agent-plans--after-revert ()
  "Read a reverted plan file's keyword lines again.
A revert that keeps the buffer's modes, as auto-revert's does, keeps the
keywords Org read before; `org-mode-restart' wipes buffer-local hooks,
so this one is global."
  (when (and (derived-mode-p 'org-mode) (wgn/agent-plans--file-parts buffer-file-name))
    (org-mode-restart)
    (wgn/agent-plans--protect-mirror)))

;;;###autoload
(define-minor-mode wgn/agent-plans-protect-mode
  "Hold plan files to what `agent-plan' expects of them.
Another host's plans are read-only and refuse every state change and
save; an entry is not done while an item in its :AFTER: is open; and a
reverted plan buffer reads its keyword lines again."
  :global t
  (if wgn/agent-plans-protect-mode
      (progn
        (add-hook 'org-blocker-hook #'wgn/agent-plans-blocker)
        (add-hook 'find-file-hook #'wgn/agent-plans--protect-mirror)
        (add-hook 'after-revert-hook #'wgn/agent-plans--after-revert))
    (remove-hook 'org-blocker-hook #'wgn/agent-plans-blocker)
    (remove-hook 'find-file-hook #'wgn/agent-plans--protect-mirror)
    (remove-hook 'after-revert-hook #'wgn/agent-plans--after-revert)))

(provide 'wgn-agent-plans)
;;; wgn-agent-plans.el ends here
