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

(defconst wgn/agent-plans--plan-regexp
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-[^.]+\\.[^.]+\\.org\\'"
  "Match the name of a plan file, not of a part or a log.")

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
          (directory-files (wgn/agent-plans-directory) t
                           wgn/agent-plans--plan-regexp))
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

(defun wgn/agent-plans-agenda-command ()
  "Return the `org-agenda-custom-commands' entry for key \"A\".
It lists TODO entries in every plan file, with `org-agenda-files' bound to
those files for that view alone."
  '("A" "Agent plans" alltodo ""
    ((org-agenda-files (wgn/agent-plans-files))
     (org-agenda-overriding-header "Agent plans"))))

(provide 'wgn-agent-plans)
;;; wgn-agent-plans.el ends here
