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

(provide 'wgn-agent-plans)
;;; wgn-agent-plans.el ends here
