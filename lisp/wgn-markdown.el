;;; wgn-markdown.el --- Code blocks in Markdown and Org others wrote -*- lexical-binding: t; -*-

;; markdown-mode, Org (source and export blocks alike), shell-maker's
;; markdown-overlays (chatgpt-shell, agent-shell) and gptel-agent's tool
;; output fontify a code block by calling the major mode its language
;; or file name names in a scratch buffer, and the text comes from
;; people and models: pull request bodies and comments, forge topics,
;; ECA, gptel and agent replies.  A name is only what someone chose, so
;; the mode is called only when it is one of an allowed set, and then
;; inertly: without mode hooks, file-local or directory-local variables,
;; a file name or auto-saving, and without what its body adds to the
;; alists of file modes or, as closures, to global hooks outliving it.
;;
;; Org also calls a block's mode to export, tangle, expand noweb
;; references in and edit it, in any buffer, the operator's own
;; included.  Export, tangling and noweb expansion call it only as
;; fontification does, and editing refuses a mode outside the same
;; set unless the operator says otherwise; an edit buffer's text
;; chooses no other mode, an exported file's text chooses none for the
;; HTML export, whose mode is at most the one an .html file gets, LaTeX
;; exports are put in Emacs's own latex-mode, and an asynchronous
;; export's Emacs is guarded the same way.
;; What is guarded is which mode a block's or file's
;; text can make Emacs call; what Org evaluates from header arguments
;; and macros follows Org's own settings.

(require 'cl-lib)
(require 'subr-x)

(defvar markdown-code-lang-modes)
(defvar org-src-lang-modes)
(declare-function markdown-overlays--resolve-internal-language "markdown-overlays")

(defconst wgn/fence-modes '(fundamental-mode markdown-mode gfm-mode)
  "Modes a code block may be fontified in whatever they derive from.")

(defconst wgn/fence-mode-families
  '(prog-mode text-mode conf-mode diff-mode css-base-mode sgml-mode)
  "Modes whose derived modes a code block may be fontified in.")

(defconst wgn/fence-denied-families
  '(special-mode comint-mode compilation-mode
    message-mode mail-mode log-edit-mode git-commit-elisp-text-mode forge-post-mode
    org-mode TeX-mode tex-mode latex-mode ses-mode
    image-mode archive-mode tar-mode doc-view-mode hexl-mode reader-mode
    nxml-mode plstore-mode go-asm-mode vhdl-mode idlwave-mode edebug-eval-mode reb-lisp-mode
    cider-history-mode slime-xref-mode dape-disassemble-mode
    antlr-mode cperl-mode verilog-mode bibtex-mode ps-mode nix-drv-mode)
  "Modes inside `wgn/fence-mode-families' whose derived modes a code
block is never fontified in: they read or write files, run processes,
leave timers or global hooks behind, choose another mode from the
text, set up mail, version control or REPL state, or evaluate what
the buffer holds.  Each is denied by name before anything loads.")

(defconst wgn/fence-denied-names
  (rx (or "--" "TeX" (seq "-maybe-mode" eos)
          (seq bos (or "inferior-" "agent-shell-" "chatgpt-shell-" "eca-" "forge-"
                       "magit-" "git-" "cider-" "slime-" "dape-" "gdb-" "gud-"
                       "edebug-"))))
  "Mode names a code block is never fontified in, matched with case:
internal modes, AUCTeX's, modes that choose another mode from the
buffer's text, and the modes of tools that act on a session,
repository or process.")

(defconst wgn/fence-loadable-modes
  '(html-mode makefile-mode)
  "Autoloaded modes no alist of file modes names that a code block may
load to be fontified in.  Loading a library runs it, so a mode joins
this list only once loading it is seen to leave no timer, process,
prompt, global mode or new entry in an alist of file modes behind.
The built-in tree-sitter modes fail the last: loading one claims its
file names in `auto-mode-alist' when its grammar is installed.")

(defun wgn/fence--named-p (mode)
  "Return non-nil when an alist of file or language modes names MODE."
  (cl-some (lambda (alist)
             (cl-some (lambda (entry)
                        (and (consp entry)
                             (eq mode (if (consp (cdr entry)) (cadr entry) (cdr entry)))))
                      alist))
           (list auto-mode-alist interpreter-mode-alist magic-mode-alist
                 (bound-and-true-p major-mode-remap-alist)
                 (bound-and-true-p markdown-code-lang-modes))))

(defun wgn/fence--load (mode)
  "Load MODE's definition when it is still an autoload of a function,
and an alist of file modes names it or it is in
`wgn/fence-loadable-modes'.  A code block's language must not choose
which other library loads: loading one runs it.  A named library loads
before the mode is known to be minor or denied, as artist.el does for
a ditaa block, along with what its `with-eval-after-load' forms load:
an alist already maps a file or language to it, so the session would
load it for that anyway."
  (let ((definition (symbol-function mode)))
    (when (and (autoloadp definition) (memq (nth 4 definition) '(nil t))
               (or (memq mode wgn/fence-loadable-modes) (wgn/fence--named-p mode)))
      (ignore-errors (autoload-do-load definition mode)))))

(defun wgn/fence-mode-p (mode)
  "Return non-nil when a code block may be fontified by calling MODE.
MODE must be in `wgn/fence-modes', or derive from one of
`wgn/fence-mode-families' and none of `wgn/fence-denied-families'
nor markdown-mode, and its name must not match
`wgn/fence-denied-names'.  What the name decides is judged first, and
only then may an autoloaded MODE be loaded, see `wgn/fence--load', so
that what it derives from is known."
  (and mode (symbolp mode) (fboundp mode)
       (not (let ((case-fold-search nil))
              (string-match-p wgn/fence-denied-names (symbol-name mode))))
       (not (memq mode wgn/fence-denied-families))
       (not (memq mode minor-mode-list))
       (progn (wgn/fence--load mode) t)
       (not (memq mode minor-mode-list))
       (or (memq mode wgn/fence-modes)
           (and (apply #'provided-mode-derived-p mode wgn/fence-mode-families)
                (not (apply #'provided-mode-derived-p mode wgn/fence-denied-families))
                (not (provided-mode-derived-p mode 'markdown-mode))))
       t))

(defvar wgn/fence--fontifying nil
  "Non-nil while a code block is fontified, when a language's mode is
judged by `wgn/fence-mode-p'.  Elsewhere Org's calls are judged by
`wgn/org-src--mode-call', and other calls are left alone.")

(defconst wgn/fence--scratch-buffers
  (rx bos " *" (or "markdown-code" "markdown-overlays" "org-src") "-fontification:")
  "The names of the scratch buffers code blocks are fontified in.")

(defun wgn/fence--scrub ()
  "Leave every code block scratch buffer without a file or delayed hooks."
  (dolist (buffer (buffer-list))
    (when (string-match-p wgn/fence--scratch-buffers (buffer-name buffer))
      (with-current-buffer buffer
        (when buffer-auto-save-file-name
          (auto-save-mode -1))
        (setq buffer-file-name nil
              buffer-file-truename nil
              buffer-auto-save-file-name nil)
        (set-buffer-modified-p nil)))))

(defvar wgn/fence--old-buffers nil
  "The buffers that existed when the outermost code block fontification began.")

(defconst wgn/fence--watched-alists
  '(auto-mode-alist interpreter-mode-alist magic-mode-alist magic-fallback-mode-alist
    major-mode-remap-alist)
  "The alists of file modes a mode's body must not change for good.")

(defvar wgn/fence--alist-cells nil
  "One cell per code block fontification under way, innermost first.
Each cell's car holds copies of the values of
`wgn/fence--watched-alists' to restore when that fontification ends,
as (SYMBOL . VALUE).  A library loading refreshes every cell, so an
enclosing fontification, such as of a Markdown block holding another
block, keeps what the library added.")

(defvar wgn/fence--hooks nil
  "The (HOOK . FUNCTION)s a mode's body added to hooks' global values
while a code block was fontified, to remove after it.")

(defvar wgn/fence--load-file nil
  "`load-file-name' when the outermost code block fontification began:
a different one while it runs means a library is loading inside it.")

(defun wgn/fence--snapshot ()
  "Return copies of the current values of `wgn/fence--watched-alists'.
They are copies so that an edit in place, such as `assoc-delete-all'
or `setcdr', shows as a change."
  (mapcar (lambda (symbol)
            (cons symbol (and (boundp symbol) (copy-alist (default-value symbol)))))
          wgn/fence--watched-alists))

(defun wgn/fence--after-load (&rest _)
  "Keep what a library loaded during fontification adds to the alists.
A library's load-time setup is for the whole session; only what a
mode's body does afterwards is undone."
  (dolist (cell wgn/fence--alist-cells)
    (setcar cell (wgn/fence--snapshot))))

(defun wgn/fence--add-hook (add hook function &optional depth local)
  "Call ADD with HOOK, FUNCTION, DEPTH and LOCAL, noting a global closure.
While a code block is fontified, a function that is not a symbol added
to HOOK's global value outside a library's load is a mode body's
per-buffer state leaking into every buffer, such as clojure-mode's
docstring indentation; `wgn/fence--inert' removes it after.  So is a
closure that a `with-eval-after-load' form in the mode's body adds at
once, its feature being loaded already.  Named functions stay: a
library may add them once, as cc-mode does."
  (when (and wgn/fence--fontifying (not local)
             (equal load-file-name wgn/fence--load-file)
             (not (symbolp function))
             (not (and (boundp hook)
                       (let ((value (default-value hook)))
                         (if (functionp value) (equal value function) (member function value))))))
    (push (cons hook function) wgn/fence--hooks))
  (funcall add hook function depth local))

(defun wgn/fence--inert (fontify &rest args)
  "Call FONTIFY with ARGS so that the mode it calls for a code block is inert.
No file-local or directory-local variable is read, so the block's
text and the directory it is shown from cannot set anything, and
`wgn/fence--run-mode-hooks' keeps mode hooks from running.  What the
mode's body adds to the alists of file modes or, as closures, to hooks'
global values is undone after.  All of it ends however FONTIFY does,
returning, signalling or quit."
  (let ((wgn/fence--load-file (if wgn/fence--fontifying wgn/fence--load-file load-file-name))
        (wgn/fence--fontifying t)
        (wgn/fence--old-buffers (or wgn/fence--old-buffers (buffer-list)))
        (wgn/fence--alist-cells (cons (list (wgn/fence--snapshot)) wgn/fence--alist-cells))
        (wgn/fence--hooks nil)
        (enable-local-variables nil)
        (enable-dir-local-variables nil)
        (auto-save-default nil))
    (unwind-protect
        (apply fontify args)
      (pcase-dolist (`(,hook . ,function) wgn/fence--hooks)
        (remove-hook hook function))
      (pcase-dolist (`(,symbol . ,value) (car (car wgn/fence--alist-cells)))
        (unless (equal (and (boundp symbol) (default-value symbol)) value)
          (set-default symbol value)))
      (wgn/fence--scrub))))

(defun wgn/fence--run-mode-hooks (run &rest hooks)
  "Call RUN with HOOKS, except in a buffer a code block's fontification made.
That is a code block's scratch buffer, or any buffer that did not exist
when the fontification began, such as one the mode makes.  There a
mode's hooks, `after-change-major-mode-hook' and the file-local
variables `run-mode-hooks' would read are all skipped.  A buffer that
existed before keeps its hooks.  A timer or process filter that runs
while fontification waits is treated as part of it: a buffer it makes
skips its hooks, and a closure it adds to a global hook is removed
after.  No mode a code block may be fontified in is known to wait.
`delay-mode-hooks' cannot
do this: it is local to each buffer, and those buffers are made
inside the fontifying function."
  (unless (and wgn/fence--fontifying
               (or (string-match-p wgn/fence--scratch-buffers (buffer-name))
                   (not (memq (current-buffer) wgn/fence--old-buffers))))
    (apply run hooks)))

(defun wgn/fence--diff-syntax (fontify &rest args)
  "Call FONTIFY with ARGS unless a code block is fontified.
FONTIFY is `diff-syntax-fontify-hunk', which with
`diff-font-lock-syntax' set to hunk-also or hunk-only sets a mode
from the file name in the diff's header, or reads that file."
  (unless wgn/fence--fontifying
    (apply fontify args)))

(defun wgn/fence--mode-only (mode)
  "Return MODE, or nil when a code block is fontified and MODE may not be."
  (if (or (not wgn/fence--fontifying) (wgn/fence-mode-p mode)) mode nil))

(defun wgn/fence--markdown-overlays (fontify start end lang &rest args)
  "Call FONTIFY on a code block in LANG, inertly, as `wgn/fence--inert'.
FONTIFY is `markdown-overlays--fontify-source-block', which calls
LANG-mode itself: a LANG whose mode may not be called is replaced by
one that names no mode.  START, END and ARGS pass through."
  (let* ((name (and (stringp lang)
                    (or (markdown-overlays--resolve-internal-language lang)
                        (downcase (string-trim lang)))))
         (mode (and name (intern-soft (concat name "-mode")))))
    (apply #'wgn/fence--inert fontify start end
           (if (and mode (fboundp mode) (not (wgn/fence-mode-p mode)))
               "wgn-no-fence"
             lang)
           args)))

(defun wgn/fence--file-mode (file)
  "Return the mode `auto-mode-alist' gives FILE by its name alone."
  (let* ((case-fold-search auto-mode-case-fold)
         (mode (assoc-default (file-name-sans-versions file) auto-mode-alist
                              #'string-match-p)))
    (if (consp mode) (car mode) mode)))

(defun wgn/fence--gptel-agent (fontify path-or-mode start end)
  "Call FONTIFY on START to END, inertly, as `wgn/fence--inert'.
FONTIFY is `gptel-agent--fontify-block', which fontifies a tool's
text as the file a model named, PATH-OR-MODE, choosing the mode with
`set-auto-mode' from that name and the text's own mode cookie.  It is
given a mode instead: the one the file name alone gives, when it may
fontify a code block, else `fundamental-mode'."
  (wgn/fence--inert
   (lambda ()
     (let ((mode (if (symbolp path-or-mode)
                     path-or-mode
                   (and (stringp path-or-mode) (wgn/fence--file-mode path-or-mode)))))
       (funcall fontify (if (wgn/fence-mode-p mode) mode 'fundamental-mode) start end)))))

(defcustom wgn/org-src-edit-modes
  '(org-mode latex-mode bibtex-mode nxml-mode texinfo-mode cperl-mode hledger-mode
    verb-mode artist-mode)
  "Modes Org may edit a block in besides those `wgn/fence-mode-p' allows.
Org edits a block in its mode to open it, and to fill, comment and
indent it.  The block's text chooses the mode, so a mode belongs here
only when calling it, with its hooks, for anyone's block is safe.  A
mode here also covers its aliases, such as `xml-mode' for
`nxml-mode', and what `major-mode-remap-alist' maps it to.  Latex
and texinfo edit in Emacs's own modes unless `wgn/org-src-auctex'
says otherwise.  Org calls minor modes for some languages: `verb-mode' for
verb and `artist-mode' for ditaa, both local to the edit buffer.  See
`wgn/org-edit-src-anyway' to edit one block in any mode."
  :type '(repeat function)
  :group 'org-edit-structure)

(defconst wgn/org-src--auctex-modes
  '((latex-mode . LaTeX-mode) (texinfo-mode . Texinfo-mode))
  "Emacs's own modes for Org's languages, with AUCTeX's in their place.")

(defcustom wgn/org-src-auctex nil
  "Non-nil to use AUCTeX's modes for Org's latex and texinfo text.
That is in the buffers Org edits latex and texinfo blocks in and puts
a LaTeX export in.  Otherwise they use Emacs's own `latex-mode' and
`texinfo-mode', whatever AUCTeX remaps them to, and files are not
affected.  AUCTeX's modes can load and run style files a repository
keeps beside the Org file, for the packages its text names, so set
this only if every Org file edited or exported so is trusted."
  :type 'boolean
  :group 'org-edit-structure)

(defvar wgn/org-src--guarded nil
  "Non-nil while a block is tangled or has its noweb references expanded.")

(defvar wgn/org-src--editing nil
  "Non-nil while a block is opened for editing, see `wgn/org-src--edit'.")

(defvar wgn/org-src--anyway nil
  "Non-nil while `wgn/org-edit-src-anyway' opens a block.")

(defvar wgn/org-src--refusal nil
  "The message of the last refusal to edit a block, for
`wgn/org-src--report-refusal' to signal.")

(defvar TeX-parse-self)
(defvar TeX-style-path)
(declare-function org-src-edit-buffer-p "org-src" (&optional buffer))

(defun wgn/org-src--tex-contained (function &rest args)
  "Call FUNCTION with ARGS, loading no AUCTeX style file beside the buffer.
It applies only with `wgn/org-src-auctex'.  With
`TeX-parse-self', AUCTeX parses a block opened in one of its
modes for the packages it uses and loads NAME.el for each from the
directories in `TeX-style-path'; the relative ones resolve against the
Org file's directory, so a repository a block came with could supply
style/NAME.el.  Parsing is off and only the absolute directories stay,
during the call and in the edit buffer it leaves current.  AUCTeX
loads first when it is installed and not loaded yet, so that binding
its variables does not keep its own values from being set."
  (when (and (featurep 'tex-site) (not (boundp 'TeX-style-path)))
    (require 'tex nil t))
  (if (not (boundp 'TeX-style-path))
      (apply function args)
    (let* ((path (seq-filter #'file-name-absolute-p TeX-style-path))
           (result (let ((TeX-parse-self nil)
                         (TeX-style-path path))
                     (apply function args))))
      (when (org-src-edit-buffer-p)
        (setq-local TeX-parse-self nil)
        (setq-local TeX-style-path path))
      result)))

(defun wgn/org-src--keep-mode (function &rest args)
  "Call FUNCTION with ARGS, except in a buffer Org edits a block in.
FUNCTION is `normal-mode' or `hack-local-variables', which would let
the block's text, its -*- line or Local Variables, choose the buffer's
mode or set its variables.  There the buffer keeps the mode Org chose
for the block's language, see `wgn/org-src--mode-call'.  While Org
opens the buffer, before `org-src-mode' is on, a mode's hooks can call
them too, there and only there: Org names the buffer it opens
\"*Org Src ...\", and other buffers, such as a file a timer visits
while Org asks a question, set their modes as ever."
  (if (or (bound-and-true-p org-src-mode)
          (and wgn/org-src--editing (string-prefix-p "*Org Src " (buffer-name))))
      (when (eq function #'normal-mode)
        (message "Keeping `%s': the text of a block Org edits chooses no mode" major-mode))
    (apply function args)))

(defun wgn/org-src--by-name (name function &rest args)
  "Call FUNCTION with ARGS, `set-auto-mode' choosing at most NAME's mode.
FUNCTION sets a buffer's mode with `set-auto-mode' from text an Org
file exported, whose Local Variables paragraph, -*- line, #! line or
leading text could otherwise pick any mode.  Here no local variable,
interpreter or `magic-mode-alist' entry counts, and of
`magic-fallback-mode-alist' only the entries for the mode a file named
NAME gets, so that text that looks like such a file still gets it,
and other text the mode the buffer's name gives."
  (let* ((mode (wgn/fence--file-mode name))
         (enable-local-variables nil)
         (interpreter-mode-alist nil)
         (magic-mode-alist nil)
         (magic-fallback-mode-alist
          (and mode (cl-remove-if-not (lambda (entry) (eq (cdr-safe entry) mode))
                                      magic-fallback-mode-alist))))
    (apply function args)))

(defun wgn/org-src--html-by-name (function &rest args)
  "Call FUNCTION with ARGS, an HTML export's mode set as for an .html file.
See `wgn/org-src--by-name'."
  (apply #'wgn/org-src--by-name "export.html" function args))

(defun wgn/org-src--export-buffer-args (args)
  "Return ARGS of `org-export-to-buffer' with its POST-PROCESS guarded.
POST-PROCESS sets the export buffer's mode once the exported text is
in it, then or, for an asynchronous export, when the text arrives:
`set-auto-mode' for HTML, which then gives at most the mode of a file
named after the backend, see `wgn/org-src--by-name', or AUCTeX's
`LaTeX-mode' for LaTeX, Beamer and KOMA letters, which becomes
Emacs's own, see `wgn/org-src--own'."
  (let ((name (format "export.%s" (car args)))
        (post-process (nth 7 args)))
    (if (not post-process)
        args
      (append (seq-take args 7)
              (list (lambda ()
                      (wgn/org-src--by-name
                       name (if (symbolp post-process)
                                (wgn/org-src--own post-process)
                              post-process))))
              (nthcdr 8 args)))))

(defvar wgn/fence--source (or load-file-name buffer-file-name)
  "The file this guard was loaded from: wgn-markdown or the init file
it is part of.")

(defvar org-export-async-init-file)

(defun wgn/org-src--guard-text ()
  "Return the text of this guard's forms, from its first line to its last.
They are read from the source of `wgn/fence--source': the first line
starts \";;; wgn-markdown.el --- \" and the last is \";;; wgn-markdown.el
ends here\", each at the start of a line and each exactly once, and
every form between them must read.  Otherwise this is a `user-error'."
  (let ((source (and wgn/fence--source
                     (concat (file-name-sans-extension wgn/fence--source) ".el")))
        ;; Built here, so that this file's own text holds neither line.
        (first (concat "^;;; " (regexp-quote "wgn-markdown.el --- ")))
        (last (concat "^;;; " (regexp-quote "wgn-markdown.el ends here") "$")))
    (unless (and source (file-readable-p source))
      (user-error "No source of the code block guard for an asynchronous export: %s"
                  wgn/fence--source))
    (with-temp-buffer
      (insert-file-contents source)
      (let ((starts (wgn/org-src--matches first))
            (ends (wgn/org-src--matches last)))
        (unless (and (= (length starts) 1) (= (length ends) 1) (< (car starts) (car ends)))
          (user-error "The code block guard's source %s has not one first and one last line"
                      source))
        (narrow-to-region (car starts) (car ends))
        (goto-char (point-min))
        (condition-case nil
            (while (progn (forward-comment (buffer-size)) (not (eobp)))
              (read (current-buffer)))
          (error (user-error "The code block guard's source %s does not read" source)))
        (buffer-string)))))

(defun wgn/org-src--matches (regexp)
  "Return where each match of REGEXP in the current buffer starts."
  (save-excursion
    (goto-char (point-min))
    (let (starts)
      (while (re-search-forward regexp nil t)
        (push (match-beginning 0) starts))
      (nreverse starts))))

(defun wgn/org-src--async-init-file ()
  "Return a new file that holds this guard, and nothing else, and enables it.
An asynchronous export runs a batch Emacs that loads only
`org-export-async-init-file', or else the user's init file, which
batch Emacs reads without the site-wide default.el this guard can be
part of.  The file holds the guard's forms, see
`wgn/org-src--guard-text'.  It is a new temporary file only its owner
can read or write, made for one export, see
`wgn/org-src--async-guarded'."
  (let ((text (wgn/org-src--guard-text)))
    (with-file-modes #o600
      (make-temp-file "wgn-markdown-async-" nil ".el"
                      (concat ";;; -*- lexical-binding: t; -*-\n" text
                              "\n(wgn/markdown-guard-code-block-modes)\n")))))

(defun wgn/org-src--async-guarded (start &rest args)
  "Call START with ARGS, so that the Emacs it starts is guarded too.
START is `org-export-async-start'.  Unless the operator names an
`org-export-async-init-file', it is a new `wgn/org-src--async-init-file',
deleted when the export's process ends."
  (if org-export-async-init-file
      (apply start args)
    (let* ((init (wgn/org-src--async-init-file))
           (org-export-async-init-file init)
           (started nil))
      (unwind-protect
          (progn
            (apply start args)
            (when-let* ((process (cl-find-if (lambda (process)
                                               (member init (process-command process)))
                                             (process-list))))
              (add-function :after (process-sentinel process)
                            (lambda (process _event)
                              (unless (process-live-p process)
                                (ignore-errors (delete-file init)))))
              (setq started t)))
        (unless started
          (ignore-errors (delete-file init)))))))

(defun wgn/org-src--report-refusal (function &rest args)
  "Call FUNCTION with ARGS, signalling a refusal to edit a block it hides.
FUNCTION fills, comments or runs a key sequence in a block by editing
it in its mode, which Org 9.8 does under a handler that drops the
refusal and reports a failure to leave an edit buffer instead."
  (let ((wgn/org-src--refusal nil)
        result)
    (condition-case err
        (setq result (apply function args))
      (error (unless wgn/org-src--refusal
               (signal (car err) (cdr err)))))
    (when wgn/org-src--refusal
      (user-error "%s" wgn/org-src--refusal))
    result))

(defun wgn/org-src--guard (function &rest args)
  "Call FUNCTION with ARGS, calling a block's mode only inertly.
FUNCTION is `org-babel-tangle' or `org-babel-expand-noweb-references',
which call it in a buffer of their own for its comment syntax, see
`wgn/org-src--mode-call'."
  (let ((wgn/org-src--guarded t))
    (apply function args)))

(defun wgn/org-src--edit (edit &rest args)
  "Call EDIT with ARGS, refusing a mode a code block may not be edited in.
EDIT is one of Org's commands that open a block in a buffer of its
language's mode, called directly or by Org to fill, comment or indent
a block, or to edit a LaTeX environment or fragment.  An allowed mode
runs as it would without this, hooks and all, since the operator works
in that buffer; see `wgn/org-src--mode-call' for which modes are."
  (let ((wgn/org-src--editing t))
    (if wgn/org-src-auctex
        (apply #'wgn/org-src--tex-contained edit args)
      (apply edit args))))

(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-type "org-element" (node &optional anonymous))
(declare-function org-edit-src-code "org-src" (&optional code edit-buffer-name))
(declare-function org-edit-export-block "org-src" ())
(declare-function org-edit-inline-src-code "org-src" ())

(defun wgn/org-edit-src-anyway ()
  "Edit the source, export or inline source block at point in its mode.
The mode is called even when Org's editing would refuse it, see
`wgn/org-src-edit-modes', with its hooks, so use this only on a block
whose language is known to be safe to call.  Only this one edit is
allowed; Org's filling, commenting and indenting of the block are not."
  (interactive)
  (let ((wgn/org-src--anyway t))
    (pcase (org-element-type (org-element-context))
      ('inline-src-block (org-edit-inline-src-code))
      ('export-block (org-edit-export-block))
      ((or 'src-block 'example-block) (org-edit-src-code))
      (_ (user-error "Not in a source, export or inline source block")))))

(defun wgn/org-src--remap (mode)
  "Return the mode `major-mode-remap-alist' gives MODE, as Org would."
  (if (fboundp 'major-mode-remap)
      (major-mode-remap mode)
    (or (cdr (assq mode (bound-and-true-p major-mode-remap-alist))) mode)))

(defun wgn/org-src--own (mode)
  "Return Emacs's own mode for MODE when MODE is AUCTeX's in its place.
That is a mode of `wgn/org-src--auctex-modes', which AUCTeX has Org
use, or what Emacs's own mode is remapped to, unless
`wgn/org-src-auctex'."
  (or (and (not wgn/org-src-auctex)
           (car (cl-find-if (lambda (entry)
                              (memq mode (list (cdr entry) (wgn/org-src--remap (car entry)))))
                            wgn/org-src--auctex-modes)))
      mode))

(defun wgn/org-src--edit-mode-p (mode)
  "Return non-nil when `wgn/org-src-edit-modes' covers MODE.
MODE is what `org-src-get-lang-mode' gives, aliases and remapping
followed, so an entry covers it when MODE is the entry, an alias of it
or of what it is remapped to, or what it is remapped to.  Nothing
loads to decide."
  (let ((target (indirect-function mode)))
    (cl-some (lambda (entry)
               (cl-some (lambda (candidate)
                          (or (eq mode candidate)
                              (and target (eq target (indirect-function candidate)))))
                        (list entry (wgn/org-src--remap entry))))
             wgn/org-src-edit-modes)))

(defun wgn/fence--call-mode (mode)
  "Call MODE in the current buffer inertly, as `wgn/fence--inert' does.
The current buffer counts as one the fontification made, so MODE's
hooks do not run in it."
  (let ((wgn/fence--old-buffers
         (remq (current-buffer) (or wgn/fence--old-buffers (buffer-list)))))
    (wgn/fence--inert mode)))

(defun wgn/org-src--stand-in (mode)
  "Return a function's name for Org to call in place of MODE, defining it.
The function calls MODE as `wgn/fence--call-mode' does when
`wgn/fence-mode-p' allows it.  Else it sets `fundamental-mode' and
says so, and Org asks for a comment syntax where it needs one, as for
a language without a mode.  It is named, not a closure, because Org
before 9.8 tests what it is given with `fboundp'."
  (let* ((allowed (wgn/fence-mode-p mode))
         (name (intern (format "wgn/org-src-%s:%s" (if allowed "inert" "refused") mode))))
    (unless (fboundp name)
      (defalias name
        (if allowed
            (lambda () (wgn/fence--call-mode mode))
          (lambda ()
            (message "Not calling `%s' for a source block: a code block may not use it" mode)
            (fundamental-mode)))
        (format "Stand-in for `%s' when Org tangles or expands noweb references." mode)))
    name))

(defun wgn/org-src--mode-call (mode)
  "Return what Org should call for MODE, a block's language's mode.
During fontification, and outside a tangle, noweb expansion or edit,
that is MODE, as it is when MODE is not defined: Org handles that.
Tangling or expanding noweb references, those callers of
`org-src-get-lang-mode' only call what it gives, in a buffer of their
own, so it is `wgn/org-src--stand-in' for MODE.  Editing, AUCTeX's
mode for latex or texinfo becomes Emacs's, see `wgn/org-src--own', and the
result is given when `wgn/org-edit-src-anyway' asks, or
`wgn/org-src-edit-modes' covers it or `wgn/fence-mode-p' allows it,
both judging the name before anything loads; else it is a
`user-error', before any buffer is made."
  (cond
   ((or wgn/fence--fontifying (not (fboundp mode))) mode)
   (wgn/org-src--guarded (wgn/org-src--stand-in mode))
   (wgn/org-src--editing
    (setq mode (wgn/org-src--own mode))
    (if (or wgn/org-src--anyway (wgn/org-src--edit-mode-p mode) (wgn/fence-mode-p mode))
        mode
      (setq wgn/org-src--refusal
            (format-message "No editing in `%s': a code block may not use it (see `wgn/org-src-edit-modes', or edit it once with M-x wgn/org-edit-src-anyway)"
                            mode))
      (user-error "%s" wgn/org-src--refusal)))
   (t mode)))

(defun wgn/markdown-guard-code-block-modes ()
  "Fontify code blocks in Markdown and Org only in allowed modes, inertly.
Org's export of a block fontifies it so too, tangling and noweb
expansion call its mode only so, and editing one refuses a mode it may
not use, see `wgn/org-src--mode-call'.  Each function advised here may
be defined after this runs; the advice still applies."
  (advice-add 'run-mode-hooks :around #'wgn/fence--run-mode-hooks)
  (advice-add 'add-hook :around #'wgn/fence--add-hook)
  (add-hook 'after-load-functions #'wgn/fence--after-load)
  (advice-add 'gptel-agent--fontify-block :around #'wgn/fence--gptel-agent)
  (advice-add 'diff-syntax-fontify-hunk :around #'wgn/fence--diff-syntax)
  (advice-add 'markdown-fontify-code-block-natively :around #'wgn/fence--inert)
  (advice-add 'markdown-get-lang-mode :filter-return #'wgn/fence--mode-only)
  (advice-add 'org-src-font-lock-fontify-block :around #'wgn/fence--inert)
  (advice-add 'org-src-get-lang-mode :filter-return #'wgn/fence--mode-only)
  (advice-add 'org-src-get-lang-mode :filter-return #'wgn/org-src--mode-call)
  (advice-add 'org-babel-tangle :around #'wgn/org-src--guard)
  (advice-add 'org-babel-expand-noweb-references :around #'wgn/org-src--guard)
  ;; Each fontifies a block for export in a buffer of its own; a block
  ;; whose mode may not be called is exported as plain text.  That
  ;; relies on each taking the mode from `org-src-get-lang-mode', as
  ;; Org 9.8 does; Org 9.6's HTML and ODT exporters name it themselves.
  (advice-add 'org-html-fontify-code :around #'wgn/fence--inert)
  (advice-add 'org-latex-src--engrave-code :around #'wgn/fence--inert)
  (advice-add 'org-odt-do-format-code :around #'wgn/fence--inert)
  (advice-add 'org-edit-src-code :around #'wgn/org-src--edit)
  (advice-add 'org-edit-export-block :around #'wgn/org-src--edit)
  (advice-add 'org-edit-inline-src-code :around #'wgn/org-src--edit)
  (advice-add 'org-edit-latex-environment :around #'wgn/org-src--edit)
  (advice-add 'org-edit-latex-fragment :around #'wgn/org-src--edit)
  (advice-add 'normal-mode :around #'wgn/org-src--keep-mode)
  ;; It sets the mode of an HTML export's text, in the process that
  ;; exports.
  (advice-add 'org-html-final-function :around #'wgn/org-src--html-by-name)
  (advice-add 'org-export-to-buffer :filter-args #'wgn/org-src--export-buffer-args)
  (advice-add 'org-export-async-start :around #'wgn/org-src--async-guarded)
  (advice-add 'hack-local-variables :around #'wgn/org-src--keep-mode)
  ;; TAB, RET, `indent-region' and `fill-region' leave a refused block
  ;; as it is without saying so.
  (advice-add 'org-fill-paragraph :around #'wgn/org-src--report-refusal)
  (advice-add 'org-fill-element :around #'wgn/org-src--report-refusal)
  (advice-add 'org-comment-dwim :around #'wgn/org-src--report-refusal)
  (advice-add 'org-comment-or-uncomment-region :around #'wgn/org-src--report-refusal)
  (advice-add 'org-insert-comment :around #'wgn/org-src--report-refusal)
  (advice-add 'org-babel-do-key-sequence-in-edit-buffer :around
              #'wgn/org-src--report-refusal)
  (advice-add 'markdown-overlays--fontify-source-block :around
              #'wgn/fence--markdown-overlays))

(provide 'wgn-markdown)
;;; wgn-markdown.el ends here
