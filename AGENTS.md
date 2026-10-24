# emacs-config

The Emacs configuration (`default.el`, `lisp/`), the packages and overlays
that build it, and the Home Manager and NixOS modules in `flake.nix` that
declare its options (`home.programs.wgn.emacs`, `programs.wgn.emacs`).

## Work that belongs in another repository

A session commits only in the repository it started in. When an item you
find belongs to a repository listed below, hand it over:

1. Where you have `ListAgents`, list the peer sessions. If exactly one
   row's name is that repository's name and a hyphen, then a suffix, as
   `nix-config-0f` is, send it the item with `SendMessage`: what you found,
   where (a file and line, or a command and its output), and that it is for
   that session to record in its plan.
2. Record the item as a follow-up in your own plan, the repository's name
   in its title and, in its context, the session you sent it to or "not
   sent".
3. Name each item not sent in your hand-back, so the operator can pass it
   on. A session without a plan names every item there.

### nix-config

`~/git/github.com/wesnel/nix-config`, whose `AGENTS.md` describes its
layout. It takes this repository as the flake input `emacs-config`, pinned
in its `flake.lock`. Its work shows up here as:

- a refusal from a permission rule, a hook, the guard or a harness's
  sandbox, or a fault in a skill or the operator's instructions (`~/.claude`,
  `~/.codex`): it owns the harnesses' settings, the skills and the MCP
  servers;
- a fault in agent tooling on `PATH` that this flake does not build, such
  as `agent-plan`, `pr-loop` or `git delegate`;
- a machine's value for one of this repository's options, or the pin.

Two words mean different things in the two repositories. Here, a sandbox is
one this repository builds (`overlays/offline-run`, `overlays/gondolin`),
and `claude` and `codex` are the Emacs bridges
`home.programs.wgn.emacs.claude.enable` and `.codex.enable`; there, they
are the harnesses' sandboxes and the harnesses themselves.

`lisp/wgn-agent-plans.el` reads the plans `agent-plan` writes, and
nix-config's `agent-plans` check runs `tests/run-agent-plans.el` at the
pinned emacs-config against its own `agent-plan`. A change to how
`wgn-agent-plans.el` reads that format goes to a nix-config session as
above.
