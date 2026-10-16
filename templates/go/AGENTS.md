# Agent instructions

The operator is the person who runs these agents, and who signs, pushes and decides.

## Checking a change

Run from the module root, inside `nix develop` (or via direnv).

| Check | Passes when |
| --- | --- |
| `gofmt -l .` | prints nothing |
| `go vet ./...` | exits 0 |
| `go test -race -count=1 ./...` | exits 0 |
| `golangci-lint run` | exits 0 |
| `govulncheck ./...` | exits 0 (needs network for its database) |
| `go mod tidy` | leaves `go.mod` and `go.sum` unchanged |
| `go generate ./...` | leaves generated files unchanged (only if the repo uses it) |
| `nix flake check` | exits 0 |

CI (`.github/workflows/go.yml`) runs the gofmt, vet, test, golangci-lint and
govulncheck checks in the dev shell on each push and pull request, with
`GOTOOLCHAIN=local`. Commit `flake.lock`, or CI resolves the flake's inputs
afresh and checks with tools other than yours.

govulncheck fails only on vulnerabilities the code can reach. One in the
standard library is fixed by the Go that `nix flake update nixpkgs` brings
in, when nixpkgs has a patched release.

Lint and test under every target the module builds for, not just the host:
`GOOS=linux GOARCH=amd64 go vet ./...`, and likewise for each other GOOS/GOARCH pair.

`golangci-lint run` includes gosec. A finding that does not apply is annotated
where it is found, `//nolint:gosec // <why it does not apply>`; one without a
rule or a reason fails the lint.

In Claude Code, project hooks in `.claude/hooks/` run `gofmt -w` on each
edited `.go` file and, when you stop, `golangci-lint run` (or `go vet` where
golangci-lint is missing) on the packages holding uncommitted or untracked
`.go` files. The stop hook blocks at most one stop, sees nothing once a change
is committed, and lets the stop through without `jq` or when `timeout` cuts
the lint off at 120 s. It warns early; it does not replace the checks above.
If gofmt reports a reformat, re-read the file before editing it again.

## Go tests

- Group related cases under one parent test with a short name
  (`TestParse`), each case a `t.Run` subtest, so `-run TestParse/empty`
  selects one.
- Helpers call `t.Helper()`, so a failure points at the caller's line.
- Contexts come from `t.Context()`, never `context.Background()` or
  `context.TODO()`: it is cancelled when the test ends, which stops what the
  test started.
- Use `t.TempDir()` for files and `t.Cleanup` for teardown.
- Each test and subtest builds its own state. Share setup only when it is too
  slow to repeat, read-only once built, and made in `TestMain` or a
  `sync.OnceValue`; say so where it is defined.
- No sleeps: wait on a channel, callback or hook the code signals when it is
  done, adding one if it has none, or inject a clock.
- CI runs with `-race`.

### Hermetic tests

A test touches nothing outside what it set up: only its own `t.TempDir()`,
never the home directory, global configuration or the test binary. It
reaches no network: a network dependency is an `httptest` server whose
address the test injects. Never rely on a port being closed or a binary
missing; start or inject what the test needs.

Every package with tests has a `TestMain` that isolates it and fails
closed. git reads neither the user's configuration nor the caller's
repository, which a test started from a git hook or `git rebase --exec`
otherwise reads and writes; every request that honours the proxy
variables, from the test binary and the git it runs, goes to a proxy that
drops every connection, so a request that escapes the test's own servers
fails:

```go
func TestMain(m *testing.M) {
	if err := isolate(); err != nil {
		fmt.Fprintln(os.Stderr, "isolating the tests:", err)
		os.Exit(1)
	}
	os.Exit(m.Run())
}

func isolate() error {
	for _, name := range []string{"GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR",
		"GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_PREFIX"} {
		if err := os.Unsetenv(name); err != nil {
			return err
		}
	}
	env := map[string]string{
		"GIT_CONFIG_GLOBAL":   "/dev/null",
		"GIT_CONFIG_NOSYSTEM": "1",
	}

	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return err
	}
	go func() {
		for {
			conn, err := ln.Accept()
			if err != nil {
				return
			}
			_ = conn.Close() // dropping it is the point; a close error changes nothing
		}
	}()
	for _, name := range []string{"HTTP_PROXY", "HTTPS_PROXY", "ALL_PROXY", "http_proxy", "https_proxy", "all_proxy"} {
		env[name] = "http://" + ln.Addr().String()
	}
	for _, name := range []string{"NO_PROXY", "no_proxy"} {
		env[name] = "localhost,127.0.0.1,::1"
	}
	for name, value := range env {
		if err := os.Setenv(name, value); err != nil {
			return err
		}
	}
	return nil
}
```

Go reads the proxy variables once per process, so this runs before any
request.

## Red check

To enable `git red-check` for this repository, set
`wgn.home.llm.repos."owner/name"` in the operator's nix-config with test
`go test ./...`, build `go build ./... && go vet ./...` and testFiles
`**/*_test.go`.
