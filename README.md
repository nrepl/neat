# neat

[![CI](https://github.com/nrepl/neat/actions/workflows/ci.yml/badge.svg)](https://github.com/nrepl/neat/actions/workflows/ci.yml)

A small, language-agnostic [nREPL][nrepl] client for Emacs, in the spirit of
[monroe][monroe].

Where monroe targets Clojure specifically, `neat` aims to work with any
language that ships a proper nREPL server. It's useful directly (a REPL
buffer plus a source-buffer minor mode) and also as a library that other
Emacs packages can build on.

This is an early, experimental project. Expect rough edges.

## Status

Pre-alpha. The first cut establishes the project skeleton, a bencode
codec, the wire protocol plumbing, and a basic comint-based REPL. No
release yet.

## Modules

`neat` is a few small files instead of one big one, so other packages can
pick and choose:

- `neat-bencode.el` - bencode encode/decode, no other dependencies.
- `neat-client.el` - connection management, request dispatch, nREPL ops.
- `neat-repl.el` - comint-derived REPL buffer.
- `neat.el` - entry point, customization group, `neat-mode` minor mode for
  source buffers.

Library users typically only need `neat-bencode` and `neat-client`.

## Quick start

Start an nREPL server. Anything that speaks the protocol will do; for a
Clojure server the easy options are:

```
bb nrepl-server :port 7888
# or
lein repl :headless :port 7888
# or
clj -M:nrepl
```

Then in Emacs:

```
M-x neat RET localhost RET 7888 RET
```

A `*neat: localhost:7888*` buffer pops up with a prompt. Type an
expression, hit `RET`, see the result. Multi-line forms work too --
`RET` only submits when the input parses as balanced; otherwise it
inserts a newline so you can finish the form. Input history is
persisted between sessions in `neat-repl-history-file` and the prompt
follows the server's reported namespace (`user> `, `myapp.core> `, ...).

To evaluate from a source buffer:

```
M-x neat-mode
```

Bindings:

| Key       | Command                |
|-----------|------------------------|
| `C-c C-e` | `neat-eval-last-sexp`  |
| `C-c C-c` | `neat-eval-defun`      |
| `C-c C-r` | `neat-eval-region`     |
| `C-c C-b` | `neat-eval-buffer`     |
| `C-c C-z` | `neat-switch-to-repl`  |
| `C-c C-k` | `neat-cancel`          |

## Development

The project uses [Eldev][eldev] and [Buttercup][buttercup].

```
eldev compile     # byte-compile
eldev lint        # lint
eldev test        # run Buttercup suites
```

The default suite is the fast one. There's also an integration suite that
boots real nREPL servers as subprocesses and exercises the full client.
It's gated behind an env var since starting a server adds a few seconds:

```
NEAT_INTEGRATION=1 eldev test
```

The integration suite walks `neat-it--server-impls` in
`test/neat-integration-test.el` and registers a block per implementation
that's installed on PATH. Currently:

| Implementation | Executable | How to install |
|----------------|------------|----------------|
| Clojure (`nrepl/nrepl`) | `clojure` | [clojure.org/guides/install_clojure](https://clojure.org/guides/install_clojure) |
| [Babashka](https://babashka.org) | `bb` | `brew install borkdude/brew/babashka` |

Add more entries to the list to teach the suite about your favorite
nREPL implementation -- it just needs an executable that prints a port
banner on stdout.

## License

Distributed under the GNU General Public License, version 3 or later. See
[`LICENSE`](LICENSE).

[nrepl]: https://nrepl.org
[monroe]: https://github.com/sanel/monroe
[eldev]: https://github.com/emacs-eldev/eldev
[buttercup]: https://github.com/jorgenschaefer/emacs-buttercup
