# neat

[![CI](https://github.com/nrepl/neat/actions/workflows/ci.yml/badge.svg)](https://github.com/nrepl/neat/actions/workflows/ci.yml)

> I take my REPL neat
> My coffee black and my bed at three

A small, language-agnostic [nREPL][nrepl] client for Emacs.

Where other nREPL clients (e.g. [monroe][] and CIDER) target Clojure specifically,
`neat` aims to be the purest language-agnostic nREPL client in the Emacs world:
no Clojure-flavored helpers, no hardcoded ops, no assumptions about your
project's build tool.  It's useful directly (a REPL buffer plus a source-buffer
minor mode) and also as a library that other Emacs packages can build on.

This is an early, experimental project. Expect rough edges.

## Status

Pre-alpha. The first cut establishes the project skeleton, a bencode
codec, the wire protocol plumbing, and a basic comint-based REPL. No
release yet.

## Project context

`neat` is part of a broader push to make nREPL a healthy multi-language
ecosystem rather than a Clojure-only protocol. That effort has three
strands:

1. **An official nREPL specification.** Today the [nREPL][nrepl] project
   is the de facto spec; a formal version is being drafted at
   [nrepl/spec.nrepl.org][spec]. `neat` aims to keep pressure on the spec
   to stay genuinely language-agnostic by being a client that refuses to
   silently assume Clojure.
2. **Reference clients.** A spec without independent client
   implementations is wishful thinking. `neat` is one such reference
   client, intentionally built on Emacs builtins and free of
   Clojure-specific helpers, so it can act as a baseline for what
   "compliant" should mean on the client side.
3. **A compatibility test suite.** The parameterised integration suite
   under [`test/neat-integration-test.el`](test/neat-integration-test.el)
   already runs the same assertions against multiple servers (Clojure,
   Babashka, Basilisp), and divergences between them get surfaced as
   real findings rather than mysterious bugs. The long-term goal is
   to grow this into a portable suite any nREPL server can self-check
   against.

[spec]: https://github.com/nrepl/spec.nrepl.org

## Installation

`neat` isn't on MELPA yet - that's an item on the road to 0.1. In the
meantime, the easiest path is `package-vc-install` on Emacs 29+:

```elisp
(package-vc-install
 '(neat :url "https://github.com/nrepl/neat" :branch "main"))
```

On Emacs 30+ with [`use-package`](https://github.com/jwiegley/use-package):

```elisp
(use-package neat
  :vc (:url "https://github.com/nrepl/neat" :branch "main")
  :commands (neat neat-mode))
```

With [`straight.el`](https://github.com/radian-software/straight.el):

```elisp
(straight-use-package
 '(neat :type git :host github :repo "nrepl/neat"))
```

Or combined with `use-package`:

```elisp
(use-package neat
  :straight (neat :type git :host github :repo "nrepl/neat")
  :commands (neat neat-mode))
```

For a manual checkout (e.g. while contributing):

```elisp
(add-to-list 'load-path "/path/to/neat")
(require 'neat)
```

`neat-mode` is a minor mode you turn on per source buffer; hook it onto
whichever languages you actually drive (`clojure-mode`, `fennel-mode`,
`hy-mode`, ...). The mode itself doesn't assume any specific language.

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
expression, hit `RET`, see the result. Multi-line forms work too:
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
| `C-c C-l` | `neat-load-buffer-file` |
| `C-c C-z` | `neat-switch-to-repl`  |
| `C-c C-k` | `neat-interrupt-eval`  |
| `C-c M-n` | `neat-set-ns`          |
| `C-c C-d C-d` | `neat-show-doc-at-point` |
| `M-.`     | `xref-find-definitions` |
| `M-,`     | `xref-go-back`         |

`neat-eval-buffer` ships the buffer contents as an `eval` op (each form
evaluated in turn, every value streamed back). `neat-load-buffer-file`
sends a `load-file` op instead, carrying the buffer's path and filename
so the server can attribute file and line numbers to errors. Use it
when you're loading an actual file from disk and care about good
diagnostics; use `neat-eval-buffer` when you're scratching around in
a buffer that may not even be on disk.

`M-.` is plain `xref-find-definitions`. `neat-mode` registers an xref
backend that asks the server's `lookup` op where the symbol at point
is defined and jumps there. `M-,` pops back through the standard
xref stack. Sources behind URLs we can't resolve locally (`jar:...`,
`http:...`, ...) yield no result; we don't try to extract files from
jars.

`neat-set-ns` (`C-c M-n`) sets the buffer-local namespace `neat-ns`,
which is sent as the `ns` field on every `eval` op from this buffer.
Set it explicitly per buffer, in a major-mode hook, or via
`.dir-locals.el`. For languages where the namespace is declared in
the source (Clojure's `(ns foo.bar)`, etc.), swap in a parser via
`neat-buffer-ns-function` -- the default just returns `neat-ns`.

## Design

For the rationale behind the architecture (the module split, the
async dispatch model, the comint pipe-process trick, why we target
Emacs 28+, and so on) see [`doc/design.md`](doc/design.md).

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
| [Basilisp](https://basilisp.readthedocs.io) (Python) | `basilisp` | `pipx install basilisp` |

Add more entries to the list to teach the suite about your favorite
nREPL implementation. It just needs an executable that prints a port
banner on stdout.

### Debugging the wire

When something goes wrong (eldoc not firing, an op returning a surprise
shape, ...) the quickest way to find out is to mirror every nREPL
message neat exchanges with the server:

```
M-x neat-toggle-message-log
```

Both directions land pretty-printed in `*neat-messages*`, prefixed with
`-->` (request) or `<--` (response). Customize the buffer name via
`neat-message-log-buffer-name`; permanently enable with
`(setq neat-log-messages t)`.

## License

Distributed under the GNU General Public License, version 3 or later. See
[`LICENSE`](LICENSE).

[nrepl]: https://nrepl.org
[monroe]: https://github.com/sanel/monroe
[eldev]: https://github.com/emacs-eldev/eldev
[buttercup]: https://github.com/jorgenschaefer/emacs-buttercup
