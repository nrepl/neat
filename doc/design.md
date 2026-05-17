# neat Design Document

This document explains how neat is built and the rationale behind the
design decisions. It targets contributors and curious users; if you
just want to use neat, the [README](../README.md) is enough.

## Goals

- An Emacs nREPL client that is genuinely language-agnostic. No
  Clojure-specific helpers in the core, no assumptions about the
  server's project layout or build tool.
- Useful both as a direct UI (REPL buffer + `neat-mode` minor mode)
  and as a library other Emacs packages can build on.
- Small. The whole codebase should fit in a single afternoon's
  reading. Currently ~800 LOC of Elisp across four files.
- Zero external runtime dependencies. Only Emacs builtins (`comint`,
  `cl-lib`, `subr-x`, `eldoc`).
- Discover server capabilities at connect time via `describe`. Don't
  hardcode "this op exists / doesn't exist" assumptions.

## Non-goals (for now)

- Feature parity with CIDER. No inspector, debugger, profiler,
  test-runner UI, structured stacktrace browser. Anything that
  requires a particular middleware on the server is out of scope.
- Automatic per-buffer connection routing in the UI. The buffer-local
  `neat-current-connection` mechanism is there for downstream code,
  but the user-facing flow ships with one global active connection
  and a manual picker.
- TLS / authenticated transports.
- Reconnect / connection-pool logic.
- A `neat-jack-in` style command that auto-detects project type and
  starts the server. Likely on the roadmap once the core stabilises.

## Module layout

`neat` is intentionally split into four files rather than one monolith.
This keeps the library/UI boundary visible and lets downstream packages
consume just the parts they need.

- `neat-bencode.el`: wire format codec. Pure functions, no I/O, no
  other module dependencies.
- `neat-client.el`: TCP connection, request dispatch, the standard
  nREPL ops (`describe`, `clone`, `eval`, `interrupt`, `close`,
  `completions`, `lookup`). Depends only on `neat-bencode`.
  UI-agnostic.
- `neat-repl.el`: comint-derived REPL buffer. Depends on
  `neat-client`.
- `neat.el`: entry point, customisation group, `neat-mode` minor
  mode, source-buffer evaluation commands, CAPF and eldoc backends.
  The "everything plugged together" file.

Library consumers (e.g. another Emacs package that wants to talk
nREPL) typically only need the first two.

## Key decisions

### Bencode: fresh implementation, stack-based decoder

monroe ships a small recursive bencode decoder; CIDER's is more robust
and optimised (stack-based, uses a tagged-list type for dicts so
ordering is preserved and `assoc` lookups don't dominate hot paths).
neat is a fresh implementation that borrows CIDER's robustness lessons
(stack-based parsing, streaming via a "truncated input -> nil" return
convention) while keeping the public API ergonomic for library use:

- `neat-bencode-encode` takes plain alists and lists; no flat
  key/value sequence trickery.
- `neat-bencode-get` provides accessor-style dict lookup so the
  underlying representation can be swapped (e.g. to a tagged list)
  later without breaking callers.

The decoder treats truncated input as a returnable signal (`nil`),
not an error. That makes it trivial to drive from a streaming network
filter: accumulate bytes, call `neat-bencode-decode`, on `nil` wait
for more, otherwise dispatch and recurse with the remainder.

### Asynchronous dispatch, single callback per request

`neat-send` takes one callback per request. That callback fires for
every response message sharing the request's `id`. Callers inspect
the response dict (`status`, `value`, `out`, `err`, `ns`, ...) and
decide what to do.

This matches monroe's model. Alternatives we considered:

- **Multi-callback** (`on-value`, `on-out`, `on-done`, ...). More
  "structured", but it commits us to a fixed set of fields and loses
  any future field the server might add.
- **Single batched callback at `done`**. Simpler for the caller but
  loses streaming, which is intolerable for long-running evals.

The single-callback model is what every responding nREPL client we
surveyed (monroe, CIDER, vim-fireplace) uses. We're not breaking
that pattern.

### Op discovery via `describe`, no hardcoded Clojurisms

nREPL is a protocol; servers advertise the ops they implement via the
`describe` op. neat sends `describe` on connect and stashes the response
on the connection. UI-level features like CAPF and eldoc consult that
capability map and silently no-op when the op they need is missing.
There are no hardcoded `clojure.repl/doc` forms, no assumption the
server runs on a JVM, no Leiningen or Clojure CLI defaults baked into
the connect command.

This is what "language-agnostic" actually means in practice: the
client doesn't carry the receiving language inside it.

### REPL via comint, with a pipe-process trick

The REPL buffer derives from `comint-mode` because `comint` is the
well-trodden Emacs pattern for "buffer with prompt, input history,
streaming output". But comint expects an actual subprocess attached
to the buffer. We don't have one. Our "process" is the nREPL TCP
connection, which lives outside the buffer.

The workaround: attach an idle `make-pipe-process` to the comint
buffer purely as a placeholder. It never sends or receives anything.
We override `comint-input-sender` so user input becomes an `eval` op
on the real nREPL connection, and use `comint-output-filter` to
write responses back into the buffer.

This gets us comint's input ring (and persistence via
`comint-input-ring-file-name`), read-only prompt handling, and line
editing for free, without inventing a brand new major mode.

### Multi-line input via `parse-partial-sexp`

`RET` doesn't blindly submit. It checks whether the pending input
balances under `neat-repl-input-syntax-table` (Emacs Lisp syntax by
default, which is close enough for any Lisp-family server). Unbalanced
parens or open strings -> insert a newline so the user can keep
typing. Balanced -> submit.

The syntax-table choice is a `defvar`, not a `defcustom`, on purpose:
it's expected to be set by whoever knows what language they talk to,
not toggled through `M-x customize`.

### Connection lifecycle and routing

Connections live in a global `neat-connections` list. `neat-connect`
pushes onto it; `neat-disconnect` and the process sentinel both
remove. `neat-default-connection` is the "active" connection; if it
dies, it demotes to the next-most-recent live one rather than
dangling at a dead struct.

Source buffers route through `neat-active-connection`, which checks
a buffer-local override (`neat-current-connection`) first and falls
back to the default. The UI ships with one user-facing command for
switching the default (`neat-set-default-connection`); per-buffer
overrides are there for downstream packages that want richer
routing logic.

### Targeting Emacs 28+

The minimum is Emacs 28.2, which buys us:

- `format-prompt` for default-aware minibuffer prompts.
- `eldoc-documentation-functions` (plural, the abnormal hook), so
  the eldoc backend can be async and never block point motion on a
  server roundtrip.
- `condition-case-unless-debug`, so `toggle-debug-on-error` surfaces
  callback bugs instead of being swallowed by the safety net.
- `make-pipe-process` for the REPL placeholder.

Supporting Emacs 27 would mean fallbacks for each of those and worse
UX where the modern API does the right thing. Not worth it for a
young project.

## Integration testing as a compatibility tool

The parameterised suite at `test/neat-integration-test.el` runs the
same assertions against every nREPL implementation whose executable
is on PATH. Today: Clojure (`nrepl/nrepl`), Babashka, Basilisp.

This isn't just nice-to-have; it's a first step toward a real nREPL
compatibility test suite. The contract under test is what we believe
any conformant server should support, and concrete divergences (for
example, Basilisp chunking `(println "hi")` into two `out` messages
where Clojure batches them into `"hi\n"`) are exactly the findings
that should feed back into the nREPL specification work.

Adding a new implementation is a single plist entry in
`neat-it--server-impls`: name, executable, command builder, and a
regex for the port banner.
