# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- `neat-load-file` library op and `neat-load-buffer-file` interactive command (bound to `C-c C-l`). Uses the standard nREPL `load-file` op, which carries the buffer's path and filename so the server can attribute file and line numbers to errors. Distinct from `neat-eval-buffer`, which still ships the buffer as a plain `eval`.
- `neat-eval` now accepts optional `file`, `line`, and `column` arguments. The source-buffer eval commands (`neat-eval-last-sexp`, `neat-eval-defun`, `neat-eval-region`, `neat-eval-buffer`) compute these from the buffer and send them, so the server can point error messages at the actual source location instead of an anonymous string.
- xref backend in `neat-mode` and `neat-repl-mode` buffers. `M-.` (`xref-find-definitions`) asks the server's `lookup` op where the symbol at point is defined and jumps there; `M-,` pops back. Customize the request timeout via `neat-lookup-timeout`. Sources behind URLs we can't resolve locally (`jar:...`, `http:...`, ...) yield no result; jar extraction and remote-path translation are out of scope.
- Completion candidates carry the server-reported `type` (function, macro, var, ...) via `:annotation-function`, so the completion UI shows it next to each candidate.
- `stdin` op + `need-input` handling. When evaluated code blocks on a read (`(read-line)`, Python `input`, ...), the server responds with a `need-input` status; neat prompts in the minibuffer and ships the answer (plus a trailing newline) via the standard `stdin` op. `C-g` at the prompt interrupts the eval instead. Library function: `neat-stdin`.
- Mode-line shows the active connection. In `neat-mode` buffers the lighter becomes ` neat[host:port]`, or ` neat[closed]` after the connection dies; in `neat-repl-mode` the same indicator replaces the misleading comint `:run` tag (the REPL's pipe process is a placeholder, not the actual network connection).
- `neat-disconnect-functions` abnormal hook fires when a connection's process dies. The REPL uses it to insert a `;; connection closed` marker into the buffer and refuse further input, so a dead connection is visible rather than silently swallowing keystrokes.
- Malformed bencode from the server no longer kills the process filter. The drain catches `neat-bencode-error`, logs the offending bytes to the message log, clears the recv buffer, and reports the drop via `message`. The filter survives so subsequent valid messages still dispatch.
- `neat-repl-clear-buffer` (bound to `C-c M-o` in the REPL) wipes the REPL buffer and reinserts a fresh prompt without touching the input ring or the underlying connection.
- Buffer-local namespace for source-buffer evaluations: `neat-ns` (defvar-local) and `neat-set-ns` interactive command bound to `C-c M-n`. The namespace is sent as the `ns` field on `eval` ops, so the server runs the code in the right place. `neat-buffer-ns-function` is the swappable seam for languages where the namespace can be derived from the buffer (e.g., parsing a `(ns foo.bar)` form); the default just returns `neat-ns`.
- Initial project skeleton with Eldev and Buttercup.
- `neat-bencode` module: encode/decode for the bencode wire format used by nREPL.
- `neat-client` module: connection management, request dispatch, and the core nREPL ops (`describe`, `clone`, `eval`, `interrupt`, `close`).
- `neat-repl` module: comint-derived REPL buffer.
- `neat-mode` minor mode with bindings for source-buffer evaluation.
- Integration test suite that boots a real nREPL server via the Clojure CLI. Run with `NEAT_INTEGRATION=1 eldev test`.
- `neat-completions` and `neat-lookup` library ops (plus blocking `*-sync` variants).
- Completion-at-point and eldoc integration for `neat-mode` source buffers, driven by the standard `completions` and `lookup` nREPL ops.
- REPL buffer styling: distinct faces for `out`, `err`, and `value` streams.
- `.nrepl-port` discovery: `M-x neat` defaults the port to whatever the nearest port file contains, so in a project with a running server `M-x neat RET RET` is enough. Customize via `neat-port-file-name`; library entry points are `neat-discover-port` and `neat-discover-port-file`.
- Multi-connection support: `neat-connections` tracks every live `neat-connection`; `neat-set-default-connection` is an interactive picker that switches which one source buffers (running `neat-mode`) talk to. Connections drop out of the registry automatically on disconnect or server death, and the default demotes to the next-most-recent live connection if it goes away.
- Integration test suite now parameterised over nREPL implementations: `neat-it--server-impls` describes each, and any one whose executable is on PATH gets its own `describe` block. Ships with entries for Clojure, Babashka, and Basilisp (a Clojure-flavored Python).
- REPL: multi-line input. `RET` only submits when the form parses as balanced; otherwise it inserts a newline. Balance check uses `neat-repl-input-syntax-table` (Emacs Lisp by default; override for languages with very different bracketing rules).
- REPL: input history persistence. New `neat-repl-history-file` defaults to `~/.emacs.d/neat-repl-history`; history is loaded on REPL start and saved on buffer kill. Set to nil to disable.
- REPL: namespace-aware prompt. The prompt is now derived from `neat-repl-prompt-format` (default `"%s> "`) and updates in response to the server's `ns` field, so `user> ` becomes `myapp.core> ` after `(in-ns 'myapp.core)`. `neat-repl-default-ns` controls what appears before the server has reported one.
- REPL: completion-at-point and eldoc are now also active in the REPL buffer, not just in source buffers running `neat-mode`. Same backends, same caveats (server must implement `completions` / `lookup`).
- Eldoc highlights the current argument in the displayed arglist. When point sits inside a call like `(map f |coll)`, the param matching the cursor position is wrapped in `eldoc-highlight-function-argument`. Multi-arity arglists pick whichever arity fits the current arg count; variadic arities highlight the rest param once you're past the fixed slots. Two seams keep this language-agnostic: `neat-eldoc-arg-index-function` (defaults to a `forward-sexp`-based walk) and `neat-eldoc-arglist-formatter` (defaults to a Clojure-shape `[a b & rest]` parser). Destructuring forms containing maps fall back to the unhighlighted arglist.

### Changed

- `neat-eval` and `neat-load-file` now take a property list of options after the required positional args, instead of stacking more and more positional `&optional`s. Migration: `(neat-eval conn code nil callback)` becomes `(neat-eval conn code :callback callback)`; `(neat-load-file conn contents file-path file-name)` becomes `(neat-load-file conn contents :file-path path :file-name name)`. Both ops accept `:session` and `:callback`; `eval` additionally accepts `:ns`, `:file`, `:line`, `:column`.
- Connection routing primitives (`neat-default-connection`, `neat-current-connection`, `neat-active-connection`) moved from `neat.el` to `neat-client.el`, where they belong as a library-level concern. The REPL buffer's `neat-repl-connection` defvar-local has been removed; the REPL now sets `neat-current-connection` directly, unifying the routing model across all buffer types.

### Removed

- `neat-repl-prompt` (defcustom). Replaced by `neat-repl-prompt-format` and `neat-repl-default-ns`. There were no released versions, so this is just churn within the unreleased changelog window.
