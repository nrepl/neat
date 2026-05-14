# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial project skeleton with Eldev and Buttercup.
- `neat-bencode` module: encode/decode for the bencode wire format used by nREPL.
- `neat-client` module: connection management, request dispatch, and the core nREPL ops (`describe`, `clone`, `eval`, `interrupt`, `close`).
- `neat-repl` module: comint-derived REPL buffer.
- `neat-mode` minor mode with bindings for source-buffer evaluation.
- Integration test suite that boots a real nREPL server via the Clojure CLI. Run with `NEAT_INTEGRATION=1 eldev test`.
- `neat-completions` and `neat-lookup` library ops (plus blocking `*-sync` variants).
- Completion-at-point and eldoc integration for `neat-mode` source buffers. Both require `cider-nrepl` (or a compatible middleware) on the server side.
- REPL buffer styling: distinct faces for `out`, `err`, and `value` streams.
- `.nrepl-port` discovery: `M-x neat` defaults the port to whatever the nearest port file contains, so in a project with a running server `M-x neat RET RET` is enough. Customize via `neat-port-file-name`; library entry points are `neat-discover-port` and `neat-discover-port-file`.
- Multi-connection support: `neat-connections` tracks every live `neat-connection`; `neat-set-default-connection` is an interactive picker that switches which one source buffers (running `neat-mode`) talk to. Connections drop out of the registry automatically on disconnect or server death, and the default demotes to the next-most-recent live connection if it goes away.
- Integration test suite now parameterised over nREPL implementations: `neat-it--server-impls` describes each, and any one whose executable is on PATH gets its own `describe` block. Ships with entries for Clojure and Babashka.
- REPL: multi-line input. `RET` only submits when the form parses as balanced; otherwise it inserts a newline. Balance check uses `neat-repl-input-syntax-table` (Emacs Lisp by default; override for languages with very different bracketing rules).
- REPL: input history persistence. New `neat-repl-history-file` defaults to `~/.emacs.d/neat-repl-history`; history is loaded on REPL start and saved on buffer kill. Set to nil to disable.
- REPL: namespace-aware prompt. The prompt is now derived from `neat-repl-prompt-format` (default `"%s> "`) and updates in response to the server's `ns` field, so `user> ` becomes `myapp.core> ` after `(in-ns 'myapp.core)`. `neat-repl-default-ns` controls what appears before the server has reported one.

### Removed

- `neat-repl-prompt` (defcustom). Replaced by `neat-repl-prompt-format` and `neat-repl-default-ns`. There were no released versions, so this is just churn within the unreleased changelog window.
