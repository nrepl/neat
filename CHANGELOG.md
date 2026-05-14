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
