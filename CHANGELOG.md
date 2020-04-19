# Changelog

## 0.5.0 (2020-04-19)

### New features

* `arrudeia.core/thread-first-macro-builder` macro enables you to create your own
macro and reader.

## 0.4.0 (2020-04-19)

### Breaking changes

* `arrudeia.core/*bypass*` is true by default.

## 0.3.3 (2020-04-19)

### Fixes

* Do not cancel procs at `run-processes!`.
* Step could be used again at pipeline, so we reset its value.

## 0.3.2 (2020-04-19)

### Fixes

* Return steps results at `run-processes!`.

## 0.3.1 (2020-04-18)

### Fixes

* Build and deploy uberjar instead of thin jar.

## 0.3.0 (2020-04-18)

### New features

* Added `ar/->*` tagged literal, it replaces all `clojure.core/->` for
`arrudeia.core/->*` at the following form.
