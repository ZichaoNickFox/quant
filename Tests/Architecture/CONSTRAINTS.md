# Architecture Constraints

This folder defines executable architecture guardrails for contributors and AI agents.

Legend: `[x]` = has architecture test case, `[ ]` = no architecture test case yet.

## Category Alignment (Required)

1. [ ] Constraint categories in this document must stay aligned with architecture test categories.

## Architecture Test Layout Constraints

1. [x] Architecture specs must stay flattened directly under `Tests/Architecture/` (no nested subfolders for specs).

## Dependency Installation Constraints

1. [x] Tool/dependency installation commands must be managed in Nix files, not runtime scripts (for example: `run`, `test`, `e2e`, `smoke`, `push`).

## Controller Constraints

1. [ ] `PageController` is the only controller allowed to render HTML shell pages.
2. [x] Non-page controllers (except `NotifyController`) must call `renderJson`.
3. [x] `NotifyController` must not call `renderJson`, `renderHTML`/`renderHtml`, or `redirect*`.

## Naming Constraints

1. [x] Frontend request methods to backend must be named with `request*`, not `fetch*`.
2. [x] Backend methods that fetch external network data must be named with `fetch*`.

## Purescript Style Constraints

1. [x] All `import` and module `export` items must be sorted alphabetically (case-insensitive).

## FRP Constraints

1. [x] In `purescript/src`, creation interfaces must use `createFRP` naming.
2. [x] In `purescript/src` files that define `createFRP`, `Events` and `Config` must be declared above `createFRP` (and `Events` must stay above `Config` if both exist).

## Protocol Constraints

1. [x] Protocol definition and ser/de code must live only in:
   `proto/`, `purescript/src/Proto/`, `proto/Purescript-Bridge.hs`, or `Web/Fetcher/`.
2. [x] `proto/` and `purescript/src/Proto/` must stay mechanical only:
   no business logic, no dependency on business/application modules.

## Purescript Test Directory Constraints

1. [x] `purescript/test/Unit/` and `purescript/test/Integration/` directory trees must mirror `purescript/src/`.
