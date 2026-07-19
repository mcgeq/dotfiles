# Module Load Strategy Metadata Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move Emacs module loading strategy from scattered global lists into per-module metadata declared in `mcg!`.

**Architecture:** Extend `core-modules` with a metadata registry while keeping the existing enabled/disabled/load-order registries compatible. `init.org` will declare each module as either a bare symbol or `(module :load STRATEGY)`, and startup will derive immediate, idle, and deferred loading from metadata.

**Tech Stack:** Emacs Lisp, Org tangling, ERT, Make.

---

### Task 1: Add Module Metadata Parsing

**Files:**
- Modify: `emacs/config-org/core/core-modules.org`
- Test: `emacs/test/run-tests.el`

- [ ] **Step 1: Add tests for metadata syntax**

Add ERT coverage showing that `mcg!` accepts bare modules, disabled modules, and `(module :load idle)` entries while preserving existing registries.

- [ ] **Step 2: Implement metadata registry**

Add `mcg-module-metadata-alist`, parse list entries, and expose `mcg-module-load-strategy`.

- [ ] **Step 3: Preserve compatibility**

Bare module declarations default to `immediate`; disabled modules remain excluded from enabled/load-order registries.

### Task 2: Derive Startup Queues From Metadata

**Files:**
- Modify: `emacs/config-org/core/core-modules.org`
- Modify: `emacs/config-org/init.org`
- Test: `emacs/test/run-tests.el`

- [ ] **Step 1: Add query helpers**

Add `mcg-list-modules-by-load-strategy` and use it for immediate, idle, and deferred setup.

- [ ] **Step 2: Replace scattered strategy variables**

Remove `mcg-immediate-categories`, `mcg-idle-modules`, `mcg-immediate-skip-modules`, and `mcg-deferred-categories` from the active startup path.

- [ ] **Step 3: Move current strategy into module declarations**

Mark current idle modules with `:load idle` and language/writing trigger modules with `:load deferred`.

### Task 3: Generate And Verify

**Files:**
- Generate: `emacs/site-lisp/config/core/core-modules.el`
- Generate: `emacs/site-lisp/config/init.el`

- [ ] **Step 1: Tangle generated files**

Run `make all` from `emacs/`.

- [ ] **Step 2: Verify behavior**

Run targeted ERT tests for module metadata and startup strategy, then run `make test`.

- [ ] **Step 3: Verify loading**

Run `emacs.exe -Q --batch -l emacs/site-lisp/config/init.el` and `git diff --check`.

### Self-Review

- Spec coverage: the plan covers parser compatibility, metadata lookup, startup queue derivation, migration, generation, and verification.
- Placeholder scan: no placeholders remain.
- Type consistency: strategy names are symbols: `immediate`, `idle`, and `deferred`.
