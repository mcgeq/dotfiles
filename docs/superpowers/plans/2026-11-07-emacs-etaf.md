# ETAF Integration Implementation Plan

Goal: expose the official ETAF gallery, manual and Markdown preview through optional, offline-at-startup Emacs configuration.

Approved design: package.el owns ETAF dependencies in the existing package-user-dir. The lightweight tools/etaf module registers M-x commands without activating packages, downloading files or opening applications at startup. Only mcg-etaf-install refreshes archives and installs the official installer plus the three targets. Open commands require an already installed target and activate it on demand. Reject conflicting source copies before package activation. Preserve existing package archives and signature/TLS policies. Do not add etaf-posframe, alter existing posframe, or install external backends.

Files: config-org/modules/tools/init-etaf.org owns options, package activation, install/open/manage/doctor commands and health checks. config-org/init.org enables the lightweight module. core-doctor includes its local health result when available. Both READMEs and the private example explain commands and lifecycle. test/etaf-tests.el covers behavior; a fresh-process installed-package smoke verifies public application entrypoints.

- [x] Write behavioral tests for offline/repeatable module setup, missing packages, selective activation, installation targets, source conflicts and non-mutating diagnostics; observe failures.
- [x] Implement init-etaf and register the module/doctor integration. Generate repository-local outputs.
- [x] Run tests, explicitly install the approved targets through package.el, and verify actual installed APIs/rendering in an isolated process.
- [x] Generate and verify the final management-entry correction with the full suite and manager-first installed smoke.

Validation: make test, existing startup/workflow smokes, official etaf-install-doctor, and installed-package smoke. Keep graphical rendering limitations explicit. No commit or push is requested.

Verification evidence: the initial seven ETAF regressions failed before implementation. Full ERT subsequently passed 266/266. Installed etaf-install 0.2.16, etaf-ui 0.3.3, etaf-manual 0.1.11 and etaf-md-preview 0.1.9 plus dependencies through the official archive into the existing user elpa directory. Fresh-process offline smoke rendered all three applications and passed official doctor checks. Startup key preservation/first-use compile smoke passed. No GUI visual verification was performed.

The real package smoke exposed an Emacs 32 two-stage autoload issue, resolved by requiring each public package facade before interactive invocation. Markdown's official file prompt is retained and simulated in the smoke.

Independent review found that the official manager calls application commands directly without activating targets. Added selective activation/loading of installed targets when opening the manager, an eighth unit regression and a manager-first installed smoke. Final verification on 2026-11-07 regenerated the module and passed make test: 267/267 results expected, zero unexpected. The fresh-process test/etaf-installed-smoke.el also passed with the manager opened first, all three applications rendered, and offline doctor checks successful. git diff --check passed. No GUI visual verification was performed. No commits or staging changes were made.
