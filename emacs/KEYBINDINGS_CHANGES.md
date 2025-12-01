# Keybindings Refactoring - Change Log

## 🎯 Version 2.0 - Complete Refactoring

**Date**: 2025-12-01  
**Status**: ✅ Ready for Testing  
**Impact**: 🔴 **BREAKING CHANGES** - Muscle memory will need updating

---

## 📋 Change Summary

| Category | Old | New | Reason |
|----------|-----|-----|--------|
| **LSP Navigation** | M-g d/r/n | **M-. / M-, / M-?** | Standard LSP bindings |
| **LSP Docs** | M-g c | **C-h .** | Standard help prefix |
| **Git Status** | M-m s t | **C-x g** or **C-c g s** | Standard/Organized |
| **Git Ops** | M-m * | **C-c g *** | Organized prefix |
| **Tabs** | M-7/8, M-s * | **C-c t *** | No conflicts |
| **Markmacro** | M-m c * | **C-c m *** | Organized prefix |
| **Search** | M-m g * | **C-c s *** | Organized prefix |
| **Org** | C-c c/a/l | **C-c n *** | Organized prefix |

**Total Changes**: 60+ keybindings refactored

---

## 🔴 CRITICAL CHANGES (Breaking)

### 1. LSP Navigation - NOW STANDARD! ⚡

#### Before (❌ Non-standard)
```elisp
M-g d     → lsp-bridge-find-def
M-g D     → lsp-bridge-find-def-return
M-g r     → lsp-bridge-find-references
M-g c     → lsp-bridge-popup-documentation
M-g n     → lsp-bridge-rename
```

**Problem**: Conflicts with Emacs goto prefix (M-g g = goto-line)

#### After (✅ Standard LSP)
```elisp
M-.       → lsp-bridge-find-def
M-,       → lsp-bridge-find-def-return
M-?       → lsp-bridge-find-references
C-h .     → lsp-bridge-popup-documentation
C-c r r   → lsp-bridge-rename
C-c r i   → lsp-bridge-code-action
C-c r f   → lsp-bridge-code-format
```

**Benefits**:
- ✅ Standard across all LSP clients
- ✅ No conflicts with Emacs defaults
- ✅ Matches init-lsp-common.org
- ✅ Easier to remember (. = dot = definition)

---

### 2. Git (Magit) - REORGANIZED

#### Before (😵 Chaotic)
```elisp
M-m s t   → magit-status
M-m s a   → magit-submodule-add
M-m f r   → magit-file-rename
M-m b r   → magit-branch-rename
M-m l     → magit-log
M-m p f   → magit-pull
M-m p h   → magit-push
M-m m     → mcge-magit-menu
```

**Problem**: 13 bindings scattered under M-m, hard to remember

#### After (✅ Organized)
```elisp
# Primary (standard)
C-x g     → magit-status
C-x M-g   → magit-dispatch

# Alternative (organized)
C-c g s   → magit-status
C-c g l   → magit-log
C-c g d   → magit-dispatch
C-c g p   → magit-push
C-c g f   → magit-pull

# Branches
C-c g b b → magit-branch-create
C-c g b r → magit-branch-rename

# Submodules
C-c g m a → magit-submodule-add
C-c g m u → magit-submodule-update

# Menu
C-c g ?   → Git menu
```

**Benefits**:
- ✅ C-x g is standard (recommended by Magit)
- ✅ C-c g * is logical (g = git)
- ✅ Grouped by function
- ✅ Easy to discover with which-key

---

### 3. Tabs (Sort-tab) - NO MORE CONFLICTS

#### Before (⚠️ Conflicts)
```elisp
M-7       → sort-tab-select-prev-tab
M-8       → sort-tab-select-next-tab
M-s 7     → sort-tab-select-first-tab
M-s 8     → sort-tab-select-last-tab
C-;       → sort-tab-close-current-tab
M-s q     → sort-tab-close-other-tabs
```

**Problem**: M-s conflicts with search prefix

#### After (✅ Clean)
```elisp
C-c t n   → sort-tab-select-next-tab
C-c t p   → sort-tab-select-prev-tab
C-c t f   → sort-tab-select-first-tab
C-c t l   → sort-tab-select-last-tab
C-c t k   → sort-tab-close-current-tab
C-c t o   → sort-tab-close-other-tabs
C-c t K   → sort-tab-close-all-tabs

# Quick access by number
C-c t 1/2/3 → Jump to tab 1/2/3
```

**Benefits**:
- ✅ No conflicts
- ✅ Logical (t = tabs)
- ✅ Consistent with other C-c prefixes

---

### 4. Markmacro - MOVED TO C-c m

#### Before
```elisp
M-m c s   → markmacro-rect-set
M-m c d   → markmacro-rect-delete
M-m c a   → markmacro-apply-all
...
```

#### After
```elisp
C-c m s   → markmacro-rect-set
C-c m d   → markmacro-rect-delete
C-c m r   → markmacro-rect-replace
C-c m i   → markmacro-rect-insert
C-c m c   → markmacro-rect-mark-columns
C-c m a   → markmacro-apply-all
C-c m e   → markmacro-apply-all-except-first
```

**Benefits**:
- ✅ Logical (m = markmacro)
- ✅ Grouped together
- ✅ Easy to discover

---

### 5. Search - UNIFIED UNDER C-c s

#### Before (😵 Scattered)
```elisp
C-s        → consult-line (Embark)
C-S-y      → blink-search
M-m g g    → color-rg-search-symbol
M-m g j    → color-rg-search-symbol-in-project
C-c g      → consult-ripgrep
C-c f      → consult-find
C-c b      → consult-buffer
```

#### After (✅ Organized)
```elisp
# Buffer search
C-c s l    → consult-line
C-c s i    → consult-imenu
C-c s m    → consult-mark

# Project search
C-c s g    → consult-ripgrep
C-c s f    → consult-find
C-c s b    → consult-buffer

# Color-rg
C-c s s    → color-rg-search-symbol
C-c s S    → color-rg-search-symbol-in-project

# Blink search
C-c s B    → blink-search

# Menu
C-c s ?    → Search menu
```

**Benefits**:
- ✅ All search under one prefix
- ✅ Easy to discover
- ✅ Grouped by scope (buffer/project)

---

### 6. Org Mode - ORGANIZED UNDER C-c n

#### Before (Mixed)
```elisp
C-c c      → org-capture
C-c a      → org-agenda
C-c l      → org-store-link
C-c v      → org-insert-clipboard-image (in org-mode)
C-c e      → org-edit-src-code (in org-mode)
```

#### After (✅ Organized)
```elisp
C-c n c    → org-capture
C-c n a    → org-agenda
C-c n l    → org-store-link
C-c n t    → org-todo-list

# In org-mode buffers
C-c n e    → org-edit-src-code
C-c n v    → org-insert-clipboard-image
C-c n h    → org-insert-heading
C-c n s    → org-insert-subheading
```

**Benefits**:
- ✅ Logical (n = notes)
- ✅ All org functions grouped
- ✅ No conflicts

---

## 🆕 NEW FEATURES

### 1. Which-key Integration

Now all prefixes have descriptive labels:

```elisp
C-c g ?   → Shows: "git commands"
C-c m ?   → Shows: "markmacro commands"
C-c s ?   → Shows: "search commands"
C-c t ?   → Shows: "tab commands"
```

### 2. Transient Menus

Quick access menus for discoverability:

```elisp
C-c g ?   → Git menu (shows all git commands)
C-c s ?   → Search menu (shows all search options)
```

### 3. Quick Reference

```elisp
C-h K     → Show keybinding quick reference
```

Displays a beautiful cheat sheet with all important bindings!

### 4. Better Line Movement

```elisp
M-<up>    → Move line up
M-<down>  → Move line down
```

More intuitive than the old `M-m p n/p p`

---

## 📊 Full Comparison Table

| Function | Old | New | Type |
|----------|-----|-----|------|
| **LSP: Go to def** | M-g d | M-. | Changed |
| **LSP: Return** | M-g D | M-, | Changed |
| **LSP: References** | M-g r | M-? | Changed |
| **LSP: Doc** | M-g c | C-h . | Changed |
| **LSP: Rename** | M-g n | C-c r r | Changed |
| **LSP: Format** | - | C-c r f | New |
| **LSP: Action** | - | C-c r i | New |
| **Git: Status** | M-m s t | C-x g, C-c g s | Changed |
| **Git: Log** | M-m l | C-c g l | Changed |
| **Git: Push** | M-m p h | C-c g p | Changed |
| **Git: Pull** | M-m p f | C-c g f | Changed |
| **Tab: Next** | M-8 | C-c t n | Changed |
| **Tab: Prev** | M-7 | C-c t p | Changed |
| **Tab: Close** | C-; | C-c t k | Changed |
| **Mark: Set** | M-m c s | C-c m s | Changed |
| **Mark: Apply** | M-m c a | C-c m a | Changed |
| **Search: Line** | C-s | C-c s l | Changed |
| **Search: Grep** | C-c g | C-c s g | Changed |
| **Org: Capture** | C-c c | C-c n c | Changed |
| **Org: Agenda** | C-c a | C-c n a | Changed |
| **Move: Line up** | M-m p p | M-<up> | Changed |
| **Move: Line down** | M-m p n | M-<down> | Changed |
| **Scroll: Up** | M-n | M-n | Same |
| **Scroll: Down** | M-p | M-p | Same |
| **Smart: C-a** | C-a | C-a | Same |

---

## 🎯 Migration Guide

### Day 1: LSP Bindings (Most Important)

**Old habits to break**:
- ❌ M-g d → definition
- ❌ M-g r → references

**New muscle memory**:
- ✅ M-. → definition (think: dot = definition point)
- ✅ M-, → return (think: comma = go back)
- ✅ M-? → references (think: ? = where is it?)

**Tip**: Put a sticky note on your monitor: "M-. not M-g d"

### Day 2-3: Git Commands

**Old habits**:
- ❌ M-m s t → status

**New muscle memory**:
- ✅ C-x g → status (standard!)
- ✅ C-c g s → status (alternative)

**Tip**: Just use C-x g, it's standard and easy

### Day 4-5: Search and Other

**Old habits**:
- ❌ C-c g → grep
- ❌ M-m g g → color-rg

**New muscle memory**:
- ✅ C-c s g → grep
- ✅ C-c s s → color-rg symbol

**Tip**: Everything search-related is now under C-c s

### Week 2: Refined Usage

By now you should be comfortable. Start using:
- C-c g ? → Git menu (when you forget)
- C-c s ? → Search menu (when you forget)
- C-h K → Quick reference

---

## 🔧 Testing Checklist

Before using in production:

- [ ] Test LSP navigation (M-., M-,, M-?)
- [ ] Test LSP documentation (C-h .)
- [ ] Test LSP rename (C-c r r)
- [ ] Test Git status (C-x g)
- [ ] Test Git operations (C-c g l/p/f)
- [ ] Test tab navigation (C-c t n/p)
- [ ] Test search (C-c s l/g/b)
- [ ] Test org capture (C-c n c)
- [ ] Test line movement (M-<up/down>)
- [ ] Test which-key (C-c g ? shows menu)
- [ ] Test quick reference (C-h K)

---

## 📈 Benefits Summary

### Before
- 😵 M-m used for 30+ unrelated commands
- ⚠️ M-g conflicts with Emacs defaults
- ⚠️ M-s conflicts with search prefix
- 🤔 Hard to discover commands
- 😓 Hard to remember bindings

### After
- ✅ Logical prefixes (C-c g/m/s/t/n)
- ✅ Standard LSP bindings (M-., M-,)
- ✅ No conflicts with Emacs
- ✅ Which-key integration
- ✅ Transient menus for discovery
- ✅ Easy to remember (semantic grouping)
- ✅ Quick reference (C-h K)

---

## 🚀 Rollout Plan

### Phase 1: Backup (Now)
```bash
cp init-keymaps.org init-keymaps.org.backup
```

### Phase 2: Apply (Testing)
```bash
mv init-keymaps-v2.org init-keymaps.org
cd emacs && make clean && make generate
```

### Phase 3: Test (1-2 days)
- Use new bindings
- Report issues
- Adjust if needed

### Phase 4: Commit (When confident)
```bash
git add .
git commit -m ":sparkles: refactor(keys): v2.0 - standard LSP & organized prefixes"
```

---

## ⚠️ Known Trade-offs

### Must Relearn
- **LSP navigation**: M-g → M-.
- **Git status**: M-m s t → C-x g
- **Tab navigation**: M-7/8 → C-c t n/p

**Estimated adaptation time**: 3-7 days

### Slightly More Keystrokes
- **Before**: M-m s t (3 keys)
- **After**: C-c g s (3 keys) OR C-x g (2 keys)

**Net change**: Neutral or better!

### Positive Trade-offs
- ✅ More consistent
- ✅ Better discoverability
- ✅ No conflicts
- ✅ Easier to teach others

---

## 🎓 Additional Notes

### Why M-. for Go-to-definition?

This is the **de facto standard** across:
- lsp-mode (Emacs)
- eglot (Emacs)
- VSCode (with vim emulation)
- IntelliJ IDEA (with vim plugin)
- Most LSP clients

**Benefit**: If you use multiple editors, same muscle memory!

### Why C-c Prefix?

`C-c <letter>` is **reserved for user bindings** in Emacs conventions.
- Safe from conflicts
- Won't break with Emacs updates
- Expected by experienced Emacs users

### Can I Keep Some Old Bindings?

Yes! The file is organized. You can uncomment alternatives:

```elisp
;; Keep M-7/M-8 for tabs if you prefer
;; (global-set-key (kbd "M-7") 'sort-tab-select-prev-tab)
;; (global-set-key (kbd "M-8") 'sort-tab-select-next-tab)
```

---

## 🎉 Conclusion

**This refactoring**:
- ✅ Fixes 3 critical conflicts
- ✅ Organizes 60+ bindings logically
- ✅ Adds discoverability features
- ✅ Follows Emacs/LSP standards
- ✅ Improves long-term usability

**Estimated effort to adapt**: 1 week  
**Long-term benefit**: **HUGE** 🚀

---

**Ready to apply? Let's do it!** 🎯
