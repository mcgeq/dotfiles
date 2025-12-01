# Keybindings Configuration Analysis

## 📊 Overall Assessment

**Current State**: 🟡 Functional but needs optimization  
**Total Bindings**: ~60+ keybindings  
**Organization**: 7/10 - Good categorization, some issues  
**Consistency**: 5/10 - Mixed patterns  
**Conflicts**: ⚠️ Some potential conflicts detected  

---

## 🎯 Keybindings Inventory

### Category Breakdown

| Category | Count | Prefix | Status |
|----------|-------|--------|--------|
| **Basic Editing** | 8 | C-a, C-c | ✅ Good |
| **Magit (Git)** | 13 | M-m | ⚠️ Too many |
| **Sort-tab** | 7 | M-7/8, M-s | ⚠️ Conflicts |
| **LSP Bridge** | 10 | M-g | ⚠️ Conflicts |
| **Embark/Consult** | 12 | C-., C-c | ✅ Good |
| **Search Tools** | 8 | M-m g, C-S-y | ⚠️ Confusing |
| **Org Mode** | 6 | C-c | ✅ Standard |
| **Others** | 8 | Various | Mixed |

**Total**: ~72 keybindings

---

## 🔍 Detailed Analysis

### ✅ Strengths

#### 1. **Good Lazy Loading**
```elisp
(lazy-load-global-keys '(("M-m s t" . magit-status)) "init-magit")
```
- ✅ Deferred loading
- ✅ Reduces startup time
- ✅ Clean organization

#### 2. **Smart Custom Functions**
```elisp
;; Custom C-a behavior
(global-set-key (kbd "C-a") 'mcge-smart-move-beginning-of-line)

;; Custom scrolling
(global-set-key (kbd "M-n") 'next-multilines)
(global-set-key (kbd "M-p") 'previous-multilines)
```
- ✅ Enhanced usability
- ✅ Vim-like smooth scrolling

#### 3. **Logical Grouping**
- Clear sections for each tool
- Easy to find related bindings
- Good documentation

---

## ⚠️ Issues & Problems

### 🔴 Critical Issues

#### 1. **M-m Prefix Overuse**

**Problem**: M-m is used for too many different things

```elisp
M-m s *   - Magit status/submodules (13 bindings)
M-m f *   - Magit files
M-m b *   - Magit branches
M-m r *   - Magit remotes
M-m p *   - Magit pull/push + Move line (CONFLICT!)
M-m c *   - Markmacro (8 bindings)
M-m g *   - Color-rg (6 bindings)
M-m m     - Magit menu
```

**Result**: 
- 😵 Confusing mental model
- ⚠️ `M-m p` conflicts (magit vs move-line)
- 🤯 Hard to remember

**Recommendation**: Split into dedicated prefixes
```elisp
C-c g *   - Git (magit)
C-c m *   - Markmacro
C-c s *   - Search (color-rg)
```

---

#### 2. **M-g Conflicts**

**Current LSP bindings**:
```elisp
M-g d    - lsp-bridge-find-def
M-g o    - lsp-bridge-find-def-other-window
M-g c    - lsp-bridge-popup-documentation
...
```

**Problem**: 
- M-g is **Emacs built-in prefix** for `goto-*` commands!
- Conflicts with:
  - `M-g M-g` / `M-g g` - goto-line
  - `M-g n` / `M-g p` - next/previous-error
  - `M-g c` - goto-char

**Impact**: 🔴 **HIGH** - Breaking standard Emacs conventions

**Recommendation**: Use the LSP common bindings we defined earlier:
```elisp
M-.      - Go to definition
M-,      - Return from definition
M-?      - Find references
C-h .    - Documentation
```

---

#### 3. **Sort-tab Key Conflicts**

```elisp
M-7, M-8          - Tab navigation
M-s 7, M-s 8      - First/last tab
M-s q, M-s Q      - Close tabs
```

**Problems**:
- `M-s` is **search prefix** in standard Emacs
- `M-s o` - occur
- `M-s w` - isearch-forward-word
- etc.

**Impact**: 🟡 **MEDIUM** - May break expected behavior

---

#### 4. **Inconsistent Pattern Usage**

**Lambda functions inline**:
```elisp
;; Good for one-off bindings
(global-set-key (kbd "C-c <down>")
                (lambda () (interactive) ...))
```

**vs Named functions**:
```elisp
;; Better for reusability
(defun previous-multilines () ...)
(global-set-key (kbd "M-p") 'previous-multilines)
```

**Issue**: No clear pattern when to use which

---

### 🟡 Medium Issues

#### 5. **Missing Discoverable Prefixes**

**Current situation**: 
- No which-key integration
- Hard to discover what M-m s * does
- No hints for nested bindings

**Recommendation**: Add which-key descriptions

#### 6. **Crowded C-c Prefix**

```elisp
C-c r    - replace-string
C-c d    - make-directory
C-c <down> / <up> - insert newline
C-c C-r  - recentf
C-c C-n  - yas-new-snippet
C-c c    - org-capture
C-c l    - org-store-link
C-c a    - org-agenda
C-c e    - org-edit-src-code
C-c v    - org-insert-clipboard-image
C-c g    - consult-ripgrep
C-c f    - consult-find
C-c b    - consult-buffer
C-c n h  - consult-find-org-headings
```

**Issue**: Too many unrelated functions on C-c

#### 7. **Duplicate/Overlapping Search**

Three search mechanisms:
```elisp
C-s         - consult-line       (Embark)
C-S-y       - blink-search       (Blink Search)
M-m g *     - color-rg-*         (Color-rg)
```

**Confusion**: When to use which?

---

## 🎯 Optimization Recommendations

### High Priority Fixes

#### 1. **Resolve M-g Conflicts**

Replace LSP bindings with standard ones:

```elisp
;; OLD (conflicts with Emacs defaults)
M-g d    - lsp-bridge-find-def

;; NEW (standard LSP bindings)
M-.      - lsp-bridge-find-def
M-,      - lsp-bridge-find-def-return
M-?      - lsp-bridge-find-references
C-h .    - lsp-bridge-popup-documentation
```

**Impact**: ✅ Compatible with standard Emacs + LSP convention

---

#### 2. **Reorganize M-m Prefix**

**Current chaos**:
```
M-m = Magit + Markmacro + Color-rg + Move-line
```

**Proposed clean structure**:
```elisp
;; Git operations
C-c g s    - magit-status
C-c g l    - magit-log
C-c g p    - magit-push
C-c g f    - magit-pull

;; OR use dedicated prefix
C-x g      - magit-status (standard)

;; Markmacro
C-c m s    - markmacro-rect-set
C-c m a    - markmacro-apply-all

;; Color-rg (keep M-m g for search)
M-s g      - color-rg-search-symbol
M-s G      - color-rg-search-input
```

---

#### 3. **Fix Sort-tab Conflicts**

**Current**:
```elisp
M-s 7, M-s 8  - Conflicts with M-s search prefix
```

**Better alternatives**:
```elisp
Option 1: Use C-TAB (standard for tabs)
C-TAB           - sort-tab-select-next-tab
C-S-TAB         - sort-tab-select-prev-tab
C-x k           - sort-tab-close-current-tab

Option 2: Use dedicated prefix
C-c t n         - next-tab
C-c t p         - prev-tab
C-c t k         - close-tab
```

---

### Medium Priority Improvements

#### 4. **Add Which-key Integration**

```elisp
(which-key-add-key-based-replacements
  "C-c g" "git"
  "C-c m" "markmacro"
  "C-c n" "notes/org"
  "M-g" "goto")
```

#### 5. **Consolidate Search Commands**

Create a search menu:
```elisp
(transient-define-prefix mcg-search-menu ()
  "Search commands"
  [["Buffer"
    ("l" "Line" consult-line)
    ("i" "Imenu" consult-imenu)]
   ["Project"
    ("g" "Grep" consult-ripgrep)
    ("s" "Symbol" color-rg-search-symbol-in-project)]
   ["Blink"
    ("b" "Blink Search" blink-search)]])

(global-set-key (kbd "C-c s") 'mcg-search-menu)
```

---

## 📋 Proposed Keybinding Scheme

### Reorganized Prefixes

```elisp
┌─────────────────────────────────────────────────┐
│              Prefix Organization                 │
├─────────────────────────────────────────────────┤
│ C-c g *    → Git (Magit)                        │
│ C-c m *    → Markmacro                          │
│ C-c s *    → Search menu                        │
│ C-c n *    → Notes/Org                          │
│ C-c t *    → Tabs                               │
│ C-c p *    → Project                            │
│                                                  │
│ M-. / M-,  → LSP navigation (standard)          │
│ C-h .      → LSP documentation                  │
│                                                  │
│ C-c <up>   → Line manipulation (keep)           │
│ C-c <down> → Line manipulation (keep)           │
└─────────────────────────────────────────────────┘
```

---

## 🔧 Specific Recommendations

### 1. **LSP Bindings** (HIGH PRIORITY)

**Replace**:
```elisp
(global-set-key (kbd "M-g d")  'lsp-bridge-find-def)
(global-set-key (kbd "M-g D")  'lsp-bridge-find-def-return)
(global-set-key (kbd "M-g r")  'lsp-bridge-find-references)
```

**With**:
```elisp
(global-set-key (kbd "M-.")    'lsp-bridge-find-def)
(global-set-key (kbd "M-,")    'lsp-bridge-find-def-return)
(global-set-key (kbd "M-?")    'lsp-bridge-find-references)
(global-set-key (kbd "C-h .")  'lsp-bridge-popup-documentation)
```

✅ Aligns with `init-lsp-common.org` we created!

---

### 2. **Magit Bindings** (MEDIUM PRIORITY)

**Current**: Too spread out under M-m

**Option A - Use standard C-x g**:
```elisp
(global-set-key (kbd "C-x g")   'magit-status)  ;; Standard
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
```

**Option B - Use C-c g prefix**:
```elisp
(lazy-load-global-keys
 '(("C-c g s" . magit-status)
   ("C-c g l" . magit-log)
   ("C-c g d" . magit-dispatch)
   ("C-c g f" . magit-file-dispatch))
 "init-magit")
```

---

### 3. **Sort-tab Bindings** (MEDIUM PRIORITY)

**Replace**:
```elisp
("M-7" . sort-tab-select-prev-tab)
("M-8" . sort-tab-select-next-tab)
("M-s 7" . sort-tab-select-first-tab)  ;; Conflicts!
```

**With**:
```elisp
("C-c t p" . sort-tab-select-prev-tab)
("C-c t n" . sort-tab-select-next-tab)
("C-c t f" . sort-tab-select-first-tab)
("C-c t l" . sort-tab-select-last-tab)
("C-c t k" . sort-tab-close-current-tab)
```

---

## 📊 Before & After Comparison

### LSP Navigation

| Command | Before (❌ Conflicts) | After (✅ Standard) |
|---------|---------------------|-------------------|
| Go to definition | M-g d | **M-.** |
| Return | M-g D | **M-,** |
| References | M-g r | **M-?** |
| Documentation | M-g c | **C-h .** |
| Rename | M-g n | **C-c r r** |

### Magit

| Command | Before (😵 Confusing) | After (✅ Clear) |
|---------|---------------------|-----------------|
| Status | M-m s t | **C-x g** or **C-c g s** |
| Log | M-m l | **C-c g l** |
| Push | M-m p h | **C-c g p** |
| Pull | M-m p f | **C-c g f** |

---

## 🎯 Action Plan

### Phase 1: Critical Fixes (Do Now)

1. **Fix LSP bindings** - Use M-., M-, , M-?
2. **Resolve M-m conflicts** - Move magit to C-c g or C-x g
3. **Update init-lsp-common** - Ensure consistency

### Phase 2: Improvements (This Week)

4. **Fix sort-tab bindings** - Move to C-c t
5. **Add which-key descriptions**
6. **Create search menu**

### Phase 3: Polish (Optional)

7. **Consolidate C-c bindings**
8. **Add hydra/transient menus**
9. **Document all bindings**

---

## 📝 Implementation Priority

| Issue | Impact | Effort | Priority | Fix |
|-------|--------|--------|----------|-----|
| **M-g LSP conflicts** | 🔴 HIGH | LOW | 🔥 **URGENT** | 30 min |
| **M-m prefix chaos** | 🟡 MED | MED | ⚡ **HIGH** | 1 hour |
| **Sort-tab conflicts** | 🟡 MED | LOW | ⚡ **HIGH** | 20 min |
| **Which-key** | 🟢 LOW | MED | 🌟 **NICE** | 1 hour |
| **Search consolidation** | 🟢 LOW | HIGH | 💡 **LATER** | 2 hours |

---

## 💡 Quick Wins

These can be fixed in **< 1 hour** with high impact:

1. ✅ **LSP bindings to standard** (30 min)
   - High impact on usability
   - Aligns with LSP common module

2. ✅ **Move magit to C-x g** (15 min)
   - Standard binding
   - Clean up M-m

3. ✅ **Fix sort-tab** (20 min)
   - Resolve M-s conflicts
   - Better organization

**Total**: ~65 minutes for major improvements!

---

## 🎓 Best Practices Recommendations

### 1. **Follow Emacs Conventions**
- `C-c [letter]` - User bindings (safe)
- `M-.` - Standard for "go to definition"
- `C-h` - Help prefix
- `C-x` - Extended commands

### 2. **Use Semantic Prefixes**
```elisp
C-c g *  - Git
C-c p *  - Project
C-c t *  - Tabs/Windows
C-c n *  - Notes
```

### 3. **Add Discoverability**
- Use which-key
- Add transient/hydra menus
- Document bindings

### 4. **Avoid Conflicts**
- Don't rebind M-g (goto)
- Don't rebind M-s (search)
- Don't rebind C-h (help)

---

## 🎯 Summary

**Current State**: 
- ✅ Good lazy loading
- ✅ Logical grouping
- ⚠️ M-g conflicts (critical)
- ⚠️ M-m overused
- ⚠️ M-s conflicts

**After Fixes**:
- ✅ Standard LSP bindings
- ✅ Clean prefix organization
- ✅ No conflicts
- ✅ Better discoverability

**Effort**: ~2-3 hours for complete refactor  
**Impact**: 🚀 **HUGE** - Much better UX

---

**Ready to implement these fixes?** 🎯
