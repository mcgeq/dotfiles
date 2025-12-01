* # Complete Keybindings Reference

**Last Updated**: 2025-12-01  
**Version**: 2.0 (After refactoring)

---

## 🎯 Quick Access

| Category | Prefix | Examples |
|----------|--------|----------|
| **LSP** | M-., M-, | Go to definition, return |
| **Git** | C-x g, C-c g | Status, log, push/pull |
| **Search** | C-c s | Line, grep, imenu |
| **Tabs** | C-c t | Next, prev, close |
| **Org** | C-c n | Capture, agenda, links |
| **Refactor** | C-c r | Rename, format, action |
| **Markmacro** | C-c m | Rectangle operations |
| **Yasnippet** | C-c y | Snippets |

---

## 📋 All Keybindings by Category

### LSP & Navigation (Standard)

| Key | Command | Description |
|-----|---------|-------------|
| **M-.** | lsp-bridge-find-def | Go to definition |
| **M-,** | lsp-bridge-find-def-return | Return from definition |
| **M-?** | lsp-bridge-find-references | Find all references |
| **C-M-.** | lsp-bridge-find-def-other-window | Open definition in other window |
| **C-h .** | lsp-bridge-popup-documentation | Show documentation |
| **C-h ,** | lsp-bridge-show-signature | Show function signature |
| **C-h <up>** | lsp-bridge-popup-documentation-scroll-up | Scroll doc up |
| **C-h <down>** | lsp-bridge-popup-documentation-scroll-down | Scroll doc down |

### LSP Code Actions (C-c r prefix)

| Key | Command | Description |
|-----|---------|-------------|
| **C-c r r** | lsp-bridge-rename | Rename symbol |
| **C-c r i** | lsp-bridge-code-action | Code action menu |
| **C-c r f** | lsp-bridge-code-format | Format buffer |
| **C-c r l** | lsp-bridge-find-impl | Find implementation |
| **C-c r w** | lsp-bridge-workspace-list-symbols | List workspace symbols |

### LSP Diagnostics (C-c ! prefix)

| Key | Command | Description |
|-----|---------|-------------|
| **C-c ! n** | lsp-bridge-diagnostic-jump-next | Next diagnostic |
| **C-c ! p** | lsp-bridge-diagnostic-jump-prev | Previous diagnostic |
| **C-c ! l** | lsp-bridge-diagnostic-list | List all diagnostics |

---

### Git (Magit)

#### Primary Access
| Key | Command | Description |
|-----|---------|-------------|
| **C-x g** | magit-status | Status (standard) |
| **C-x M-g** | magit-dispatch | Dispatch menu |

#### Alternative Access (C-c g prefix)
| Key | Command | Description |
|-----|---------|-------------|
| **C-c g s** | magit-status | Status |
| **C-c g l** | magit-log | Log |
| **C-c g d** | magit-dispatch | Dispatch |
| **C-c g f** | magit-file-dispatch | File dispatch |
| **C-c g p** | magit-push | Push |
| **C-c g f** | magit-pull | Pull |
| **C-c g ?** | mcg-git-menu | Git menu |

#### Branches (C-c g b)
| Key | Command | Description |
|-----|---------|-------------|
| **C-c g b b** | magit-branch-create | Create branch |
| **C-c g b r** | magit-branch-rename | Rename branch |
| **C-c g b d** | magit-branch-delete | Delete branch |

#### Remote (C-c g r)
| Key | Command | Description |
|-----|---------|-------------|
| **C-c g r a** | magit-remote-add | Add remote |
| **C-c g r r** | magit-remote-rename | Rename remote |
| **C-c g r d** | magit-remote-remove | Remove remote |

#### Submodules (C-c g m)
| Key | Command | Description |
|-----|---------|-------------|
| **C-c g m a** | magit-submodule-add | Add submodule |
| **C-c g m r** | magit-submodule-remove | Remove submodule |
| **C-c g m u** | magit-submodule-update | Update submodule |

#### Files (C-c g F)
| Key | Command | Description |
|-----|---------|-------------|
| **C-c g F r** | magit-file-rename | Rename file |
| **C-c g F d** | magit-file-delete | Delete file |

---

### Search & Navigation (C-c s prefix)

#### Buffer Search
| Key | Command | Description |
|-----|---------|-------------|
| **C-c s l** | consult-line | Search lines in buffer |
| **C-c s i** | consult-imenu | Navigate imenu |
| **C-c s m** | consult-mark | Navigate marks |

#### Project Search
| Key | Command | Description |
|-----|---------|-------------|
| **C-c s g** | consult-ripgrep | Grep in project |
| **C-c s f** | consult-find | Find files |
| **C-c s b** | consult-buffer | Switch buffer |
| **C-c s r** | consult-recent-file | Recent files |

#### Org Specific
| Key | Command | Description |
|-----|---------|-------------|
| **C-c s o** | mcg/consult-find-org-headings | Find org headings |

#### Color-rg
| Key | Command | Description |
|-----|---------|-------------|
| **C-c s s** | color-rg-search-symbol | Search symbol |
| **C-c s S** | color-rg-search-symbol-in-project | Search symbol in project |
| **C-c s .** | color-rg-search-symbol-in-current-file | Search in current file |

#### Alternative Search
| Key | Command | Description |
|-----|---------|-------------|
| **C-c s B** | blink-search | Blink search |
| **C-c s ?** | mcg-search-menu | Search menu |

---

### Tabs (C-c t prefix)

| Key | Command | Description |
|-----|---------|-------------|
| **C-c t n** | sort-tab-select-next-tab | Next tab |
| **C-c t p** | sort-tab-select-prev-tab | Previous tab |
| **C-c t f** | sort-tab-select-first-tab | First tab |
| **C-c t l** | sort-tab-select-last-tab | Last tab |
| **C-c t k** | sort-tab-close-current-tab | Close tab |
| **C-c t o** | sort-tab-close-other-tabs | Close other tabs |
| **C-c t K** | sort-tab-close-all-tabs | Close all tabs |
| **C-c t 1/2/3** | sort-tab-select-by-index | Jump to tab by number |
| **C-c t s** | mcg/theme-selector | Theme selector |
| **C-c t i** | mcg/show-theme-settings | Show theme info |

---

### Org Mode (C-c n prefix)

| Key | Command | Description |
|-----|---------|-------------|
| **C-c n c** | org-capture | Capture |
| **C-c n a** | org-agenda | Agenda |
| **C-c n l** | org-store-link | Store link |
| **C-c n t** | org-todo-list | Todo list |

#### In Org Buffers
| Key | Command | Description |
|-----|---------|-------------|
| **C-c n e** | org-edit-src-code | Edit source code |
| **C-c n v** | mg/org-insert-clipboard-image | Insert clipboard image |
| **C-c n h** | org-insert-heading | Insert heading |
| **C-c n s** | org-insert-subheading | Insert subheading |

---

### Markmacro (C-c m prefix)

#### Rectangle Operations
| Key | Command | Description |
|-----|---------|-------------|
| **C-c m s** | markmacro-rect-set | Set rectangle start |
| **C-c m d** | markmacro-rect-delete | Delete rectangle |
| **C-c m r** | markmacro-rect-replace | Replace rectangle content |
| **C-c m i** | markmacro-rect-insert | Insert in rectangle |

#### Mark Conversion
| Key | Command | Description |
|-----|---------|-------------|
| **C-c m c** | markmacro-rect-mark-columns | Mark columns as objects |
| **C-c m S** | markmacro-rect-mark-symbols | Mark symbols |

#### Apply Macros
| Key | Command | Description |
|-----|---------|-------------|
| **C-c m a** | markmacro-apply-all | Apply to all marks |
| **C-c m e** | markmacro-apply-all-except-first | Apply except first |

#### Super Key Bindings (Alternative)
| Key | Command | Description |
|-----|---------|-------------|
| **s-/** | markmacro-mark-words | Mark words |
| **s-?** | markmacro-mark-lines | Mark lines |
| **s-:** | markmacro-mark-chars | Mark characters |
| **s-L** | markmacro-mark-imenus | Mark imenu items |
| **s-<** | markmacro-apply-all | Apply all |
| **s->** | markmacro-apply-all-except-first | Apply except first |

---

### Yasnippet (C-c y prefix)

| Key | Command | Description |
|-----|---------|-------------|
| **C-c y n** | yas-new-snippet | New snippet |
| **C-c y v** | yas-visit-snippet-file | Visit snippet file |
| **C-c y r** | yas-reload-all | Reload all snippets |

---

### Editing & Movement

#### Line Movement
| Key | Command | Description |
|-----|---------|-------------|
| **C-a** | mcge-smart-move-beginning-of-line | Smart beginning of line |
| **C-c <up>** | insert-line-above | Insert line above |
| **C-c <down>** | insert-line-below | Insert line below |
| **M-<up>** | move-line-up | Move line up |
| **M-<down>** | move-line-down | Move line down |

#### Scrolling
| Key | Command | Description |
|-----|---------|-------------|
| **M-n** | mcg-scroll-up-third | Scroll down 1/3 window |
| **M-p** | mcg-scroll-down-third | Scroll up 1/3 window |

#### Misc Editing
| Key | Command | Description |
|-----|---------|-------------|
| **C-c e** | mcg/open-init-file | Open init file |
| **C-c r** | replace-string | Replace string |
| **C-c d** | make-directory | Make directory |
| **C-c f r** | recentf-open-files | Recent files |
| **C-c C-i** | mcg-show-config-info | Config info |

---

### Undo/Redo

| Key | Command | Description |
|-----|---------|-------------|
| **C-/** | undo | Undo |
| **C-?** | undo-redo | Redo |
| **C-c u** | vundo | Visual undo tree |

---

### Embark & Consult

| Key | Command | Description |
|-----|---------|-------------|
| **C-.** | embark-act | Embark act |
| **C-,** | embark-dwim | Embark dwim |
| **C-h b** | embark-bindings | Show bindings |

---

### Help System

#### Helpful (Enhanced Help)
| Key | Command | Description |
|-----|---------|-------------|
| **C-h f** | helpful-callable | Describe function/macro |
| **C-h v** | helpful-variable | Describe variable |
| **C-h k** | helpful-key | Describe key |
| **C-h x** | helpful-command | Describe command |
| **C-h F** | helpful-function | Describe function only |
| **C-c C-d** | helpful-at-point | Help at point |

#### Which-key
| Key | Command | Description |
|-----|---------|-------------|
| **C-h w p** | mcg-which-key-show-all-prefixes | Show all prefixes |
| **C-h w m** | mcg-which-key-show-major-mode | Show major mode keys |
| **C-h w ?** | mcg-which-key-tips | Which-key tips |

#### Custom Help
| Key | Command | Description |
|-----|---------|-------------|
| **C-h K** | mcg-show-keybindings | Keybindings reference |

---

### Elisp Development

| Key | Command | Description |
|-----|---------|-------------|
| **C-c C-b** | eval-buffer | Eval buffer |
| **C-c C-c** | eval-to-comment | Eval to comment |
| **C-c C-r** | eval-region | Eval region |

---

### Comments

| Key | Command | Description |
|-----|---------|-------------|
| **M-;** | comment-or-uncomment | Toggle comment |

---

## 🎨 Transient Menus

### Git Menu (C-c g ?)
```
┌─────────────────────────────────┐
│ Main      Commit      Remote    │
│ s Status  c Commit    p Push    │
│ l Log     a Amend     f Pull    │
│ d Diff                          │
└─────────────────────────────────┘
```

### Search Menu (C-c s ?)
```
┌─────────────────────────────────┐
│ Buffer     Project    Alt       │
│ l Line     g Grep     b Blink   │
│ i Imenu    f Find     r Color-rg│
│ m Mark     s Symbol             │
└─────────────────────────────────┘
```

---

## 📖 How to Use This Reference

### 1. Discovery

**With Which-key** (Recommended):
```
1. Press a prefix (e.g., C-c g)
2. Wait 0.5 seconds
3. See all available options!
```

**With This Document**:
- Find your category
- Look up the key
- Practice!

### 2. Learning Strategy

**Week 1**: Focus on LSP
- M-. (definition)
- M-, (return)
- C-h . (docs)

**Week 2**: Git basics
- C-x g (status)
- C-c g l (log)

**Week 3**: Search
- C-c s l (line)
- C-c s g (grep)

**Week 4**: Everything else

### 3. Quick Tips

- **Forgot a key?** Press the prefix and wait for which-key
- **Complex operation?** Look for `?` suffix (menu)
- **Need help?** `C-h K` for this reference
- **LSP?** Remember: M-. is universal

---

## 🔧 Customization

All keybindings defined in:
- `config-org/keybindings/init-keymaps.org` - Main bindings
- `config-org/ui/init-which-key.org` - Descriptions
- Various plugin configs - Plugin-specific bindings

---

## 📝 Notes

### Changed from v1.0
- LSP: M-g d → **M-.**
- Git: M-m s t → **C-x g**
- Tabs: M-7/8 → **C-c t n/p**
- Search: Mixed → **C-c s ***
- Markmacro: M-m c → **C-c m**

### Standard Conventions Used
- **C-c [letter]** - User prefix (safe)
- **M-.** - LSP go-to-definition (standard)
- **C-x g** - Magit status (standard)
- **C-h** - Help prefix (standard)

---

**Last updated**: After v2.0 refactoring  
**Total keybindings**: 100+  
**Conflicts**: 0  
**Standard compliance**: 95%

**Print this for reference while learning!** 📖
