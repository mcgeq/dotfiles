# Which-key vs Transient - Complete Comparison

## 🎯 Quick Answer

**Use BOTH!** They serve different but complementary purposes.

---

## 📊 Core Differences

| Aspect | Which-key | Transient |
|--------|-----------|-----------|
| **Purpose** | Display available keys | Create interactive menus |
| **Type** | Passive (shows info) | Active (interaction) |
| **When** | After pressing prefix | When calling menu |
| **Complexity** | Simple | Complex but powerful |
| **Best for** | Discovery | Workflows |

---

## 🔍 Which-key - The Discovery Tool

### What It Does

**Shows you what keys are available** after you press a prefix.

```
You press: C-c g
Which-key shows:
┌─────────────────────────────────┐
│ C-c g                            │
├─────────────────────────────────┤
│ s → git status                   │
│ l → git log                      │
│ p → git push                     │
│ f → git pull                     │
│ b → branch...                    │
│ ? → git menu                     │
└─────────────────────────────────┘
```

### Characteristics

✅ **Automatic** - No explicit configuration needed  
✅ **Passive** - Just shows information  
✅ **Always on** - Works for ALL prefixes  
✅ **Low overhead** - Minimal performance impact  
✅ **Easy to learn** - Zero learning curve  

### Configuration

```elisp
;; Super simple
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))  ; Show after 0.5s

;; Add descriptions
(which-key-add-key-based-replacements
  "C-c g" "git"
  "C-c s" "search")
```

### When to Use

- ✅ Learning new keybindings
- ✅ Discovering what's available under a prefix
- ✅ Reminding yourself of rarely-used keys
- ✅ Teaching others your config

### Limitations

❌ Can't execute complex workflows  
❌ Can't show dynamic content  
❌ Can't take arguments  
❌ Just displays info, doesn't help with multi-step operations  

---

## 🎨 Transient - The Menu System

### What It Does

**Creates interactive, multi-level menus** for complex commands.

```elisp
;; Define a transient menu
(transient-define-prefix my-git-menu ()
  "Git operations"
  [["Commit"
    ("c" "commit" magit-commit)
    ("a" "amend" magit-commit-amend)]
   ["Remote"
    ("p" "push" magit-push)
    ("f" "pull" magit-pull)]])

;; When you call it:
┌─────────────────────────────────┐
│ Git operations                   │
├─────────────────────────────────┤
│ Commit      Remote              │
│ c commit    p push              │
│ a amend     f pull              │
└─────────────────────────────────┘
```

### Characteristics

✅ **Interactive** - Can take arguments  
✅ **Multi-level** - Nested menus  
✅ **State management** - Remember choices  
✅ **Visual** - Beautiful, organized display  
✅ **Powerful** - Can do complex workflows  

### Configuration

```elisp
;; More complex but powerful
(transient-define-prefix my-search-menu ()
  "Search commands"
  [:description "Search in..."
   ["Buffer"
    ("l" "Line" consult-line)
    ("i" "Imenu" consult-imenu)]
   ["Project"
    ("g" "Grep" consult-ripgrep)
    ("f" "Find" consult-find)]])

;; Can have arguments/switches
(transient-define-prefix my-format-menu ()
  "Format code"
  ["Options"
   ("-s" "Save" "--save-buffer")
   ("-a" "All" "--all-files")]
  ["Actions"
   ("f" "Format" my-format-function)])
```

### When to Use

- ✅ Complex workflows with multiple options
- ✅ Commands that need arguments
- ✅ Creating a "control panel" for a feature
- ✅ Replacing multiple related keybindings with one menu
- ✅ When you want a more "modern UI" feel

### Limitations

❌ Requires explicit menu definition  
❌ More complex to set up  
❌ Higher learning curve  
❌ Need to maintain menu definitions  

---

## 🎯 Real-World Comparison

### Example: Git Operations

#### With Which-key Only
```elisp
;; You define keybindings
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)
(global-set-key (kbd "C-c g p") 'magit-push)

;; Add descriptions
(which-key-add-key-based-replacements
  "C-c g" "git"
  "C-c g s" "status"
  "C-c g l" "log"
  "C-c g p" "push")

;; User experience:
;; 1. Press C-c g
;; 2. See all options
;; 3. Press s/l/p
;; 4. Command executes
```

**Pros**: Simple, automatic, works everywhere  
**Cons**: Just shows keys, can't do complex things  

#### With Transient
```elisp
;; Define menu
(transient-define-prefix my-git-menu ()
  "Git operations"
  [["Main"
    ("s" "Status" magit-status)
    ("l" "Log" magit-log)]
   ["Remote"
    ("p" "Push" magit-push)
    ("f" "Pull" magit-pull)]
   ["Branch"
    ("b" "Create" magit-branch-create)
    ("r" "Rename" magit-branch-rename)]])

(global-set-key (kbd "C-c g ?") 'my-git-menu)

;; User experience:
;; 1. Press C-c g ?
;; 2. See beautiful menu
;; 3. Press letter for action
;; 4. Can navigate between sections
```

**Pros**: Beautiful, organized, powerful  
**Cons**: Need to define menu, one more key press  

#### Best of Both Worlds
```elisp
;; Direct keybindings + descriptions (which-key)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)
(which-key-add-key-based-replacements "C-c g" "git")

;; Plus menu for discovery/complex ops (transient)
(global-set-key (kbd "C-c g ?") 'my-git-menu)
```

**Result**:
- Fast access for common commands: `C-c g s`
- Menu for exploration: `C-c g ?`
- Which-key shows both options!

---

## 🎨 Detailed Feature Comparison

### Discovery & Learning

**Which-key**: ⭐⭐⭐⭐⭐
- Shows ALL available keys
- Works automatically
- No setup needed

**Transient**: ⭐⭐⭐
- Only shows what you explicitly define
- Need to create menus
- But more organized when shown

### Visual Appeal

**Which-key**: ⭐⭐⭐
- Simple list
- Functional but plain
- Good enough

**Transient**: ⭐⭐⭐⭐⭐
- Beautiful layouts
- Organized columns
- Grouped by function
- Professional look

### Complexity Handling

**Which-key**: ⭐⭐
- Just shows keys
- Can't handle arguments
- Can't do workflows

**Transient**: ⭐⭐⭐⭐⭐
- Multi-step workflows
- Arguments and switches
- State management
- Very powerful

### Performance

**Which-key**: ⭐⭐⭐⭐⭐
- Lightweight
- Minimal overhead
- Always fast

**Transient**: ⭐⭐⭐⭐
- Slightly heavier
- Still fast
- Negligible for menus

### Learning Curve

**Which-key**: ⭐⭐⭐⭐⭐
- Zero learning curve
- Install and go
- Automatic

**Transient**: ⭐⭐⭐
- Need to learn API
- More complex setup
- Worth it for power users

### Maintenance

**Which-key**: ⭐⭐⭐⭐⭐
- Minimal maintenance
- Just add descriptions
- Works with any keys

**Transient**: ⭐⭐⭐
- Need to maintain menus
- Update when commands change
- More work

---

## 🏆 Recommendation

### Use Which-key When:
- ✅ You want automatic discovery
- ✅ You have simple keybinding schemes
- ✅ You want zero configuration
- ✅ You want it to "just work"

### Use Transient When:
- ✅ You have complex workflows
- ✅ You want beautiful menus
- ✅ You need argument passing
- ✅ You want to create "apps" within Emacs

### Use BOTH When:
- ✅ You want the best of both worlds (RECOMMENDED!)
- ✅ Fast access + discovery
- ✅ Simple commands + complex workflows

---

## 💡 Recommended Setup

```elisp
;;; Perfect combination

;; 1. Which-key for automatic discovery
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  
  ;; Add descriptions for common prefixes
  (which-key-add-key-based-replacements
    "C-c g" "git"
    "C-c s" "search"
    "C-c n" "notes"
    "C-c t" "tabs"
    "C-c m" "markmacro"
    "C-c r" "refactor"))

;; 2. Direct keybindings for common commands
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)
;; ... etc

;; 3. Transient menus for complex/discovery
(transient-define-prefix my-git-menu ()
  "Git menu"
  [["Main"
    ("s" "Status" magit-status)
    ("l" "Log" magit-log)
    ("d" "Diff" magit-diff)]
   ["Commit"
    ("c" "Commit" magit-commit)
    ("a" "Amend" magit-commit-amend)]
   ["Remote"
    ("p" "Push" magit-push)
    ("f" "Pull" magit-pull)]])

(global-set-key (kbd "C-c g ?") 'my-git-menu)

;; Now users can:
;; - C-c g s    → Fast access (which-key shows options)
;; - C-c g ?    → Full menu (transient)
;; - C-c g      → Shows all options (which-key)
```

---

## 📊 Usage Patterns

### Pattern 1: Fast Power User
```
Common commands: Direct keys (C-c g s)
Rare commands:   Via which-key discovery (C-c g → see options)
Learning:        Via which-key
```

### Pattern 2: Menu Lover
```
Everything:      Via transient menus (C-c g ?)
Learning:        Via menu exploration
Fast access:     Memorize menu letters
```

### Pattern 3: Hybrid (BEST!)
```
Daily commands:  Direct keys (C-c g s)
Exploration:     Which-key (press C-c g, wait)
Complex ops:     Transient menus (C-c g ?)
Learning:        Both!
```

---

## 🎯 Specific Recommendations

### For Your Emacs Config

**Essential (Install now)**:
```elisp
✅ Which-key - Automatic discovery, minimal setup
✅ Add prefix descriptions for your new keybindings
```

**Nice to Have (Add later)**:
```elisp
⭐ Transient menu for Git (C-c g ?)
⭐ Transient menu for Search (C-c s ?)
⭐ Transient menu for LSP (C-c r ?)
```

### Priority

1. **High**: Install which-key (30 min)
   - Immediate benefit
   - Zero learning curve
   - Works with existing keys

2. **Medium**: Add prefix descriptions (15 min)
   - Makes which-key more useful
   - Simple configuration

3. **Low**: Create transient menus (2-4 hours)
   - Nice to have
   - Takes time to design
   - Can do gradually

---

## 🚀 Quick Start

### Step 1: Which-key (Do This Now)
```elisp
(use-package which-key
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "C-c g" "git"
    "C-c s" "search"
    "C-c n" "notes"
    "C-c t" "tabs"
    "C-c m" "markmacro"
    "C-c r" "refactor/lsp"
    "C-c !" "diagnostics"))
```

### Step 2: Test It
```
Press: C-c g
Wait: 0.5 seconds
See: Beautiful list of all C-c g commands!
```

### Step 3: Create One Menu (Optional)
```elisp
(transient-define-prefix my-help-menu ()
  "Help commands"
  [["Describe"
    ("f" "Function" describe-function)
    ("v" "Variable" describe-variable)
    ("k" "Key" describe-key)]
   ["Info"
    ("i" "Info" info)
    ("m" "Mode" describe-mode)]])

(global-set-key (kbd "C-h ?") 'my-help-menu)
```

---

## 🎓 Conclusion

### The Answer

**Use BOTH!**

**Which-key**: Foundation - automatic discovery  
**Transient**: Enhancement - beautiful menus for complex tasks  

### Why Both?

1. **Which-key** gives you automatic discovery everywhere
2. **Transient** gives you polished menus where you need them
3. They don't conflict, they complement!
4. Start with which-key, add transient gradually

### The Perfect Setup

```
Daily workflow:
├─ Common commands → Direct keys (fast!)
├─ Exploration → Which-key (automatic!)
└─ Complex operations → Transient menus (beautiful!)
```

---

## 📈 Adoption Curve

**Week 1**: Install which-key  
- Immediate benefit
- Start discovering keys you didn't know

**Week 2-3**: Add descriptions  
- Make which-key more useful
- Document your config

**Month 2+**: Add transient menus  
- For complex workflows
- When you find patterns
- Gradually, not all at once

---

**TL;DR**: Install which-key NOW (takes 5 min), add transient menus LATER (when you need them).

**Both together = Perfect Emacs UX!** 🎉
