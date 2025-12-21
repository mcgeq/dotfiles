# ty Configuration Guide

## Overview

`ty` is a fast Python type checker developed by Astral (the same team behind ruff). This guide explains how to configure ty in your project.

## pyproject.toml Configuration

### Basic Configuration

```toml
[tool.ty]
# Python version to target
pythonVersion = "3.13"

# Directories to include in type checking
include = ["src", "tests"]

# Directories to exclude from type checking
exclude = [
    "**/node_modules",
    "**/__pycache__",
    ".venv",
    "build",
    "dist",
]

# Virtual environment configuration
venvPath = "."
venv = ".venv"

# Additional paths to search for imports
extraPaths = ["./src"]
```

### Type Checking Modes

ty supports different levels of type checking strictness:

```toml
[tool.ty]
# Type checking mode (optional)
# Options: "off", "basic", "standard", "strict", "all"
typeCheckingMode = "standard"  # Default
```

**Mode Descriptions:**
- `off`: Disable type checking
- `basic`: Basic type checking (minimal)
- `standard`: Standard type checking (recommended)
- `strict`: Strict type checking
- `all`: Maximum strictness

### Report Settings

Control what ty reports:

```toml
[tool.ty]
# Import-related reports
reportMissingImports = true
reportMissingTypeStubs = false

# Unused code reports
reportUnusedImport = true
reportUnusedClass = true
reportUnusedFunction = true
reportUnusedVariable = true

# Other reports
reportDuplicateImport = true
reportIncompatibleMethodOverride = true
reportIncompatibleVariableOverride = true
```

### Advanced Configuration

```toml
[tool.ty]
# Stub path for type stubs
stubPath = "typings"

# Enable/disable specific features
useLibraryCodeForTypes = true
strictListInference = false
strictDictionaryInference = false
strictSetInference = false

# Analysis settings
analyzeUnannotatedFunctions = true
strictParameterNoneValue = true
```

## Neovim Integration

The ty LSP server is automatically configured in `nvim/lua/plugins/python-ty-ruff.lua`:

```lua
[tool.ty]
cmd = { "uv", "run", "ty", "server" }
root_markers = {
  "pyproject.toml",
  "setup.py",
  "requirements.txt",
  ".git",
}
settings = {
  ty = {
    -- Settings from pyproject.toml are automatically loaded
  },
}
```

## Command Line Usage

### Check a file
```bash
uv run ty check main.py
```

### Check entire project
```bash
uv run ty check .
```

### Start LSP server
```bash
uv run ty server
```

### Show version
```bash
uv run ty --version
```

## Common Configuration Examples

### Strict Project

```toml
[tool.ty]
pythonVersion = "3.13"
include = ["src"]
exclude = [".venv", "tests"]

typeCheckingMode = "strict"
reportMissingImports = true
reportUnusedImport = true
reportUnusedVariable = true
strictParameterNoneValue = true
```

### Lenient Project

```toml
[tool.ty]
pythonVersion = "3.13"
include = ["src"]
exclude = [".venv"]

typeCheckingMode = "basic"
reportMissingImports = false
reportUnusedImport = false
```

### Library Development

```toml
[tool.ty]
pythonVersion = "3.13"
include = ["src", "tests"]
exclude = [".venv", "build", "dist"]

typeCheckingMode = "strict"
stubPath = "typings"
useLibraryCodeForTypes = true
reportMissingTypeStubs = true
reportIncompatibleMethodOverride = true
```

## Comparison with basedpyright

ty is designed to be compatible with pyright/basedpyright configuration:

| Feature | ty | basedpyright |
|---------|-----|--------------|
| Speed | ⚡ Faster (Rust) | Slower (TypeScript) |
| Configuration | Compatible | Standard |
| LSP Support | ✅ Yes | ✅ Yes |
| Type Checking | ✅ Full | ✅ Full |

You can keep both configurations in `pyproject.toml` for compatibility:

```toml
[tool.ty]
pythonVersion = "3.13"
include = ["src"]

[tool.basedpyright]
pythonVersion = "3.13"
include = ["src"]
```

## Troubleshooting

### ty not finding imports

Add the directory to `extraPaths`:

```toml
[tool.ty]
extraPaths = ["./src", "./lib"]
```

### Virtual environment not detected

Ensure `venvPath` and `venv` are set correctly:

```toml
[tool.ty]
venvPath = "."
venv = ".venv"
```

### Too many false positives

Reduce strictness:

```toml
[tool.ty]
typeCheckingMode = "basic"
reportMissingImports = false
```

## Resources

- [ty Documentation](https://docs.astral.sh/ty/)
- [ty Configuration Reference](https://docs.astral.sh/ty/configuration/)
- [ty GitHub Repository](https://github.com/astral-sh/ty)

## See Also

- [ruff Configuration](https://docs.astral.sh/ruff/configuration/)
- [Python Type Hints](https://docs.python.org/3/library/typing.html)
