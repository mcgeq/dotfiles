# C++ / clangd

This config enables `clangd` through [lua/user/lsp/clangd.lua](/d:/config/dotfiles/nvim/lua/user/lsp/clangd.lua) with a C++-first setup:

- prefer a system `clangd` / LLVM toolchain when it is on `PATH`
- fall back to Mason's bundled `clangd` if no system binary is found
- ask `clangd` to query local drivers such as `clang++`, `clang-cl`, `clang`, or `cl`
- use `-std=c++23` as the fallback parse mode for C++ buffers when no compilation database exists
- enable `clangd_extensions.nvim` and `cmake-tools.nvim` through [lua/user/plugins/cpp.lua](/d:/config/dotfiles/nvim/lua/user/plugins/cpp.lua)
- install `codelldb` by default for future C++ debugging workflows

## What This Fixes

This is enough for the common "open a `.cpp` file and expect C++20/23 syntax, standard headers, completion, and diagnostics to work" case.

In particular, `--query-driver=...` helps `clangd` discover system headers and the C++ standard library from the real compiler toolchain rather than guessing.

The extra plugin layer adds a few workflow upgrades inspired by AstroCommunity's C++ pack:

- `:ClangdSwitchSourceHeader`
- `:ClangdAST`
- `:ClangdSymbolInfo`
- `:ClangdTypeHierarchy`
- `:ClangdMemoryUsage`
- `cmake-tools.nvim` helpers for generating, building, and running CMake targets

## Localleader Maps

For `c`, `cpp`, `objc`, `objcpp`, and `cuda` buffers:

- `<localleader>ch`: switch source / header
- `<localleader>ca`: show AST
- `<localleader>ci`: show symbol info
- `<localleader>ct`: show type hierarchy
- `<localleader>cm`: show memory usage

For `cmake` buffers:

- `<localleader>cg`: generate project
- `<localleader>cb`: build target
- `<localleader>cr`: run target
- `<localleader>ct`: run tests
- `<localleader>cs`: select build target
- `<localleader>cl`: select launch target
- `<localleader>cp`: select configure preset
- `<localleader>cv`: select build type

## What It Does Not Magically Fix

C++20 modules and header units still depend on project build information.

Examples:

- `#include <iostream>` usually works once `clangd` can discover the toolchain and standard library.
- `import my_module;` needs compile commands that describe how the BMI / PCM is built and where it lives.
- `import <iostream>;` is a header-unit workflow, not a plain include. It requires precompiled module artifacts such as `iostream.pcm` and matching compile flags.

So the editor config can make `clangd` much more reliable, but modules still need project-level build setup.

## Minimal Project `.clangd`

For a small project without `compile_commands.json`, a checked-in `.clangd` file is the fastest way to force a modern C++ mode:

```yaml
If:
  PathMatch: .*\\.(cc|cp|cpp|cxx|h|hh|hpp|hxx|ixx|cppm|mpp)$
CompileFlags:
  Add: [-std=c++23, -xc++]
```

This helps single-file and lightweight projects.

## Better: `compile_commands.json`

For anything serious, especially modules, use a compilation database.

If you use `cmake-tools.nvim`, this config enables `-DCMAKE_EXPORT_COMPILE_COMMANDS=1` during generate and copies the resulting `compile_commands.json` back into the project root for Windows-friendly clangd discovery.

With CMake, the usual baseline is:

```cmake
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
```

For module builds, the compile commands must also include whatever your build uses for module artifacts, for example flags such as:

- `-fprebuilt-module-path=...`
- `-fmodule-file=...`

Without those flags, `clangd` cannot infer where your named modules or header units come from.

## Practical Rule

Use this rule of thumb:

- regular C++ code and standard headers: editor config plus `query-driver`
- modern language mode in small projects: add a project `.clangd`
- modules / header units: provide real compile commands from the build system
