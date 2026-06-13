# Commit Standardization

Commitizen + commitlint + husky for standardized Git commit messages across projects.

## Files

- `package.json` — Dependencies (commitizen, cz-customizable, commitlint)
- `.cz-config.cjs` — Commitizen adapter config (commit types, scopes)
- `commitlint.config.js` — Commitlint rules
- `.lintstagedrc.cjs` — Lint-staged config
- `.husky/` — Git hooks (commit-msg, pre-commit)
- `.editorconfig` — Editor formatting rules
- `eslint.config.js` / `.eslintrc.cjs` — ESLint config (supports ESLint 8 & 9)
- `.prettierrc` / `.prettierignore` — Prettier config
- `tsconfig.json` — TypeScript config
- `vite.config.ts` — Vite config

## Branches

- `main` — ESLint 8
- `feature/eslint9` — ESLint 9

## Usage

Copy these files to any project to enable standardized commits:

```
npx cz                    # Commitizen interactive commit
npx commitlint            # Validate commit message
npx lint-staged           # Run linters on staged files
```
