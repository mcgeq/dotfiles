# 提交规范化

Commitizen + commitlint + husky，用于在项目中实现标准化的 Git 提交信息。

## 文件

- `package.json` — 依赖（commitizen、cz-customizable、commitlint）
- `.cz-config.cjs` — Commitizen 适配器配置（提交类型、范围）
- `commitlint.config.js` — Commitlint 规则
- `.lintstagedrc.cjs` — Lint-staged 配置
- `.husky/` — Git 钩子（commit-msg、pre-commit）
- `.editorconfig` — 编辑器格式化规则
- `eslint.config.js` / `.eslintrc.cjs` — ESLint 配置（支持 ESLint 8 & 9）
- `.prettierrc` / `.prettierignore` — Prettier 配置
- `tsconfig.json` — TypeScript 配置
- `vite.config.ts` — Vite 配置

## 分支

- `main` — ESLint 8
- `feature/eslint9` — ESLint 9

## 使用

将所需文件复制到项目中即可启用标准化提交：

```
npx cz                    # Commitizen 交互式提交
npx commitlint            # 验证提交信息
npx lint-staged           # 对暂存文件运行 linter
```
