# nvim-toggler - 文本快速切换

## 📖 简介

`nvim-toggler` 可以快速在相反含义的词之间切换，如 `true/false`、`on/off`、`yes/no` 等，提高编辑效率。

## 🎯 核心功能

### 支持的切换对

| 类别 | 切换对 |
|------|--------|
| 布尔值 | `true` ↔ `false`, `True` ↔ `False`, `TRUE` ↔ `FALSE` |
| 数字 | `0` ↔ `1` |
| 开关 | `on` ↔ `off`, `yes` ↔ `no`, `enable` ↔ `disable` |
| 操作符 | `&&` ↔ `\|\|`, `==` ↔ `!=`, `>` ↔ `<`, `>=` ↔ `<=` |
| 访问修饰符 | `public` ↔ `private` ↔ `protected` |
| 方向 | `left` ↔ `right`, `top` ↔ `bottom`, `up` ↔ `down` |
| 尺寸 | `width` ↔ `height`, `min` ↔ `max` |
| 变量声明 | `let` ↔ `const`, `var` ↔ `let` |

## ⌨️ 快捷键

| 快捷键 | 功能 |
|--------|------|
| `<leader>i` | 切换光标下的词 |
| `gs` | 切换（备用快捷键）|

## 🔥 使用场景

### 1. 切换布尔值

```javascript
const isActive = true;
//               ^ 光标在这里，按 <leader>i
// 结果：const isActive = false;
```

### 2. 修改操作符

```python
if a == b:
#    ^ 按 <leader>i
# 结果：if a != b:
```

### 3. 更改访问修饰符

```java
public class User {
// ^ 按 <leader>i
// 结果：private class User {
// 再按一次：protected class User {
```

### 4. 切换方向

```css
text-align: left;
//          ^ 按 <leader>i
// 结果：text-align: right;
```

### 5. 更改变量声明

```javascript
let userName = "John";
// ^ 按 <leader>i
// 结果：const userName = "John";
```

## 💡 实用技巧

### 1. 快速调试

```javascript
const DEBUG = false;
//            ^ 按 <leader>i 开启/关闭调试
```

### 2. 切换逻辑条件

```python
if enabled and authenticated:
#          ^ 按 <leader>i 改为 or
```

### 3. 修改 CSS 属性

```css
.container {
  min-width: 100px;
  /* ^ 按 <leader>i 切换 min/max */
}
```

## 📚 注意事项

- 光标需要在目标词上
- 支持大小写保持（`True` 切换为 `False`，不是 `false`）
- 多项切换（如 public/private/protected）可连续按切换键

## 🔗 相关资源

- [GitHub - nvim-toggler](https://github.com/nguyenvukhang/nvim-toggler)
- [AstroCommunity 插件页](https://github.com/AstroNvim/astrocommunity/tree/main/lua/astrocommunity/utility/nvim-toggler)
