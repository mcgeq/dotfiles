# neotest - 测试框架集成

## 📖 简介

`neotest` 是一个通用测试框架集成插件，支持在 Neovim 中运行、调试和查看测试结果。支持多种测试框架（Jest、pytest、Go test 等）。

## 🎯 核心功能

- ✅ 运行单个测试、文件测试、整个测试套件
- ✅ 实时显示测试状态（通过/失败）
- ✅ 查看详细的测试输出和错误信息
- ✅ 集成调试器（nvim-dap）
- ✅ 自动发现测试文件

## ⌨️ 快捷键

### 运行测试

| 快捷键 | 功能 |
|--------|------|
| `<leader>tr` | 运行最近的测试 |
| `<leader>tf` | 运行当前文件所有测试 |
| `<leader>ts` | 运行整个测试套件 |
| `<leader>tl` | 运行上次测试 |
| `<leader>to` | 显示测试输出 |
| `<leader>tp` | 切换摘要面板 |
| `<leader>td` | 调试最近的测试 |

### 导航

| 快捷键 | 功能 |
|--------|------|
| `]t` | 跳转到下一个测试 |
| `[t` | 跳转到上一个测试 |

## 🔥 使用场景

### JavaScript/TypeScript

```typescript
describe('UserService', () => {
  it('should get user', async () => {
    // 光标在这里，按 <leader>tr 运行此测试
    const user = await userService.getById(1);
    expect(user.name).toBe('John');
  });
});
```

### Python

```python
def test_add():
    # 光标在这里，按 <leader>tr 运行此测试
    assert add(2, 3) == 5
```

### Go

```go
func TestAdd(t *testing.T) {
    // 光标在这里，按 <leader>tr 运行此测试
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("Expected 5, got %d", result)
    }
}
```

## 💡 实用技巧

1. **TDD 工作流**：`<leader>tr` 运行测试 -> 修改代码 -> 再次运行
2. **调试失败测试**：`<leader>td` 启动调试器
3. **查看输出**：`<leader>to` 查看详细错误信息
4. **快速导航**：使用 `]t` 和 `[t` 在测试间跳转

## 🔗 相关资源

- [GitHub - neotest](https://github.com/nvim-neotest/neotest)
- [AstroCommunity 插件页](https://github.com/AstroNvim/astrocommunity/tree/main/lua/astrocommunity/test/neotest)
