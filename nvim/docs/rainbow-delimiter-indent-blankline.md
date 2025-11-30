# Rainbow Delimiter + Indent Blankline - 彩虹缩进可视化

## 📖 简介

这个包整合了两个插件，提供彩虹括号和对应颜色的缩进线，让代码结构一目了然。

## 🎯 核心功能

- ✅ 彩虹括号高亮（7种颜色循环）
- ✅ 缩进线与括号颜色匹配
- ✅ 当前作用域高亮
- ✅ 快速定位括号对应关系
- ✅ 多层嵌套可视化

## 🎨 视觉效果

### 普通缩进线（单色）
```javascript
function example() {
│   if (condition) {
│   │   const data = {
│   │   │   items: [
│   │   │   │   { id: 1 }
│   │   │   ]
│   │   }
│   }
}
```

### 彩虹缩进线（多彩）
```javascript
function example() {      // 🔴 红色
│   if (condition) {      // 🟠 橙色
│   │   const data = {    // 🟡 黄色
│   │   │   items: [      // 🟢 绿色
│   │   │   │   { id: 1 } // 🔵 蓝色
│   │   │   ]             // 🟢 对应绿色
│   │   }                 // 🟡 对应黄色
│   }                     // 🟠 对应橙色
}                         // 🔴 对应红色
```

## 💡 颜色方案

默认使用 7 种颜色循环：

1. 🔴 **Red** - 第1层
2. 🟠 **Yellow** - 第2层
3. 🟡 **Green** - 第3层
4. 🟢 **Cyan** - 第4层
5. 🔵 **Blue** - 第5层
6. 🟣 **Magenta** - 第6层
7. 🟤 **Violet** - 第7层

然后循环重复。

## 🔥 实际应用

### 1. JavaScript/TypeScript 深层嵌套

```typescript
const UserProfile = () => {           // 🔴
  return (                            // 🟠
    <div className="profile">         // 🟡
      {users.map(user => (            // 🟢
        <Card key={user.id}>          // 🔵
          {user.posts.filter(p => (   // 🟣
            p.published               // 🟤
          ))}                         // 🟣
        </Card>                       // 🔵
      ))}                             // 🟢
    </div>                            // 🟡
  );                                  // 🟠
};                                    // 🔴
```

### 2. JSON 配置文件

```json
{                                     // 🔴
  "dependencies": {                   // 🟠
    "react": {                        // 🟡
      "version": "18.0.0",
      "requires": {                   // 🟢
        "loose-envify": "^1.1.0"
      }                               // 🟢
    }                                 // 🟡
  }                                   // 🟠
}                                     // 🔴
```

### 3. Python 嵌套函数

```python
def process_data(items):              # 🔴
    result = []                       
    for item in items:                # 🟠
        if item.active:               # 🟡
            data = transform(         # 🟢
                item.value,
                lambda x: (           # 🔵
                    x * 2 if x > 0    # 🟣
                    else 0
                )                     # 🔵
            )                         # 🟢
            result.append(data)
    return result
```

### 4. Rust Match 表达式

```rust
match value {                         // 🔴
    Some(x) => {                      // 🟠
        match x {                     // 🟡
            1..=10 => {               // 🟢
                println!("Small");
            }                         // 🟢
            _ => println!("Large"),
        }                             // 🟡
    }                                 // 🟠
    None => println!("None"),
}                                     // 🔴
```

## 🎯 核心优势

### 1. 括号-缩进关联
- 同一层级使用相同颜色
- 缩进线颜色 = 对应括号颜色
- 快速定位匹配括号

### 2. 作用域可视化
```javascript
function outer() {        // 🔴 红色作用域
  let x = 1;
  
  function inner() {      // 🟠 橙色作用域
    let y = 2;
    return x + y;
  }                       // 🟠 橙色结束
  
  return inner();
}                         // 🔴 红色结束
```

### 3. 错误检测
```javascript
// 颜色不连续 = 可能括号不匹配
function broken() {       // 🔴
  if (true) {             // 🟠
    console.log("hi");
  // 缺少 }              // ⚠️ 颜色异常
}                         // 🔴 直接跳回，说明有问题
```

## ⌨️ 使用技巧

### 1. 快速定位括号

光标在括号上时：
- 对应的缩进线会高亮
- 快速找到匹配的另一半
- 整个作用域一目了然

### 2. 检查嵌套层级

```javascript
// 嵌套太深？看颜色就知道
function a() {            // 🔴 1层
  function b() {          // 🟠 2层
    function c() {        // 🟡 3层
      function d() {      // 🟢 4层
        function e() {    // 🔵 5层
          // 颜色太多 = 嵌套太深 = 需要重构
        }
      }
    }
  }
}
```

### 3. 代码审查

在审查代码时：
- 快速理解代码结构
- 发现不合理的嵌套
- 检查括号是否匹配

## 🎨 与主题配合

彩虹颜色会自动适配你的配色方案：
- 亮色主题：柔和的彩虹色
- 暗色主题：鲜艳的彩虹色
- 高对比度主题：明显的区分

## 📊 适用文件类型

特别适合：
- **JavaScript/TypeScript** - 回调地狱、JSX 嵌套
- **JSON/YAML** - 配置文件层级
- **Python** - 深层函数嵌套
- **Rust/Go** - match/if 嵌套
- **HTML/JSX** - 标签嵌套
- **Lua** - table 嵌套

## 🆚 对比普通缩进线

| 特性 | 普通缩进线 | 彩虹缩进线 |
|------|-----------|-----------|
| 颜色 | 单色 | 7色循环 |
| 括号关联 | ❌ | ✅ |
| 层级识别 | 需要数 | 看颜色 |
| 视觉效果 | 单调 | 生动 |
| 调试帮助 | 一般 | 很大 |

## 💡 实用场景

### 场景 1：重构深层嵌套

```javascript
// 发现嵌套太深（7种颜色都用完了）
getData().then(data => {          // 🔴
  processData(data).then(r => {   // 🟠
    validate(r).then(v => {       // 🟡
      save(v).then(s => {         // 🟢
        notify(s).then(n => {     // 🔵
          // 这里已经第5层了！    // 🟣
        });
      });
    });
  });
});

// 重构后（使用 async/await）
const data = await getData();     // 🔴 只需1层
const result = await processData(data);
const valid = await validate(result);
const saved = await save(valid);
await notify(saved);
```

### 场景 2：快速理解陌生代码

```typescript
// 第一次看到这段代码，颜色帮助快速理解结构
interface Config {                     // 🔴
  server: {                            // 🟠
    port: number;
    options: {                         // 🟡
      timeout: number;
      retry: {                         // 🟢
        attempts: number;
        delay: number;
      };                               // 🟢
    };                                 // 🟡
  };                                   // 🟠
}                                      // 🔴
```

### 场景 3：调试括号匹配

```rust
// 颜色突然跳变 = 括号可能有问题
fn complex() {                         // 🔴
    match value {                      // 🟠
        Some(x) => {                   // 🟡
            if x > 0 {                 // 🟢
                process(x)
            // 这里缺少了 }             
        // 颜色直接跳回 🟡 而不是 🟢
        }                              // 🟡 异常！
        None => {}
    }
}
```

## 🔧 自定义（可选）

如果想调整颜色或行为，可以在配置中自定义：

```lua
{
  "HiPhish/rainbow-delimiters.nvim",
  opts = {
    -- 自定义颜色
    highlight = {
      'RainbowRed',
      'RainbowYellow',
      'RainbowGreen',
      -- ...
    },
  },
}
```

## 📚 总结

**核心价值**：
- 🎨 **视觉化** - 代码结构清晰可见
- 🎯 **快速定位** - 括号对应一目了然
- 🐛 **辅助调试** - 发现括号不匹配
- 📖 **易读性** - 降低理解成本

**最适合**：
- 处理深层嵌套代码
- 阅读和审查代码
- 学习新项目结构
- 调试括号匹配问题

这是一个让代码更漂亮、更易读的视觉增强！🌈
