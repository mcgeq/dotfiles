-- 键位映射管理器
-- 统一管理所有自定义键位映射，便于查找和修改

local M = {}

-- 键位映射分类
M.categories = {
  file = "File Operations",
  buffer = "Buffer Management",
  window = "Window Navigation",
  edit = "Editing",
  search = "Search & Replace",
  git = "Git Operations",
  lsp = "LSP",
  debug = "Debugging",
  terminal = "Terminal",
  rust = "Rust Development",
  neorg = "Neorg Note-taking",
  util = "Utilities",
}

-- 文件操作
M.file = {
  ["<C-s>"] = { cmd = ":w!<CR>", desc = "Save file", modes = { "n", "i" } },
  ["<Leader>q"] = { cmd = ":q<CR>", desc = "Quit" },
  ["<Leader>Q"] = { cmd = ":qa!<CR>", desc = "Quit all without saving" },
}

-- Buffer 管理
M.buffer = {
  ["]b"] = { func = function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
  ["[b"] = { func = function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },
  ["<Leader>bd"] = {
    func = function()
      require("astroui.status.heirline").buffer_picker(
        function(bufnr) require("astrocore.buffer").close(bufnr) end
      )
    end,
    desc = "Close buffer from tabline",
  },
}

-- 窗口导航
M.window = {
  ["<C-h>"] = { cmd = "<C-w>h", desc = "Move to left window" },
  ["<C-j>"] = { cmd = "<C-w>j", desc = "Move to window below" },
  ["<C-k>"] = { cmd = "<C-w>k", desc = "Move to window above" },
  ["<C-l>"] = { cmd = "<C-w>l", desc = "Move to right window" },
}

-- 编辑操作
M.edit = {
  ["jk"] = { cmd = "<Esc>", desc = "Exit insert mode", modes = { "i" } },
  ["<"] = { cmd = "<gv", desc = "Indent left", modes = { "v" } },
  [">"] = { cmd = ">gv", desc = "Indent right", modes = { "v" } },
}

-- 终端操作
M.terminal = {
  ["<Esc><Esc>"] = { cmd = "<C-\\><C-n>", desc = "Exit terminal mode", modes = { "t" } },
}

-- Rust 开发 (crates.nvim)
-- 注意：插件由 astrocommunity.pack.rust 提供
-- 使用 <leader>R 前缀避免与 <leader>c (close buffer) 冲突
M.rust = {
  ["<leader>Ru"] = { func = function() require("crates").update_crate() end, desc = "Update crate" },
  ["<leader>RU"] = { func = function() require("crates").upgrade_crate() end, desc = "Upgrade crate" },
  ["<leader>Ra"] = { func = function() require("crates").update_all_crates() end, desc = "Update all crates" },
  ["<leader>RA"] = { func = function() require("crates").upgrade_all_crates() end, desc = "Upgrade all crates" },
  ["<leader>Rh"] = { func = function() require("crates").open_homepage() end, desc = "Open crate homepage" },
  ["<leader>Rd"] = { func = function() require("crates").open_documentation() end, desc = "Open crate docs" },
  ["<leader>Rr"] = { func = function() require("crates").open_repository() end, desc = "Open crate repository" },
}

-- Neorg 笔记管理
-- 使用 <leader>n 前缀
M.neorg = {
  ["<leader>nw"] = { cmd = ":Neorg workspace notes<CR>", desc = "Open notes workspace" },
  ["<leader>nt"] = { cmd = ":Neorg journal today<CR>", desc = "Open today's journal" },
  ["<leader>ny"] = { cmd = ":Neorg journal yesterday<CR>", desc = "Open yesterday's journal" },
  ["<leader>nm"] = { cmd = ":Neorg journal tomorrow<CR>", desc = "Open tomorrow's journal" },
  ["<leader>ni"] = { cmd = ":Neorg index<CR>", desc = "Open workspace index" },
  ["<leader>nr"] = { cmd = ":Neorg return<CR>", desc = "Close all norg buffers" },
  ["<leader>nc"] = { cmd = ":Neorg toc<CR>", desc = "Show table of contents" },
}

--- 构建 AstroCore 兼容的 mappings 表
---@return table mappings AstroCore mappings
function M.build_mappings()
  local mappings = { n = {}, i = {}, v = {}, t = {} }
  
  for category_key, category_maps in pairs(M) do
    if type(category_maps) == "table" and category_key ~= "categories" then
      for key, mapping in pairs(category_maps) do
        local modes = mapping.modes or { "n" }
        for _, mode in ipairs(modes) do
          mappings[mode] = mappings[mode] or {}
          mappings[mode][key] = {
            mapping.cmd or mapping.func,
            desc = mapping.desc,
          }
        end
      end
    end
  end
  
  return mappings
end

--- 获取键位映射列表（用于文档生成）
---@return table docs 文档列表
function M.get_docs()
  local docs = {}
  
  for category_key, category_maps in pairs(M) do
    if type(category_maps) == "table" and category_key ~= "categories" and category_key ~= "build_mappings" then
      local category_name = M.categories[category_key] or category_key
      docs[category_name] = {}
      
      for key, mapping in pairs(category_maps) do
        if type(mapping) == "table" then
          table.insert(docs[category_name], {
            key = key,
            desc = mapping.desc,
            modes = mapping.modes or { "n" },
          })
        end
      end
    end
  end
  
  return docs
end

--- 打印键位映射文档
function M.print_docs()
  local docs = M.get_docs()
  
  for category, mappings in pairs(docs) do
    print("\n" .. category .. ":")
    print(string.rep("-", 50))
    for _, mapping in ipairs(mappings) do
      local modes_str = table.concat(mapping.modes, ", ")
      print(string.format("  %-20s [%s] %s", mapping.key, modes_str, mapping.desc))
    end
  end
end

return M
