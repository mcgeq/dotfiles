local M = {
  spec = {},
}

local DEFAULTS = {
  enabled = false,
  blog_path = nil,
  content_dir = "content",
  posts_dir = "posts",
}

local util = require "core.util"

local function config()
  local user = require("user").lang or {}
  local private = util.require_if_exists "user.private" or {}
  local cfg = vim.tbl_deep_extend("force", {}, DEFAULTS, user.hugo or {}, private.hugo or {})
  return cfg
end

local function blog_root()
  local cfg = config()
  if not cfg.enabled then return nil end

  local raw = cfg.blog_path
  if not raw or raw == "" then
    vim.notify(
      "Hugo is enabled but `blog_path` is not set in `user.lang.hugo`.",
      vim.log.levels.WARN,
      { title = "Hugo" }
    )
    return nil
  end

  local path = raw:sub(1, 1) == "~" and vim.fn.expand(raw) or raw
  path = vim.fs.normalize(path)

  if vim.uv.fs_stat(path) == nil then
    vim.notify("Blog path does not exist: " .. path, vim.log.levels.WARN, { title = "Hugo" })
    return nil
  end

  return path
end

local function posts_dir(root)
  local cfg = config()
  return vim.fs.joinpath(root, cfg.content_dir or "content", cfg.posts_dir or "posts")
end

local function slugify(title)
  local chars = {}
  local pending = ""

  for byte in tostring(title):lower():gmatch "." do
    local value = byte:byte()
    if value < 128 then
      if pending ~= "" then
        if pending:match "^[\228-\233][\128-\191][\128-\191]$" then table.insert(chars, pending) end
        pending = ""
      end
      if byte:match "%w" then
        table.insert(chars, byte)
      elseif (byte:match "%s" or byte == "_") and chars[#chars] ~= "-" then
        table.insert(chars, "-")
      end
    else
      pending = pending .. byte
      if #pending == 3 then
        if pending:match "^[\228-\233][\128-\191][\128-\191]$" then table.insert(chars, pending) end
        pending = ""
      end
    end
  end

  local slug = table.concat(chars):gsub("%-+", "-"):gsub("^%-+", ""):gsub("%-+$", "")
  return slug
end

local function timestamp_now()
  local now = os.time()
  local utc = os.time(os.date("!*t", now))
  local minutes = math.floor((now - utc + 30) / 60)
  local sign = minutes < 0 and "-" or "+"
  local abs = math.abs(minutes)
  return ("%s%s%02d:%02d"):format(vim.fn.strftime("%Y-%m-%dT%H:%M:%S", now), sign, math.floor(abs / 60), abs % 60)
end

local function front_matter(title, series, stamp)
  local escaped = tostring(title):gsub("\\", "\\\\"):gsub('"', '\\"')
  local series_value = series ~= "" and ('["' .. series:gsub("\\", "\\\\"):gsub('"', '\\"') .. '"]') or "[]"
  return table.concat({
    "+++",
    'title = "' .. escaped .. '"',
    'author = ["mcge"]',
    "date = " .. stamp,
    "lastmod = " .. stamp,
    "draft = true",
    'description = ""',
    "categories = []",
    "tags = []",
    "series = " .. series_value,
    'image = ""',
    "hideFromHomePage = false",
    "+++",
    "---",
    "",
  }, "\n")
end

local function open_file(path) vim.cmd("edit " .. vim.fn.fnameescape(path)) end

local function write_manual_post(path, title, series, stamp)
  pcall(vim.fn.writefile, vim.split(front_matter(title, series, stamp), "\n", { plain = true }), path)
end

local function new_post()
  local root = blog_root()
  if not root then return end

  local ok_input, input = pcall(require, "snacks.input")
  if not ok_input then
    vim.notify("`snacks.input` is not available; cannot prompt for a title.", vim.log.levels.ERROR, { title = "Hugo" })
    return
  end

  input.input({
    prompt = "Post title: ",
  }, function(raw_title)
    local title = (raw_title or ""):gsub("^%s+", ""):gsub("%s+$", "")
    if title == "" then
      vim.notify("Post title is required.", vim.log.levels.WARN, { title = "Hugo" })
      return
    end

    input.input({
      prompt = "Series (folder, empty = none): ",
    }, function(raw_series)
      local series = (raw_series or ""):gsub("^%s+", ""):gsub("%s+$", "")
      local cfg = config()

      local now = os.time()
      local year = vim.fn.strftime("%Y", now)
      local month = vim.fn.strftime("%m", now)
      local stamp = timestamp_now()

      local slug = slugify(title)
      if slug == "" then slug = vim.fn.strftime("%Y%m%d%H%M%S", now) end

      local dir = posts_dir(root)
      if series ~= "" then dir = vim.fs.joinpath(dir, series) end
      dir = vim.fs.joinpath(dir, year, month)
      vim.fn.mkdir(dir, "p")

      local name = slug .. ".md"
      local path = vim.fs.joinpath(dir, name)

      if vim.uv.fs_stat(path) then
        vim.notify("Post already exists: " .. path, vim.log.levels.WARN, { title = "Hugo" })
        return
      end

      if vim.fn.executable "hugo" == 1 then
        local rel = vim.fs.joinpath(cfg.content_dir or "content", cfg.posts_dir or "posts", series, year, month, name)
        vim.fn.system({ "hugo", "new", rel }, nil, { cwd = root })
      end

      if vim.uv.fs_stat(path) == nil then write_manual_post(path, title, series, stamp) end

      if vim.uv.fs_stat(path) then
        vim.notify("Created post: " .. path, vim.log.levels.INFO, { title = "Hugo" })
        open_file(path)
      else
        vim.notify("Failed to create post.", vim.log.levels.ERROR, { title = "Hugo" })
      end
    end)
  end)
end

local function server_command()
  local root = blog_root()
  if not root then return end

  if vim.fn.executable "hugo" ~= 1 then
    vim.notify("`hugo` is not available on PATH.", vim.log.levels.WARN, { title = "Hugo" })
    return
  end

  local ok, terminal = pcall(require, "core.terminal")
  if not ok then return end

  terminal.open_float({ "hugo", "server", "-D" }, {
    cwd = root,
    title = " hugo server ",
  })
end

local function blog_files()
  local root = blog_root()
  if not root then return end
  local ok, snacks = pcall(require, "snacks")
  if not ok then return end
  snacks.picker.files { cwd = root }
end

local function blog_grep()
  local root = blog_root()
  if not root then return end
  local ok, snacks = pcall(require, "snacks")
  if not ok then return end
  snacks.picker.grep { cwd = root }
end

function M.setup()
  local cfg = config()
  if not cfg.enabled then return end

  if not cfg.blog_path or cfg.blog_path == "" then
    vim.notify(
      "Hugo is enabled but `blog_path` is not set in `user.lang.hugo`.",
      vim.log.levels.WARN,
      { title = "Hugo" }
    )
    return
  end

  local map = require("core.keymaps").map

  map("n", "<leader>hn", new_post, "New Hugo post")
  map("n", "<leader>hs", server_command, "Start Hugo server")
  map("n", "<leader>hf", blog_files, "Find blog files")
  map("n", "<leader>hg", blog_grep, "Grep blog content")

  vim.api.nvim_create_user_command("HugoNewPost", new_post, { desc = "Create a new Hugo post" })
  vim.api.nvim_create_user_command(
    "HugoServer",
    server_command,
    { desc = "Start `hugo server` in a floating terminal" }
  )
end

return M
