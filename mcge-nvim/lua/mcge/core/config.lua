---@class UserConfig
---@field colorscheme? "tokyonight" | "onedark" | "gruvbox" | "dracula" builtin colorscheme
---@field max_highlight_line_count? number disable code hightlight no big file for performance default 1000
---@field enable_imselect? boolean auto switch your input method, default false
---@field enable_very_magic_search? boolean enable regexp very magic mode
---@field fix_windows_clipboard? boolean fix yank problem on windows WSL2
---@field keys? McgeCommonkeys common keymappings
---@field s_windows? McgeSWindowConfig enabled by default
---@field s_tab? McgeSTabConfig disabled by default
---@field cmp? McgeCMPConfig Completion user config
---@field notify? McgeNotifyConfig nvim-notify plugin user config
---@field nvimTree? McgeNvimTreeConfig nvim-tree plugin user config
---@field bufferLine? McgeBufferLineConfig bufferline.nvim plugin user config
---@field telescope? McgeTelescopeConfig telescope.nvim plugin user config
---@field surround? McgeSurroundConfig nvim-surround plugin user config
---@field venn? McgeVENNConfig venn.nvim plugin user config
---@field zen? McgeZenConfig zen-mode.nvim plugin user config
---@field comment? McgeCommentConfig Comment.nvim plugin user config
---@field toggleterm? McgeToggleTermConfig toggleterm.nvim plugin user config
---@field neotest? McgeNeoTestConfig neotest plugin user config
---@field lsp? McgeLSPConfig LSP common config
---@field dap? McgeDAPConfig DAP common config
---@field frontend? McgeFrontendConfig Frontend development user config
---@field clangd? McgeClangdConfig Clangd user config
---@field golang? McgeGolangConfig Golang user config
---@field lua? McgeLuaConfig Lua development user config
---@field rust? McgeRustConfig Rust development user config
---@field bash? McgeBashConfig sh development user config
---@field python? McgePythonConfig python development user config
---@field ruby? McgeRubyConfig ruby development user config
---@field json? McgeJsonConfig Json user config
---@field markdown? McgeMarkdownConfig
---@field toml? McgeTomlConfig Toml user config
---@field yaml? McgeYamlConfig Yaml user config
---@field docker? McgeDockerConfig Docker user config
---@field solidity? McgeSolidityConfig
---@field java? McgeJavaConfig
---@field git? McgeGitConfig git user config
---@field mirror? McgeMirrorConfig mirror config

local UserConfig = {
	colorscheme = "gruvbox",
	max_highlight_line_count = 10000,
	enable_imselect = false,
	enable_very_magic_search = false,
	fix_windows_clipboard = false,

	---@class McgeCommonkeys
	keys = {
		leader_key = " ",
		n_save = "<leader>w",
		n_force_quit = "<leader>q",
		n_v_5j = "<C-j>",
		n_v_5k = "<C-k>",
		n_v_10k = "<C-u>",
		n_v_10j = "<C-d>",
		terminal_to_normal = "<Esc>",
	},

	---@class McgeBufferLineConfig
	bufferLine = {
		enable = true,
		keys = {
			prev = "<C-h>",
			next = "<C-l>",
			close = "<C-w>",
			close_left = "<leader>bh",
			close_other = "<leader>bo",
			close_pick = "<leader>bp",
		},
	},

	---@class McgeSWindowConfig
	s_windows = {
		enable = true,
		keys = {
			split_vertically = "sv",
			split_horizontally = "sh",
			close = "sc",
			close_other = "so",
			jump_left = { "<A-h>", "<leader>h" },
			jump_right = { "<A-l>", "<leader>l" },
			jump_up = { "<A-k>", "<leader>k" },
			jump_down = { "<A-j>", "<leader>j" },
			width_decrease = "s,",
			width_increase = "s.",
			height_decrease = "sj",
			height_increase = "sk",
			size_equal = "s=",
		},
	},

	---@class McgeSTabConfig
	s_tab = {
		enable = false,
		keys = {
			split = "ts",
			prev = "th",
			next = "tl",
			first = "tj",
			last = "tk",
			close = "tc",
		},
	},

	---@class McgeCMPConfig
	cmp = {
		enable = true,
		copilot = false,
		keys = {
			confirm = "<CR>",
			select_next_item = "<C-j>",
			select_prev_item = "<C-h>",
			scroll_doc_up = "<C-u>",
			scroll_doc_down = "<C-d>",
			complete = "<A-.>",
			abort = "<A-,>",
			-- luasnip
			snip_jump_next = "<C-l>",
			snip_jump_prev = "<C-h>",
			snip_next_choice = "<C-j>",
			snip_prev_choice = "<C-k>",
			copilot_panel = "<leader>cpp",
		},
	},

	---@class McgeNotifyConfig
	notify = {
		enable = true,
		---@type number in millionsecond
		timeout = 3000,
		---@type 'fade' | 'static' | 'slide'
		stages = "fade",
		---@type 'default' | 'minimal' | 'simple'
		render = "minimal",
	},

	---@class McgeNvimTreeConfig
	nvimTree = {
		enable = true,
		keys = {
			toggle = { "<A-m>", "<leader>m" },
			refresh = "R",
			-- open / close --
			edit = { "o", "<2-LeftMouse>" },
			close = "<BS>", -- close parent folder
			system_open = "<CR>",
			vsplit = "sv",
			split = "sh",
			tabnew = "st",
			-- movement --
			parent_node = "P",
			first_sibling = "K",
			last_sibling = "J",
			cd = ">",
			dir_up = "<",
			-- file toggle --
			toggle_git_ignored = "i", --.gitignore (git enable)
			toggle_dotfiles = ".", -- Hide (dotfiles)
			toggle_custom = "u", -- togglle custom config
			-- file operate --
			create = "a",
			remove = "d",
			rename = "r",
			cut = "x",
			copy = "c",
			paste = "p",
			copy_name = "y",
			copy_path = "Y",
			copy_absolute_path = "gy",
			toggle_file_info = "gh",
		},
	},

	---@class McgeTelescopeConfig
	telescope = {
		enable = true,
		keys = {
			find_files = { "<C-p>", "ff" },
			live_grep = "<C-f>",
			-- super find  "xx" -tmd ---@see telescope-live-grep-args.nvim
			live_grep_args = "sf",
			-- up and down
			move_selection_next = "<C-j>",
			move_selection_previous = "<C-k>",
			-- history
			cycle_history_next = "<Down>",
			cycle_history_prev = "<Up>",
			-- close window
			close = "<esc>",
			-- scrolling in preview window
			preview_scrolling_up = "<C-u>",
			preview_scrolling_down = "<C-d>",
		},
	},

	---@class McgeSurroundConfig
	surround = {
		enable = true,
		keys = {
			-- you surround
			normal = "ys",
			-- you surround line
			normal_cur = "yss",
			delete = "ds",
			change = "cs",
			-- visual mode
			visual = "s",
			visual_line = "gs",
			-- disable
			insert = false,
			insert_line = false,
			normal_line = false,
			normal_cur_line = false,
		},
	},

	---@class McgeVENNConfig
	venn = {
		enable = true,
		keys = {
			-- toggle keymappings for venn using <leader>v
			toggle = "<leader>v",
			up = "K",
			down = "J",
			left = "H",
			right = "L",
			-- draw a box by pressing "f" with visual selection
			draw_box = "f",
		},
	},

	---@class McgeZenConfig
	zen = {
		enable = true,
		keys = {
			toggle = "<leader>z",
		},
	},

	---@class McgeCommentConfig
	comment = {
		enable = true,
		-- normal mode
		toggler = {
			line = "gcc", -- line comment
			block = "gbc", -- block comment
		},
		-- visual mode
		opleader = {
			line = "gc",
			block = "gb",
		},
	},

	---@class McgeToggleTermConfig
	toggleterm = {
		-- enable 3 builtin terminal <leader>t a/b/c
		enable = true,
		toggle_float_window = "<leader>ta",
		toggle_float_window_command = nil,
		toggle_side_window = "<leader>tb",
		toggle_side_window_command = nil,
		toggle_bottom_window = "<leader>tc",
		toggle_bottom_window_command = nil,
	},

	---@class McgeLSPConfig
	lsp = {
		-- Goto the definition of the word under the cursor, if there's only one, otherwise show all options in Telescope
		definition = "gd",
		-- Goto the implementation of the word under the cursor if there's only one, otherwise show all options in Telescope
		implementation = "gi",
		-- Lists LSP references for word under the cursor
		references = "gr",
		-- Displays hover information
		hover = "gh",
		-- Lists LSP incoming calls for word under the cursor
		call_in = "gci",
		-- Lists LSP outgoing calls for word under the cursor
		call_out = "gco",
		-- Rename variable under the cursor
		rename = "<leader>rn",
		-- Popup code action
		code_action = "<leader>ca",
		-- Format the current buffer
		format = "<leader>f",

		----- Diagnostic ------

		-- Show diagnostics in a floating window.
		open_float = "gp",
		-- Move to the next diagnostic.
		goto_next = "gj",
		-- Move to the previous diagnostic.
		goto_prev = "gk",
	},

	---@class McgeDAPConfig
	dap = {
		-- start, stop
		continue = "<leader>dc",
		terminate = "<leader>de",
		--  stepOver, stepInto, stepOut,
		step_over = "<leader>dj",
		step_into = "<leader>di",
		step_out = "<leader>do",
		-- breakpoints
		toggle_breakpoint = "<leader>dt",
		clear_breakpoints = "<leader>dT",
		eval = "<leader>dh",
	},

	---@class McgeNeoTestConfig
	neotest = {
		toggle = "<leader>nt",
		run = "<leader>nr",
		run_file = "<leader>nf",
		run_dap = "<leader>nd",
		run_stop = "<leader>ns",
		output_open = "<leader>gh",
	},

	---@class McgeFrontendConfig
	frontend = {
		enable = false,
		---@type "eslint" | false
		linter = "eslint", -- :EslintFixAll command added
		---@type false | "prettier" | "tsserver"
		formatter = "tsserver",
		format_on_save = false,
		cspell = false,
		tailwindcss = true,
		prisma = false,
		-- vue will take over typescript lsp
		vue = false,
		-- extra lsp command provided by typescript.nvim
		typescript = {
			keys = {
				ts_organize = "gs",
				ts_rename_file = "gR",
				ts_add_missing_import = "ga",
				ts_remove_unused = "gu",
				ts_fix_all = "gf",
				ts_goto_source = "gD",
			},
		},
	},

	---@class McgeClangdConfig
	clangd = {
		enable = false,
		lsp = "clangd",
		-- linter = "clangd-tidy",
		formatter = "clang-format",
		format_on_save = false,
	},

	---@class McgeGolangConfig
	golang = {
		enable = false,
		lsp = "gopls",
		linter = "golangci-lint",
		formatter = "gofmt",
		format_on_save = false,
	},

	---@class McgeLuaConfig
	lua = {
		enable = true,
		lsp = "lua_ls",
		formatter = "stylua",
		format_on_save = true,
	},

	---@class McgeRustConfig
	rust = {
		enable = false,
		lsp = "rust_analyzer",
		-- rustup component add rustfmt
		formatter = "rustfmt",
		format_on_save = false,
	},

	---@class McgeBashConfig
	bash = {
		enable = false,
		lsp = "bashls",
		--  brew install shfmt
		formatter = "shfmt",
		format_on_save = false,
	},

	---@class McgePythonConfig
	python = {
		enable = false,
		-- can be pylsp or pyright
		lsp = "pylsp",
		-- pip install black
		-- asdf reshim python
		formatter = "black",
		format_on_save = false,
	},

	---@class McgeRubyConfig
	ruby = {
		enable = false,
		lsp = "ruby_ls",
		-- gem install rubocop
		formatter = "rubocop",
		format_on_save = false,
	},

	---@class McgeJsonConfig
	json = {
		enable = false,
		lsp = "jsonls",
		---@type "jsonls" | "prettier"
		formatter = "jsonls",
		format_on_save = false,
	},

	---@class McgeMarkdownConfig
	markdown = {
		enable = false,
		mkdnflow = {
			next_link = "gn",
			prev_link = "gp",
			next_heading = "gj",
			prev_heading = "gk",
			go_back = "<C-o>",
			follow_link = "gd",
			toggle_item = "tt",
		},
		formatter = "prettier",
		format_on_save = false,
		wrap = true,
		---@type "dark" | "light"
		theme = "dark",
	},

	---@class McgeTomlConfig
	toml = {
		enable = false,
		lsp = "taplo",
	},

	---@class McgeYamlConfig
	yaml = {
		enable = false,
		lsp = "yamlls",
		---@type "prettier" | false
		formatter = "prettier",
		format_on_save = false,
	},

	---@class McgeDockerConfig
	docker = {
		enable = false,
		lsp = "dockerls",
	},

	---@class McgeSolidityConfig
	solidity = {
		enable = false,
		---@type "solhint" | false
		linter = "solhint",
		format_on_save = false,
	},

	---@class McgeJavaConfig
	java = {
		enable = false,
	},

	---@class McgeGitConfig
	git = {
		enable = true,
		code_actions = "gitsigns",
		-- sign display
		signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
		numhl = false, -- Toggle with `:Gitsigns toggle_numhl`
		linehl = false, -- Toggle with `:Gitsigns toggle_linehl`
		word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
		current_line_blame = false, -- Toggle with `:Gitsigns toggle_current_line_blame`
	},

	---@class McgeMirrorConfig
	mirror = {
		-- treesitter = "https://github.com/",
		treesitter = false,
		packer = "https://github.com/",
		-- TODO: LSP DAP mirror config
		-- carefully change these value
	},
}

return UserConfig
