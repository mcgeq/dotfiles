return {
	-------------------------- plugins -------------------------------------------
	-- requires
	{ "kyazdani42/nvim-web-devicons" },
	{ "moll/vim-bbye" },
	{ "nvim-lua/plenary.nvim" },
	-- nvim-notify
	{
		"rcarriga/nvim-notify",
		config = function()
			require("pisces.plugins.nvim-notify")
		end,
	},
	-- nvim-tree
	{
		"kyazdani42/nvim-tree.lua",
		config = function()
			require("pisces.plugins.nvim-tree")
		end,
	},
	-- bufferline
	{
		"akinsho/bufferline.nvim",
		config = function()
			require("pisces.plugins.bufferline")
		end,
	},
	-- lualine
	{
		"nvim-lualine/lualine.nvim",
		config = function()
			require("pisces.plugins.lualine")
		end,
	},

	-- telescope
	-- telescope extensions
	{ "LinArcX/telescope-env.nvim" },
	{ "nvim-telescope/telescope-ui-select.nvim" },
	{ "nvim-telescope/telescope-live-grep-args.nvim" },
	{
		"nvim-telescope/telescope.nvim",
		-- opt = true,
		-- cmd = "Telescope",
		config = function()
			require("pisces.plugins.telescope")
		end,
	},

	-- alpha.nvim
	{
		"goolord/alpha-nvim",
		config = function()
			require("pisces.plugins.alpha")
		end,
	},

	-- treesitter
	{
		"HiPhish/rainbow-delimiters.nvim",
		config = function()
			require("pisces.plugins.rainbow-delimiters")
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
			ts_update()
		end,
		config = function()
			require("pisces.plugins.treesitter")
		end,
	},
	{ "windwp/nvim-ts-autotag" },
	{ "nvim-treesitter/nvim-treesitter-refactor" },
	{ "nvim-treesitter/nvim-treesitter-textobjects" },
	{ "RRethy/nvim-treesitter-endwise" },

	-- Comment
	{ "JoosepAlviste/nvim-ts-context-commentstring" },
	{
		"numToStr/Comment.nvim",
		config = function()
			require("pisces.plugins.comment")
		end,
	},
	-- which-key
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		opts = {
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		},
		keys = {
			{
				"<leader>?",
				function()
					require("which-key").show({ global = false })
				end,
				desc = "Buffer Local Keymaps (which-key)",
			},
		},
	},

	{
		"folke/flash.nvim",
		event = "VeryLazy",
		---@type Flash.Config
		opts = {},
  -- stylua: ignore
  keys = {
    { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end, desc = "Flash" },
    { "S", mode = { "n", "x", "o" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
    { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
    { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
    { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
  },
	},

	-- indent-blankline
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		config = function()
			require("pisces.plugins.indent-blankline")
		end,
	},
	-- use indent-blankline or hlchunk ?
	------------------------------------
	-- {
	--   "shellRaining/hlchunk.nvim",
	--   config = function()
	--     require("pisces.plugins.hlchunk")
	--   end,
	-- },
	-------------------------------------
	-- toggleterm
	{
		"akinsho/toggleterm.nvim",
		config = function()
			require("pisces.plugins.toggleterm")
		end,
	},

	-- nvim-surround
	{
		"kylechui/nvim-surround",
		config = function()
			require("pisces.plugins.nvim-surround")
		end,
	},

	-- nvim-autopairs
	{
		"windwp/nvim-autopairs",
		config = function()
			require("pisces.plugins.autopairs")
		end,
	},

	-- fidget.nvim
	{
		"j-hui/fidget.nvim",
		tag = "legacy",
		config = function()
			require("pisces.plugins.fidget")
		end,
	},

	-- todo-comments.nvim
	{
		"folke/todo-comments.nvim",
		config = function()
			require("pisces.plugins.todo-comments")
		end,
	},

	-- venn.nvim
	{
		"jbyuki/venn.nvim",
		config = function()
			require("pisces.plugins.venn")
		end,
	},

	-- zen mode
	{
		"folke/zen-mode.nvim",
		config = function()
			require("pisces.plugins.zen-mode")
		end,
	},

	{
		"kevinhwang91/nvim-ufo",
		dependencies = "kevinhwang91/promise-async",
		config = function()
			require("pisces.plugins.nvim-ufo")
		end,
	},

	{
		"LunarVim/bigfile.nvim",
		event = "BufReadPre",
		opts = {
			filesize = 1,
		},
		config = function(_, opts)
			require("bigfile").setup(opts)
		end,
	},

	------------------ Markdown -------------------------------------------------
	{
		"jakewvincent/mkdnflow.nvim",
		-- ft = { "markdown" }, -- lazy load
		config = function()
			require("pisces.plugins.mkdnflow")
		end,
	},

	{
		"iamcco/markdown-preview.nvim",
		enabled = function()
			local cfg = require("pisces").config.markdown
			return cfg and cfg.enable
		end,
		config = function()
			require("pisces.plugins.markdown-preview")
		end,
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
	},

	------------------ LSP ------------------------------------------------------

	-- Installer
	{ "williamboman/mason.nvim" },
	{ "williamboman/mason-lspconfig.nvim" },
	{ "WhoIsSethDaniel/mason-tool-installer.nvim" },
	-- Lspconfig
	{ "neovim/nvim-lspconfig" },
	-- Completion engine
	{ "hrsh7th/nvim-cmp" },
	-- Snippet engine
	{ "L3MON4D3/LuaSnip" },
	{ "saadparwaiz1/cmp_luasnip" },
	-- Completion sources
	{ "hrsh7th/cmp-vsnip" },
	{ "hrsh7th/cmp-nvim-lsp" }, -- { name = nvim_lsp }
	{ "hrsh7th/cmp-buffer" }, -- { name = 'buffer' },
	{ "hrsh7th/cmp-path" }, -- { name = 'path' }
	{ "hrsh7th/cmp-cmdline" }, -- { name = 'cmdline' }
	{ "hrsh7th/cmp-nvim-lsp-signature-help" }, -- { name = 'nvim_lsp_signature_help' }
	-- common snippets
	{ "rafamadriz/friendly-snippets" },
	-- UI improvement
	{ "onsails/lspkind-nvim" },

	{
		"folke/trouble.nvim",
		opts = {}, -- for default options, refer to the configuration section for custom setup.
		cmd = "Trouble",
		keys = {
			{
				"<leader>xx",
				"<cmd>Trouble diagnostics toggle<cr>",
				desc = "Diagnostics (Trouble)",
			},
			{
				"<leader>xX",
				"<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
				desc = "Buffer Diagnostics (Trouble)",
			},
			{
				"<leader>cs",
				"<cmd>Trouble symbols toggle focus=false<cr>",
				desc = "Symbols (Trouble)",
			},
			{
				"<leader>cl",
				"<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
				desc = "LSP Definitions / references / ... (Trouble)",
			},
			{
				"<leader>xL",
				"<cmd>Trouble loclist toggle<cr>",
				desc = "Location List (Trouble)",
			},
			{
				"<leader>xQ",
				"<cmd>Trouble qflist toggle<cr>",
				desc = "Quickfix List (Trouble)",
			},
		},
	},

	------------------ Code formatter -------------------------------------------

	-- { "mhartington/formatter.nvim" },
	-- { "jose-elias-alvarez/null-ls.nvim" },
	{ "nvimtools/none-ls.nvim" },

	------------------ Language enhancement -------------------------------------

	-- TypeScript
	-- { "jose-elias-alvarez/nvim-lsp-ts-utils", requires = "nvim-lua/plenary.nvim" },
	-- { "jose-elias-alvarez/typescript.nvim" },
	{
		"pmizio/typescript-tools.nvim",
		config = function()
			require("pisces.plugins.typescript-tools")
		end,
	},
	-- JSON
	{ "b0o/schemastore.nvim" },
	-- Rust
	-- { "simrat39/rust-tools.nvim" }, -- archive
	{
		"mrcjkb/rustaceanvim",
		version = "^5", -- Recommended
		lazy = false, -- This plugin is already lazy
	},
	-- Java
	{ "mfussenegger/nvim-jdtls" },

	--------------------- colorschemes ------------------------------------------

	-- tokyonight
	{
		"folke/tokyonight.nvim",
		config = function()
			require("pisces.plugins.tokyonight")
		end,
	},

	-- gruvbox
	{ "rktjmp/lush.nvim" },
	{ "ellisonleao/gruvbox.nvim" },

	-- nord
	{ "shaunsingh/nord.nvim" },

	-- onedark
	{ "ful1e5/onedark.nvim" },

	-- nightfox
	{ "EdenEast/nightfox.nvim" },

	-- dracula
	{ "Mofiqul/dracula.nvim" },

	-- kanagawa
	{ "rebelot/kanagawa.nvim" },

	--------------------- git ---------------------------------------------------

	{
		"lewis6991/gitsigns.nvim",
		config = function()
			require("pisces.plugins.gitsigns")
		end,
	},

	{
		"kdheepak/lazygit.nvim",
		lazy = false,
		cmd = {
			"LazyGit",
			"LazyGitConfig",
			"LazyGitCurrentFile",
			"LazyGitFilter",
			"LazyGitFilterCurrentFile",
		},
		-- optional for floating window border decoration
		dependencies = {
			"nvim-telescope/telescope.nvim",
			"nvim-lua/plenary.nvim",
		},
		-- setting the keybinding for LazyGit with 'keys' is recommended in
		-- order to load the plugin when the command is run for the first time
		keys = {
			{ "<leader>lg", "<cmd>LazyGitCurrentFile<cr>", desc = "LazyGit" },
		},
		config = function()
			require("telescope").load_extension("lazygit")
		end,
	},

	-- {
	-- 	"NeogitOrg/neogit",
	-- 	dependencies = {
	-- 		"nvim-lua/plenary.nvim", -- required
	-- 		"sindrets/diffview.nvim", -- optional - Diff integration--
	-- 		-- Only one of these is needed.
	-- 		"nvim-telescope/telescope.nvim", -- optional
	-- 		"ibhagwan/fzf-lua", -- optional
	-- 		"echasnovski/mini.pick", -- optional
	-- 	},
	-- 	config = true,
	-- },

	--------------------- DAP ---------------------------------------------------

	-- vimspector
	-- {
	--   "puremourning/vimspector",
	--   cmd = { "VimspectorInstall", "VimspectorUpdate" },
	--   fn = { "vimspector#Launch()", "vimspector#ToggleBreakpoint", "vimspector#Continue" },
	--   config = function()
	--     require("dap.vimspector")
	--   end,
	-- },

	-- nvim-dap
	{ "mfussenegger/nvim-dap" },
	{ "theHamsta/nvim-dap-virtual-text" },
	{ "rcarriga/nvim-dap-ui" },

	-- node
	{ "mxsdev/nvim-dap-vscode-js" },

	-- go
	{ "leoluz/nvim-dap-go" },

	--[[
  -- TODO: python not work yet

  {
  "mfussenegger/nvim-dap-python",
  requires = { "mfussenegger/nvim-dap" },
  config = function()
  require("dap-python").setup("/rs/nn/.local/share/nvim/mason/bin/debugpy")
  end,
  }

  --]]

	----------------- neotest ---------------------------
	{ "nvim-neotest/neotest-go" },
	{
		"nvim-neotest/neotest",
		config = function()
			require("pisces.plugins.neotest")
		end,
	},

	----------------- ai ---------------------------
	{
		"zbirenbaum/copilot.lua",
		cmd = "Copilot",
		event = "InsertEnter",
		config = function()
			require("pisces.plugins.copilot").copilot()
		end,
	},
	{
		"zbirenbaum/copilot-cmp",
		config = function()
			require("pisces.plugins.copilot").copilot_cmp()
		end,
	},
	{
		"CopilotC-Nvim/CopilotChat.nvim",
		dependencies = {
			{ "zbirenbaum/copilot.lua" }, -- or github/copilot.vim
			{ "nvim-lua/plenary.nvim" }, -- for curl, log wrapper
		},
		build = "make tiktoken", -- Only on MacOS or Linux
		config = function()
			require("pisces.plugins.copilot").copilot_chat()
		end,
	},
	-- Codeium
	{
		"Exafunction/codeium.nvim",
		config = function()
			require("pisces.plugins.codeium").init()
		end,
	},
}
