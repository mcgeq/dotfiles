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
			require("mcge.plugins.nvim-notify")
		end,
	},
	-- nvim-tree
	{
		"kyazdani42/nvim-tree.lua",
		config = function()
			require("mcge.plugins.nvim-tree")
		end,
	},
	-- bufferline
	{
		"akinsho/bufferline.nvim",
		config = function()
			require("mcge.plugins.bufferline")
		end,
	},
	-- lualine
	{
		"nvim-lualine/lualine.nvim",
		config = function()
			require("mcge.plugins.lualine")
		end,
	},

	-- telescope
	-- telescope extensions
	{ "LinArcX/telescope-env.nvim" },
	{ "nvim-telescope/telescope-ui-select.nvim" },
	{ "nvim-telescope/telescope-live-grep-args.nvim" },
	{
		"nvim-telescope/telescope.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		-- opt = true,
		-- cmd = "Telescope",
		config = function()
			require("mcge.plugins.telescope")
		end,
	},

	-- alpha.nvim
	{
		"goolord/alpha-nvim",
		config = function()
			require("mcge.plugins.alpha")
		end,
	},

	-- treesitter
	{
		"HiPhish/rainbow-delimiters.nvim",
		config = function()
			require("mcge.plugins.rainbow-delimiters")
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
			ts_update()
		end,
		config = function()
			require("mcge.plugins.treesitter")
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
			require("mcge.plugins.comment")
		end,
	},

	-- indent-blankline
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		config = function()
			require("mcge.plugins.indent-blankline")
		end,
	},
	-- use indent-blankline or hlchunk ?
	------------------------------------
	-- {
	--   "shellRaining/hlchunk.nvim",
	--   config = function()
	--     require("mcge.plugins.hlchunk")
	--   end,
	-- },
	-------------------------------------
	-- toggleterm
	{
		"akinsho/toggleterm.nvim",
		config = function()
			require("mcge.plugins.toggleterm")
		end,
	},

	-- nvim-surround
	{
		"kylechui/nvim-surround",
		config = function()
			require("mcge.plugins.nvim-surround")
		end,
	},

	-- nvim-autopairs
	{
		"windwp/nvim-autopairs",
		config = function()
			require("mcge.plugins.autopairs")
		end,
	},

	-- fidget.nvim
	{
		"j-hui/fidget.nvim",
		tag = "legacy",
		config = function()
			require("mcge.plugins.fidget")
		end,
	},

	-- todo-comments.nvim
	{
		"folke/todo-comments.nvim",
		config = function()
			require("mcge.plugins.todo-comments")
		end,
	},

	-- venn.nvim
	{
		"jbyuki/venn.nvim",
		config = function()
			require("mcge.plugins.venn")
		end,
	},

	-- zen mode
	{
		"folke/zen-mode.nvim",
		config = function()
			require("mcge.plugins.zen-mode")
		end,
	},

	{
		"kevinhwang91/nvim-ufo",
		dependencies = "kevinhwang91/promise-async",
		config = function()
			require("mcge.plugins.nvim-ufo")
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
			require("mcge.plugins.mkdnflow")
		end,
	},

	{
		"iamcco/markdown-preview.nvim",
		enabled = function()
			local cfg = require("mcge").config.markdown
			return cfg and cfg.enable
		end,
		config = function()
			require("mcge.plugins.markdown-preview")
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
			require("mcge.plugins.typescript-tools")
		end,
	},
	-- JSON
	{ "b0o/schemastore.nvim" },
	-- Rust
	-- { "simrat39/rust-tools.nvim" },
	{
		"mrcjkb/rustaceanvim",
		version = "^5", -- Recommended
		lazy = false, -- This plugin is already lazy
	},
	-- Java
	{ "mfussenegger/nvim-jdtls" },

	--------------------- colorschemes ------------------------------------------

	-- molokai
	{
		"tanvirtin/monokai.nvim",
		config = function()
			require("mcge.plugins.monokai")
		end,
	},

	-- tokyonight
	{
		"folke/tokyonight.nvim",
		config = function()
			require("mcge.plugins.tokyonight")
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
			require("mcge.plugins.gitsigns")
		end,
	},

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
			require("mcge.plugins.neotest")
		end,
	},

	----------------- ai ---------------------------
	{
		"zbirenbaum/copilot.lua",
		cmd = "Copilot",
		event = "InsertEnter",
		config = function()
			require("mcge.plugins.copilot").copilot()
		end,
	},
	{
		"zbirenbaum/copilot-cmp",
		config = function()
			require("mcge.plugins.copilot").copilot_cmp()
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
			require("mcge.plugins.copilot").copilot_chat()
		end,
	},
	-- Codeium
	{
		"Exafunction/codeium.nvim",
		config = function()
			require("mcge.plugins.codeium").init()
		end,
	},
}
