-- TODO:
-- [ ] Do the anchor thing / check harpoon?
-- [ ] Fix telescope moving up and down
-- [ ] DAP
-- [ ] Nice icons / configure lua line
-- [ ] Disable cmp before typing starts
-- [ ] ToggleTerm, maybe?


-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- [[ Install `lazy.nvim` plugin manager ]]
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- [[ Configure plugins ]]
-- NOTE: Here is where you install your plugins.
--  You can configure plugins using the `config` key.
--
--  You can also configure plugins after the setup call,
--    as they will be available in your neovim runtime.
require('lazy').setup({
  -- NOTE: First, some plugins that don't require any configuration

  -- Git related plugins
  'tpope/vim-fugitive', -- makes git commands available through :G or :Git

  -- Detect tabstop and shiftwidth automatically
  'tpope/vim-sleuth',

  -- NOTE: This is where your plugins related to LSP can be installed.
  --  The configuration is done below. Search for lspconfig to find it below.
  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      { 'williamboman/mason.nvim',          config = true },
      { 'williamboman/mason-lspconfig.nvim' },

      -- Useful status updates for LSP (the text that appears on the lower right corner)
      -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
      { 'j-hui/fidget.nvim',                opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      'folke/neodev.nvim',
    },
  },

  {
    -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',
    },
    event = "InsertEnter",
    opts = function()
      local cmp = require "cmp"
      local snip_status_ok, luasnip = pcall(require, "luasnip")
      --local lspkind_status_ok, lspkind = pcall(require, "lspkind")
      -- local utils = require "astronvim.utils"
      if not snip_status_ok then return end
      local border_opts = {
        border = "rounded",
        winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",
      }

      local function has_words_before()
        local line, col = (unpack or table.unpack)(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match "%s" == nil
      end

      return {
        preselect = cmp.PreselectMode.None,
        formatting = {
          fields = { "kind", "abbr", "menu" },
          -- format = lspkind_status_ok and lspkind.cmp_format(utils.plugin_opts "lspkind.nvim") or nil,
        },
        snippet = {
          expand = function(args) luasnip.lsp_expand(args.body) end,
        },
        duplicates = {
          nvim_lsp = 1,
          luasnip = 1,
          cmp_tabnine = 1,
          buffer = 1,
          path = 1,
        },
        confirm_opts = {
          behavior = cmp.ConfirmBehavior.Replace,
          select = false,
        },
        window = {
          completion = cmp.config.window.bordered(border_opts),
          documentation = cmp.config.window.bordered(border_opts),
        },
        mapping = {
          ["<Up>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
          ["<Down>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
          ["<C-p>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
          ["<C-n>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
          ["<C-k>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Insert },
          ["<C-j>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Insert },
          ["<C-u>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
          ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
          ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
          ["<C-y>"] = cmp.config.disable,
          ["<C-e>"] = cmp.mapping { i = cmp.mapping.abort(), c = cmp.mapping.close() },
          ["<CR>"] = cmp.mapping.confirm { select = false },
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        },
        sources = cmp.config.sources {
          { name = "nvim_lsp", priority = 1000 },
          { name = "luasnip",  priority = 750 },
          { name = "buffer",   priority = 500 },
          { name = "path",     priority = 250 },
        },
      }
    end,
  },

  -- Undo tree
  { 'mbbill/undotree' },

  -- Useful plugin to show you pending keybinds.
  { 'folke/which-key.nvim', opts = {} },
  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map({ 'n', 'v' }, ']c', function()
          if vim.wo.diff then
            return ']c'
          end
          vim.schedule(function()
            gs.next_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, desc = 'Jump to next hunk' })

        map({ 'n', 'v' }, '[c', function()
          if vim.wo.diff then
            return '[c'
          end
          vim.schedule(function()
            gs.prev_hunk()
          end)
          return '<Ignore>'
        end, { expr = true, desc = 'Jump to previous hunk' })

        -- Actions
        -- visual mode
        map('v', '<leader>ghs', function()
          gs.stage_hunk { vim.fn.line '.', vim.fn.line 'v' }
        end, { desc = 'stage git hunk' })
        map('v', '<leader>ghr', function()
          gs.reset_hunk { vim.fn.line '.', vim.fn.line 'v' }
        end, { desc = 'reset git hunk' })
        -- normal mode
        map('n', '<leader>ghs', gs.stage_hunk, { desc = 'git stage hunk' })
        map('n', '<leader>ghr', gs.reset_hunk, { desc = 'git reset hunk' })
        map('n', '<leader>ghS', gs.stage_buffer, { desc = 'git Stage buffer' })
        map('n', '<leader>ghu', gs.undo_stage_hunk, { desc = 'undo stage hunk' })
        map('n', '<leader>ghR', gs.reset_buffer, { desc = 'git Reset buffer' })
        map('n', '<leader>ghp', gs.preview_hunk, { desc = 'preview git hunk' })
        map('n', '<leader>ghb', function()
          gs.blame_line { full = false }
        end, { desc = 'git blame line' })
        map('n', '<leader>ghd', gs.diffthis, { desc = 'git diff against index' })
        map('n', '<leader>ghD', function()
          gs.diffthis '~'
        end, { desc = 'git diff against last commit' })

        -- Toggles
        map('n', '<leader>gtb', gs.toggle_current_line_blame, { desc = 'toggle git blame line' })
        map('n', '<leader>gtd', gs.toggle_deleted, { desc = 'toggle git show deleted' })

        -- Text object
        map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>', { desc = 'select git hunk' })
      end,
    },
  },

  {
    'nyoom-engineering/oxocarbon.nvim',
    config = function()
      -- Set theme
      vim.opt.background = "dark" -- set this to dark or light
      vim.cmd("colorscheme oxocarbon")
    end
  },

  {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = true,
        theme = 'oxocarbon',
        component_separators = '|',
        section_separators = '',
      },
    },
  },

  {
    "goolord/alpha-nvim",
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = function()
      local dashboard = require("alpha.themes.dashboard")
      -- customize the dashboard header
      dashboard.section.header.val = {
        "   ⣴⣶⣤⡤⠦⣤⣀⣤⠆     ⣈⣭⣿⣶⣿⣦⣼⣆          ",
        "    ⠉⠻⢿⣿⠿⣿⣿⣶⣦⠤⠄⡠⢾⣿⣿⡿⠋⠉⠉⠻⣿⣿⡛⣦       ",
        "          ⠈⢿⣿⣟⠦ ⣾⣿⣿⣷    ⠻⠿⢿⣿⣧⣄     ",
        "           ⣸⣿⣿⢧ ⢻⠻⣿⣿⣷⣄⣀⠄⠢⣀⡀⠈⠙⠿⠄    ",
        "          ⢠⣿⣿⣿⠈    ⣻⣿⣿⣿⣿⣿⣿⣿⣛⣳⣤⣀⣀   ",
        "   ⢠⣧⣶⣥⡤⢄ ⣸⣿⣿⠘  ⢀⣴⣿⣿⡿⠛⣿⣿⣧⠈⢿⠿⠟⠛⠻⠿⠄  ",
        "  ⣰⣿⣿⠛⠻⣿⣿⡦⢹⣿⣷   ⢊⣿⣿⡏  ⢸⣿⣿⡇ ⢀⣠⣄⣾⠄   ",
        " ⣠⣿⠿⠛ ⢀⣿⣿⣷⠘⢿⣿⣦⡀ ⢸⢿⣿⣿⣄ ⣸⣿⣿⡇⣪⣿⡿⠿⣿⣷⡄  ",
        " ⠙⠃   ⣼⣿⡟  ⠈⠻⣿⣿⣦⣌⡇⠻⣿⣿⣷⣿⣿⣿ ⣿⣿⡇ ⠛⠻⢷⣄ ",
        "      ⢻⣿⣿⣄   ⠈⠻⣿⣿⣿⣷⣿⣿⣿⣿⣿⡟ ⠫⢿⣿⡆     ",
        "       ⠻⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⡟⢀⣀⣤⣾⡿⠃     ",
        "                                   ",
        " -<  N    E    O    V    I    M  >-",
      }
      dashboard.section.header.opts.hl = "DashboardHeader"
      dashboard.section.footer.opts.hl = "DashboardFooter"

      dashboard.section.buttons.val = {
        dashboard.button("SPC o", "  File Explorer"),
        dashboard.button("SPC ff", "  Find File"),
        dashboard.button("SPC fF", "  Find in Files"),
        dashboard.button("SPC ft", "  Find TODOs"),
        dashboard.button("u", "  Update plugins", '<cmd>Lazy sync<cr>'),
        dashboard.button("q", "󰗼  Quit", "<cmd>qa<cr>")
      }

      dashboard.config.layout = {
        { type = "padding", val = vim.fn.max { 2, vim.fn.floor(vim.fn.winheight(0) * 0.2) } },
        dashboard.section.header,
        { type = "padding", val = 5 },
        dashboard.section.buttons,
        { type = "padding", val = 3 },
        dashboard.section.footer,
      }
      dashboard.config.opts.noautocmd = true
      return dashboard
    end,
    config = function(_, opts)
      require("alpha").setup(opts.config)

      vim.api.nvim_create_autocmd("User", {
        pattern = "LazyVimStarted",
        desc = "Add Alpha dashboard footer",
        once = true,
        callback = function()
          local stats = require("lazy").stats()
          local ms = math.floor(stats.startuptime * 100 + 0.5) / 100
          opts.section.footer.val = { "NeoVim loaded " .. stats.count .. " plugins  in " .. ms .. "ms" }
          pcall(vim.cmd.AlphaRedraw)
        end,
      })
    end
  },

  {
    -- Add indentation guides even on blank lines
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    opts = {
      indent = { char = "▏" },
      scope = { show_start = false, show_end = false },
      exclude = {
        buftypes = {
          "nofile",
          "terminal",
        },
        filetypes = {
          "help",
          "startify",
          "aerial",
          "alpha",
          "dashboard",
          "lazy",
          "neogitstatus",
          "NvimTree",
          "neo-tree",
          "Trouble",
        },
      },
    },
  },

  -- "gc" to comment visual regions/lines
  { 'numToStr/Comment.nvim',                  opts = {} },

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- Fuzzy Finder Algorithm which requires local dependencies to be built.
      -- Only load if `make` is available. Make sure you have the system
      -- requirements installed.
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        -- NOTE: If you are having trouble with this installation,
        --       refer to the README for telescope-fzf-native for more instructions.
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
    cmd = "Telescope",
    opts = function()
      local actions = require "telescope.actions"
      return {
        defaults = {
          git_worktrees = vim.g.git_worktrees,
          -- prompt_prefix = get_icon("Selected", 1),
          -- selection_caret = get_icon("Selected", 1),
          path_display = { "truncate" },
          sorting_strategy = "ascending",
          layout_config = {
            horizontal = { prompt_position = "top", preview_width = 0.55 },
            vertical = { mirror = false },
            width = 0.87,
            height = 0.80,
            preview_cutoff = 120,
          },
          mappings = {
            i = {
              ["<C-n>"] = actions.cycle_history_next,
              ["<C-p>"] = actions.cycle_history_prev,
              ["<C-j>"] = actions.move_selection_next,
              ["<C-k>"] = actions.move_selection_previous,
            },
            n = { q = actions.close },
          },
        },
      }
    end,
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects',
    },
    build = ':TSUpdate',
  },

  -- Show header of current function on top
  { "nvim-treesitter/nvim-treesitter-context" },

  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "main", -- HACK: force neo-tree to checkout `main` for initial v3 migration since default branch has changed
    dependencies = { "MunifTanjim/nui.nvim" },
    cmd = "Neotree",
    init = function() vim.g.neo_tree_remove_legacy_commands = true end,
    opts = function()
      -- local get_icon = utils.get_icon
      return {
        auto_clean_after_session_restore = true,
        close_if_last_window = true,
        sources = { "filesystem", "buffers", "git_status" },
        source_selector = {
          winbar = true,
          content_layout = "center",
          -- sources = {
          --   { source = "filesystem", display_name = get_icon("FolderClosed", 1, true) .. "File" },
          --   { source = "buffers", display_name = get_icon("DefaultFile", 1, true) .. "Bufs" },
          --   { source = "git_status", display_name = get_icon("Git", 1, true) .. "Git" },
          --   { source = "diagnostics", display_name = get_icon("Diagnostic", 1, true) .. "Diagnostic" },
          -- },
        },
        default_component_configs = {
          indent = { padding = 0 },
          -- icon = {
          --   folder_closed = get_icon "FolderClosed",
          --   folder_open = get_icon "FolderOpen",
          --   folder_empty = get_icon "FolderEmpty",
          --   folder_empty_open = get_icon "FolderEmpty",
          --   default = get_icon "DefaultFile",
          -- },
          -- modified = { symbol = get_icon "FileModified" },
          -- git_status = {
          --   symbols = {
          --     added = get_icon "GitAdd",
          --     deleted = get_icon "GitDelete",
          --     modified = get_icon "GitChange",
          --     renamed = get_icon "GitRenamed",
          --     untracked = get_icon "GitUntracked",
          --     ignored = get_icon "GitIgnored",
          --     unstaged = get_icon "GitUnstaged",
          --     staged = get_icon "GitStaged",
          --     conflict = get_icon "GitConflict",
          --   },
          -- },
        },
        commands = {
          system_open = function(state)
            vim.ui.open(state.tree:get_node():get_id())
          end,
          parent_or_close = function(state)
            local node = state.tree:get_node()
            if (node.type == "directory" or node:has_children()) and node:is_expanded() then
              state.commands.toggle_node(state)
            else
              require("neo-tree.ui.renderer").focus_node(state, node:get_parent_id())
            end
          end,
          child_or_open = function(state)
            local node = state.tree:get_node()
            if node.type == "directory" or node:has_children() then
              if not node:is_expanded() then -- if unexpanded, expand
                state.commands.toggle_node(state)
              else                           -- if expanded and has children, seleect the next child
                require("neo-tree.ui.renderer").focus_node(state, node:get_child_ids()[1])
              end
            else -- if not a directory just open it
              state.commands.open(state)
            end
          end,
          copy_selector = function(state)
            local node = state.tree:get_node()
            local filepath = node:get_id()
            local filename = node.name
            local modify = vim.fn.fnamemodify

            local vals = {
              ["BASENAME"] = modify(filename, ":r"),
              ["EXTENSION"] = modify(filename, ":e"),
              ["FILENAME"] = filename,
              ["PATH (CWD)"] = modify(filepath, ":."),
              ["PATH (HOME)"] = modify(filepath, ":~"),
              ["PATH"] = filepath,
              ["URI"] = vim.uri_from_fname(filepath),
            }

            local options = vim.tbl_filter(function(val) return vals[val] ~= "" end, vim.tbl_keys(vals))
            if vim.tbl_isempty(options) then
              -- utils.notify("No values to copy", vim.log.levels.WARN)
              return
            end
            table.sort(options)
            vim.ui.select(options, {
              prompt = "Choose to copy to clipboard:",
              format_item = function(item) return ("%s: %s"):format(item, vals[item]) end,
            }, function(choice)
              local result = vals[choice]
              if result then
                -- utils.notify(("Copied: `%s`"):format(result))
                vim.fn.setreg("+", result)
              end
            end)
          end,
          find_in_dir = function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            require("telescope.builtin").find_files {
              cwd = node.type == "directory" and path or vim.fn.fnamemodify(path, ":h"),
            }
          end,
        },
        window = {
          width = 30,
          mappings = {
            ["<space>"] = false, -- disable space until we figure out which-key disabling
            ["[b"] = "prev_source",
            ["]b"] = "next_source",
            F = "find_in_dir",
            O = "system_open",
            Y = "copy_selector",
            h = "parent_or_close",
            l = "child_or_open",
            o = "open",
          },
          fuzzy_finder_mappings = { -- define keymaps for filter popup window in fuzzy_finder_mode
            ["<C-j>"] = "move_cursor_down",
            ["<C-k>"] = "move_cursor_up",
          },
        },
        filesystem = {
          follow_current_file = { enabled = true },
          hijack_netrw_behavior = "open_current",
          use_libuv_file_watcher = true,
        },
        event_handlers = {
          {
            event = "neo_tree_buffer_enter",
            handler = function(_) vim.opt_local.signcolumn = "auto" end,
          },
        },
      }
    end,
  },

  -- Conjure for REPL
  {
    'PaterJason/cmp-conjure',
    config = function()
      local cmp = require "cmp"
      local config = cmp.get_config()
      table.insert(config.sources, {
        name = "buffer",
        option = {
          sources = {
            { name = "conjure" },
          },
        },
      })
      cmp.setup(config)
    end,
  },

  {
    'Olical/conjure',

    ft = { "scheme", "fennel", "lua", "python", "rust", "hy", "scheme", "guile", "common-lisp" }, -- etc
    init = function()
      -- Set configuration options here
      -- vim.g["conjure#debug"] = true
    end,
  },

  -- Format on save
  { 'stevearc/conform.nvim',    opts = {} },

  -- Highlight TODOs and others
  { "folke/todo-comments.nvim", opts = {} },

  -- Autopairs
  {
    'altermo/ultimate-autopair.nvim',
    event = { 'InsertEnter', 'CmdlineEnter' },
    branch = 'v0.6', --recomended as each new version will have breaking changes
    opts = {
      --Config goes here
    },
  }

  -- NOTE: Next Step on Your Neovim Journey: Add/Configure additional "plugins" for kickstart
  --       These are some example plugins that I've included in the kickstart repository.
  --       Uncomment any of the lines below to enable them.
  -- require 'kickstart.plugins.autoformat',
  -- require 'kickstart.plugins.debug',

  -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
  --    You can use this folder to prevent any conflicts with this init.lua if you're interested in keeping
  --    up-to-date with whatever is in the kickstart repo.
  --    Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
  --
  --    For additional information see: https://github.com/folke/lazy.nvim#-structuring-your-plugins
  -- { import = 'custom.plugins' },
}, {})

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable relative line neumbers
vim.wo.relativenumber = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menu,menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- Hide command line unless needed
vim.o.cmdheight = 0

-- Copy the previous indentation on autoindenting
vim.o.copyindent = true

-- Highlight the text line of the cursor
vim.o.cursorline = true

-- Enable the use of space in tab
vim.o.expandtab = true

-- File content encoding for the buffer
-- vim.o.fileencoding = "utf-8" -- Causes error when Lazy is lauched

-- Disable `~` on nonexistent lines
-- vim.o.fillchars = { eob = " " }

-- Enable fold for nvim-ufo
vim.o.foldenable = true

-- Set high foldlevel for nvim-ufo
vim.o.foldlevel = 99

-- Start with all code unfolded
vim.o.foldlevelstart = 99

-- Show foldcolumn in nvim 0.9
vim.o.foldcolumn = vim.fn.has "nvim-0.9" == 1 and "1" or nil

-- Number of commands to remember in a history table
vim.o.history = 100

-- Global statusline
vim.o.laststatus = 3

-- Wrap lines at 'breakat'
vim.o.linebreak = true

-- Preserve indent structure as much as possible
vim.o.preserveindent = true

-- Height of the pop up menu
vim.o.pumheight = 10

-- Disable showing modes in command line
vim.o.showmode = false

-- Splitting a new window below the current one
vim.o.splitbelow = true

-- Splitting a new window at the right of the current one
vim.o.splitright = true

-- Set terminal title to the filename and path
vim.o.title = true

-- Allow going past end of line in visual block mode
vim.o.virtualedit = "block"

-- Disable wrapping of lines longer than the width of window
vim.o.wrap = false

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- Telescope live_grep in git root
-- Function to find the git root directory based on the current buffer's path
local function find_git_root()
  -- Use the current buffer's path as the starting point for the git search
  local current_file = vim.api.nvim_buf_get_name(0)
  local current_dir
  local cwd = vim.fn.getcwd()
  -- If the buffer is not associated with a file, return nil
  if current_file == '' then
    current_dir = cwd
  else
    -- Extract the directory from the current file's path
    current_dir = vim.fn.fnamemodify(current_file, ':h')
  end

  -- Find the Git root directory from the current file's path
  local git_root = vim.fn.systemlist('git -C ' .. vim.fn.escape(current_dir, ' ') .. ' rev-parse --show-toplevel')[1]
  if vim.v.shell_error ~= 0 then
    print 'Not a git repository. Searching on current working directory'
    return cwd
  end
  return git_root
end

-- Custom live_grep function to search in git root
local function live_grep_git_root()
  local git_root = find_git_root()
  if git_root then
    require('telescope.builtin').live_grep {
      search_dirs = { git_root },
    }
  end
end

vim.api.nvim_create_user_command('LiveGrepGitRoot', live_grep_git_root, {})

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'bash', 'latex', 'cmake', 'ninja', 'fennel', 'arduino', 'bibtex', 'markdown', 'markdown_inline', 'commonlisp', 'cuda', 'json', 'toml', 'diff', 'git_config', 'git_rebase', 'gitattributes', 'gitcommit', 'gitignore', 'scheme', 'ssh_config' },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    auto_install = false,

    highlight = { enable = true },
    indent = { enable = true },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = '<c-space>',
        node_incremental = '<c-space>',
        scope_incremental = '<c-s>',
        node_decremental = '<M-space>',
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ['aa'] = '@parameter.outer',
          ['ia'] = '@parameter.inner',
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
        },
      },
      -- swap = {
      --   enable = true,
      --   swap_next = {
      --     ['<leader>a'] = '@parameter.inner',
      --   },
      --   swap_previous = {
      --     ['<leader>A'] = '@parameter.inner',
      --   },
      -- },
    },
  }
end, 0)

-- [[ Configure LSP ]]
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('<leader>cr', vim.lsp.buf.rename, 'Rename')
  nmap('<leader>ca', vim.lsp.buf.code_action, 'Code action')

  nmap('gd', require('telescope.builtin').lsp_definitions, 'Goto definition')
  nmap('<leader>cd', require('telescope.builtin').lsp_definitions, 'Goto definition')
  nmap('gr', require('telescope.builtin').lsp_references, 'Goto references')
  nmap('gI', require('telescope.builtin').lsp_implementations, 'Goto implementation')
  nmap('<leader>ci', require('telescope.builtin').lsp_implementations, 'Goto implementation')
  nmap('<leader>fD', require('telescope.builtin').lsp_type_definitions, 'Find definition')
  nmap('<leader>cd', require('telescope.builtin').lsp_document_symbols, 'Document symbols')
  nmap('<leader>cw', require('telescope.builtin').lsp_dynamic_workspace_symbols, 'workspace symbols')

  -- See `:help K` for why this keymap
  nmap('K', vim.lsp.buf.hover, 'Hover Documentation')
  nmap('<leader>ck', vim.lsp.buf.hover, 'Hover documentation')
  nmap('<C-k>', vim.lsp.buf.signature_help, 'Signature Documentation')
  nmap('<leader>cK', vim.lsp.buf.signature_help, 'Signature documentation')

  -- Lesser used LSP functionality
  nmap('gD', vim.lsp.buf.declaration, 'Goto declaration')
  -- nmap('<leader>wa', vim.lsp.buf.add_workspace_folder, '[W]orkspace [A]dd Folder')
  -- nmap('<leader>wr', vim.lsp.buf.remove_workspace_folder, '[W]orkspace [R]emove Folder')
  -- nmap('<leader>wl', function()
  --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  -- end, '[W]orkspace [L]ist Folders')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

-- document existing key chains
require('which-key').register {
  ['<leader>g'] = { name = ' Git', _ = 'which_key_ignore' },
  ['<leader>gh'] = { name = 'Hunk', _ = 'which_key_ignore' },
  ['<leader>gt'] = { name = 'Toggle', _ = 'which_key_ignore' },
  -- ['<leader>r'] = { name = '[R]ename', _ = 'which_key_ignore' }, -- TODO: Remove this from menu!
  ['<leader>u'] = { name = '󱘎 UndoTree', _ = 'which_key_ignore' },
  ['<leader>f'] = { name = ' Find', _ = 'which_key_ignore' },
  ['<leader>c'] = { name = ' Code', _ = 'which_key_ignore' },
  ['<leader>e'] = { name = '󰘧 Evaluate', _ = 'which_key_ignore' },
  ['<leader>l'] = { name = ' Eval Buffer', _ = 'which_key_ignore' },
}
-- register which-key VISUAL mode
-- required for visual <leader>hs (hunk stage) to work
require('which-key').register({
  ['<leader>'] = { name = 'VISUAL <leader>' },
  ['<leader>c'] = { name = ' Code', _ = 'which_key_ignore' },
  ['<leader>h'] = { 'Git [H]unk' },
}, { mode = 'v' })


---- [[ Keymaps ]] ----

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap movements
vim.keymap.set('n', "<C-h>", "<C-w>h", { desc = "Move to left split" })
vim.keymap.set('n', "<C-j>", "<C-w>j", { desc = "Move to below split" })
vim.keymap.set('n', "<C-k>", "<C-w>k", { desc = "Move to above split" })
vim.keymap.set('n', "<C-l>", "<C-w>l", { desc = "Move to right split" })

-- Remap resize
vim.keymap.set('n', "<C-Up>", "<cmd>resize -2<CR>", { desc = "Resize split up" })
vim.keymap.set('n', "<C-Down>", "<cmd>resize +2<CR>", { desc = "Resize split down" })
vim.keymap.set('n', "<C-Left>", "<cmd>vertical resize -2<CR>", { desc = "Resize split left" })
vim.keymap.set('n', "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Resize split right" })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })

-- Indentation (Stay in indent mode)
vim.keymap.set('v', '<S-Tab>', '<gv', { desc = 'Unindent line' })
vim.keymap.set('v', '<Tab>', '>gv', { desc = 'Indent line' })

-- Escape insert mode
vim.keymap.set('i', 'jj', '<Esc>', { desc = 'Escape insert mode' })
vim.keymap.set('i', 'jk', '<Esc>', { desc = 'Escape insert mode' })

-- UndoTree
vim.keymap.set('n', '<leader>uu', vim.cmd.UndotreeToggle, { desc = 'Toggle' })
vim.keymap.set('n', '<leader>uw', vim.cmd.UndotreePresistUndo, { desc = 'Write undo to file' })

-- Telescope
-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>ff', require('telescope.builtin').find_files, { desc = 'Find files' })
vim.keymap.set('n', '<leader>fF', require('telescope.builtin').live_grep, { desc = 'Find in files' })
vim.keymap.set('n', '<leader>fr', require('telescope.builtin').oldfiles, { desc = 'Find recently opened files' })
vim.keymap.set('n', '<leader>fb', require('telescope.builtin').buffers, { desc = 'Find buffers' })
vim.keymap.set('n', '<leader>f/',
  function()
    -- You can pass additional configuration to telescope to change theme, layout, etc.
    require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
      winblend = 10,
      previewer = false,
    })
  end, { desc = 'Fuzzy search in buffer' })
vim.keymap.set('n', '<leader>fo',
  function()
    require('telescope.builtin').live_grep {
      grep_open_files = true,
      prompt_title = 'Live Grep in Open Files',
    }
  end, { desc = 'Find in open buffers' })
vim.keymap.set('n', '<leader>fs', require('telescope.builtin').builtin, { desc = 'Find in selection' })
vim.keymap.set('n', '<leader>fg', require('telescope.builtin').git_files, { desc = 'Find in git project' })
vim.keymap.set('n', '<leader>fh', require('telescope.builtin').help_tags, { desc = 'Find in help' })
vim.keymap.set('n', '<leader>fw', require('telescope.builtin').grep_string, { desc = 'Find current word' })
vim.keymap.set('n', '<leader>fd', require('telescope.builtin').diagnostics, { desc = 'Find diagnostics' })
vim.keymap.set('n', '<leader>fR', require('telescope.builtin').resume, { desc = 'Find resume' })
vim.keymap.set('n', '<leader>ft', '<cmd>TodoTelescope keywords=TODO,FIX,WARNING,NOTE,HACK,PERF<cr>',
  { desc = 'Find TODOs, etc' })

-- Code
vim.keymap.set('v', '<leader>cc', '<esc><cmd>lua require("Comment.api").toggle.linewise(vim.fn.visualmode())<cr>',
  { desc = 'Toggle comment selection' })
vim.keymap.set('n', '<leader>cc',
  function() require("Comment.api").toggle.linewise.count(vim.v.count > 0 and vim.v.count or 1) end,
  { desc = 'Toggle comment line' })
vim.keymap.set('n', '<leader>cf', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>cq', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- Conjure
vim.g['conjure#mapping#eval_motion'] = 'eE'

-- NeoTree
vim.keymap.set('n', '<leader>o', '<cmd>Neotree toggle<cr>', { desc = 'Toggle File Tree' })


---- [[ Mason/LSP ]] ----

-- mason-lspconfig requires that these setup functions are called in this order
-- before setting up the servers.
require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = {
    'lua_ls',
    'rust_analyzer',
    'pyright',
    'clangd',
    'bashls',
    'neocmake',
    'gopls',
    'jsonls',
    'marksman',
    'taplo',
    'texlab',
    'arduino_language_server',
    'fennel_language_server',
  }
})
require("conform").setup({
  formatters_by_ft = {
    lua = { 'stylua', 'luacheck' },
    python = { 'isort', 'black' },
    bash = { 'shellcheck', 'shfmt' },
    go = { 'gomodifytags', 'gofumpt', 'iferr', 'impl', 'goimports' },
    markdown = { 'prettierd' },
    -- Use the "*" filetype to run formatters on all filetypes.
    ["*"] = { "codespell" },
    -- Use the "_" filetype to run formatters on filetypes that don't
    -- have other formatters configured.
    ["_"] = { "trim_whitespace" },
  },
  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 500,
    lsp_fallback = true,
  },
})

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
--
--  If you want to override the default filetypes that your language server will attach to you can
--  define the property 'filetypes' to the map in question.
local servers = {
  -- clangd = {},
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  -- tsserver = {},
  -- html = { filetypes = { 'html', 'twig', 'hbs'} },

  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      -- NOTE: toggle below to ignore Lua_LS's noisy `missing-fields` warnings
      -- diagnostics = { disable = { 'missing-fields' } },
    },
  },
}

-- Setup neovim lua configuration
require('neodev').setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
      filetypes = (servers[server_name] or {}).filetypes,
    }
  end,
}

-- [[ Configure nvim-cmp ]]
-- See `:help cmp`
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  completion = {
    completeopt = 'menu,menuone,noinsert',
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'path' },
  },
}

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
