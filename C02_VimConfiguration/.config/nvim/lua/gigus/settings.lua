--[[

=====================================================================
============================== OPTIONS ==============================
=====================================================================

--]]

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = false

-- Set incremental search
vim.o.incsearch = true

-- Make line numbers default
vim.wo.number = true

-- Make relative line numbers default
vim.wo.relativenumber = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Indenting and Tabs
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true

vim.o.smartindent = true

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Linewrap
vim.o.wrap = true

-- Save undo history
vim.oswapfile = false
vim.o.backupdir = os.getenv("HOME") .. "/.dotfiles/C02_VimConfiguration/backup"
vim.o.backup = false
vim.o.undodir = os.getenv("HOME") .. "/.dotfiles/C02_VimConfiguration/undodir"
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Stay focused on cursor, never go within 8 lines of borders
vim.o.scrolloff = 8

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Set colorcolumn - 90 chars
vim.o.colorcolumn = "90"

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true
