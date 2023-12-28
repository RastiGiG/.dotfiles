--[[

=====================================================================
============================ KEYBINDINGS ============================
=====================================================================

--]]

-- [[ Leader Keys ]]
local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.keymap.set

-- Set <space> as the leader key
-- See `:help mapleader`
-- NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- [[ Modes ]]
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- [[ Normal Mode ]]
-- Better window navigation
-- keymap("n", "<C-h>", "<C-w>h", opts)
-- keymap("n", "<C-j>", "<C-w>j", opts)
-- keymap("n", "<C-k>", "<C-w>k", opts)
-- keymap("n", "<C-l>", "<C-w>l", opts)

-- Resize with arrows
keymap("n", "<C-Up>", ":resize -2<CR>", opts)
keymap("n", "<C-Down>", ":resize +2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)

-- Move text up and down
keymap("n", "<A-j>", ":m .+1<CR>==", opts)
keymap("n", "<A-k>", ":m .-2<CR>==", opts)

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
keymap({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
keymap('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
keymap('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
keymap('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
keymap('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

-- [[ Insert ]]
-- Press jk fast to exit insert mode 
-- Add easier way to exit insert
keymap("i", "jk", "<ESC>", opts)
keymap("i", "kj", "<ESC>", opts)

-- [[ Visual ]]
-- Stay in indent mode
keymap("v", "<", "<gv^", opts)
keymap("v", ">", ">gv^", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m '>+1<CR>gv=gv", opts)
keymap("v", "<A-k>", ":m '<-2<CR>gv=gv", opts)
keymap("v", "p", '"_dP', opts)

-- [[ Visual Block ]]
-- Move text up and down
keymap("x", "J", ":m '>+1<CR>gv=gv", opts)
keymap("x", "K", ":m '<-2<CR>gv=gv", opts)
keymap("x", "<A-j>", ":m '>+1<CR>gv=gv", opts)
keymap("x", "<A-k>", ":m '<-2<CR>gv=gv", opts)

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

-- [[ Project keybindings ]]

keymap("n", "<leader>pv", vim.cmd.Ex, { desc = 'Open Explorer ([P]roject [V]iew)' })


-- [[ QoL Keymaps ]]

-- Remap for dealing with word wrap
keymap('v', 'J', ":m '>+1<CR>gv=gv")
keymap('v', 'K', ":m '<-2<CR>gv=gv")

-- Center cursor when jumping half pages or searh terms
keymap('n', '<C-d>', "<C-d>zz")
keymap('n', '<C-u>', "<C-u>zz")
keymap('n', 'n', "nzzzv")
keymap('n', 'N', "Nzzzv")

-- Quickfix improvements: ThePrimeagen
keymap('n', '<C-j>', "<cmd>cprev<CR>zz")
keymap('n', '<C-k>', "<cmd>cnext<CR>zz")
keymap('n', '<leader>j', "<cmd>lprev<CR>zz")
keymap('n', '<leader>k', "<cmd>lnext<CR>zz")

-- Don't overwrite current register: ThePrimeagen
keymap('x', '<leader>vp', "\"_dP")

-- Some more void register stuff: The Primeagen
keymap('n', '<leader>vd', "\"_d")
keymap('v', '<leader>vd', "\"_d")

-- Separate system clipboard: asbjornHaland
keymap('n', '<leader>y', "\"+y")
keymap('v', '<leader>y', "\"+y")
keymap('n', '<leader>Y', "\"+Y")

-- Shorthand for search and replace: The Primeagen
keymap('n', '<leader>fr', [[:%s/\\<<C-r><C-w>>\\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "[F]ind and [R]eplace" })
