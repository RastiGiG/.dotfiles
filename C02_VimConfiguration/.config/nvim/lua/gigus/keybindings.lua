--[[

=====================================================================
============================ KEYBINDINGS ============================
=====================================================================

--]]

-- [[ Leader Keys ]]

-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- [[ Basic Keymaps ]]

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })

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

vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, { desc = 'Open Explorer ([P]roject [V]iew)' })


-- [[ QoL Keymaps ]]

-- Remap for dealing with word wrap
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv")

-- Center cursor when jumping half pages or searh terms
vim.keymap.set('n', '<C-d>', "<C-d>zz")
vim.keymap.set('n', '<C-u>', "<C-u>zz")
vim.keymap.set('n', 'n', "nzzzv")
vim.keymap.set('n', 'N', "Nzzzv")

-- Quickfix improvements: ThePrimeagen
vim.keymap.set('n', '<C-j>', "<cmd>cprev<CR>zz")
vim.keymap.set('n', '<C-k>', "<cmd>cnext<CR>zz")
vim.keymap.set('n', '<leader>j', "<cmd>lprev<CR>zz")
vim.keymap.set('n', '<leader>k', "<cmd>lnext<CR>zz")

-- Don't overwrite current register: ThePrimeagen
vim.keymap.set('x', '<leader>vp', "\"_dP")

-- Some more void register stuff: The Primeagen
vim.keymap.set('n', '<leader>vd', "\"_d")
vim.keymap.set('v', '<leader>vd', "\"_d")


-- Separate system clipboard: asbjornHaland
vim.keymap.set('n', '<leader>y', "\"+y")
vim.keymap.set('v', '<leader>y', "\"+y")
vim.keymap.set('n', '<leader>Y', "\"+Y")

-- Shorthand for search and replace: The Primeagen
vim.keymap.set('n', '<leader>fr', [[:%s/\\<<C-r><C-w>>\\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "[F]ind and [R]eplace" })
