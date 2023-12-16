--[[

=====================================================================
======================== TREE-SITTER CONFIG =========================
=====================================================================

--]]

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
       'c',
       'cpp',
       'go',
       'lua',
       'python',
       'rust',
       'ruby',
       'perl',
       'julia',
       'tsx',
       'php',
       'commonlisp',
       'java',
       'javascript',
       'typescript',
       'r',
       'ocaml',
       'sql',
       'zig',
       'bash',
       'latex',
       'ledger',
       'vim',
       'vimdoc',
       'llvm',
       'nix',
       'regex',

       -- File Types
       'html',
       'http',
       'xml',
       'css',
       'csv',
       'tsv',
       'psv',
       'json',
       'json5',
       'yaml',
       'toml',
       'gitignore',
       'ssh_config',
       'org',
       'make',
       'cmake',
       'meson',
       'ninja',
       'dockerfile',
       'devicetree',
       'kconfig',
    },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    auto_install = true,

    highlight = {
      enable = true,
      additional_vim_regex_highlighting = { 'org' },
    },

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
      swap = {
        enable = true,
        swap_next = {
          ['<leader>a'] = '@parameter.inner',
        },
        swap_previous = {
          ['<leader>A'] = '@parameter.inner',
        },
      },
    },
  }
end, 0)
