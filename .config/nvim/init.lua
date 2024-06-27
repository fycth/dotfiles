vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
-- line numbers
vim.wo.number = true
-- Relative line numbers
vim.wo.relativenumber = true
-- Share clipboard between OS and Neovim
vim.o.clipboard = 'unnamedplus'

-- Needed to use 2 spaces width for tabs
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.bo.softtabstop = 2

-- Needed for some teams for some reason
vim.o.termguicolors = true

-- Configure lazy.nvim package manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local packages = {
  { "ellisonleao/gruvbox.nvim" },
  { "onsails/lspkind.nvim" },
  -- Capuccin theme: https://github.com/catppuccin/nvim
  { "catppuccin/nvim",                  name = "catppuccin-latte", priority = 1000 },
  -- Other themes
  { "savq/melange-nvim" },
  { "aktersnurra/no-clown-fiesta.nvim" },
  { "Shatur/neovim-ayu" },
  { "felipeagc/fleet-theme-nvim" },
  -- Manson stuff
  { "williamboman/mason.nvim" },
  { "williamboman/mason-lspconfig.nvim" },
  -- Auto pairs plugin
  { "echasnovski/mini.nvim",            version = false },
  -- Telescope stuff
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.4',
    -- or                              , branch = '0.1.x',
    dependencies = { 'nvim-lua/plenary.nvim' }
  },
  {
    'zbirenbaum/copilot.lua',
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      require("copilot").setup({})
    end
  },
  {
    "zbirenbaum/copilot-cmp",
    config = function()
      require("copilot_cmp").setup()
    end
  },
  -- "gc" to comment visual regions/lines
  { 'numToStr/Comment.nvim',     opts = {} },

  -- LSP & Autocomplete using lsp-zero: https://github.com/VonHeikemen/lsp-zero.nvim
  { 'VonHeikemen/lsp-zero.nvim', branch = 'v3.x' },
  { 'neovim/nvim-lspconfig' },
  { 'hrsh7th/cmp-nvim-lsp' },
  { 'hrsh7th/nvim-cmp' },
  { 'L3MON4D3/LuaSnip' }

}

-- Initiating all the packages
require("lazy").setup(packages, {})
require('ayu').setup({
  overrides = {
    Normal = { bg = "None" },
    ColorColumn = { bg = "None" },
    SignColumn = { bg = "None" },
    Folded = { bg = "None" },
    FoldColumn = { bg = "None" },
    CursorLine = { bg = "None" },
    CursorColumn = { bg = "None" },
    WhichKeyFloat = { bg = "None" },
    VertSplit = { bg = "None" },
  },
})
-- Setting the theme.
-- vim.cmd.colorscheme "catppuccin"
-- vim.cmd.colorscheme 'melange'
-- vim.cmd.colorscheme "no-clown-fiesta"
vim.cmd.colorscheme "gruvbox"
-- vim.cmd.colorscheme 'fleet'

vim.api.nvim_set_hl(0, 'LineNrAbove', { fg = 'yellow' })
vim.api.nvim_set_hl(0, 'LineNr', { fg = 'yellow' })
vim.api.nvim_set_hl(0, 'LineNrBelow', { fg = 'yellow' })
-- Setting up lsp-zero and its default keybindings
local lsp_zero = require('lsp-zero')
lsp_zero.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp_zero.default_keymaps({ buffer = bufnr })
end)

-- Automatic setup of LSP servers based on Mason & LSP Zero
require('mason').setup({})
require('mason-lspconfig').setup({
  ensure_installed = { 'tsserver' },
  handlers = {
    lsp_zero.default_setup,
    tsserver = function()
      require('lspconfig').tsserver.setup({})
    end,
  },
})


local cmp = require('cmp')
local lspkind = require('lspkind')

-- Configuring keybindings for LSP suggestions
cmp.setup({
  preselect = 'item',
  mapping = cmp.mapping.preset.insert({
    -- Using Enter to select instead of CTRL + y
    ['<CR>'] = cmp.mapping.confirm({ select = false }),
    -- Invoke completion menu manually
--    ['<C-Space>'] = cmp.mapping.complete(),
    ['<f12>'] = cmp.mapping.complete(),

  }),
  sources = {
    { name = 'copilot', group_index = 2},
    { name = 'nvim_lsp', group_index = 2},
  },
  completion = {
    autocomplete = false,
    -- Preselect first item from LSP
    completeopt = 'menu,menuone,noinsert'
  },
  formatting = {
    format = lspkind.cmp_format({
      mode = "symbol",
      max_width = 50,
      symbol_map = { Copilot = "" }
    })
  }
})

-- configure telescope stuff
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>,', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

-- Mapping 'd' to delete without saving to clipboard
vim.keymap.set('n', 'd', '"_d', { noremap = true })
vim.keymap.set('v', 'd', '"_d', { noremap = true })

-- Mapping 'c' to change without saving to clipboard
vim.keymap.set('n', 'c', '"_c', { noremap = true })
vim.keymap.set('v', 'c', '"_c', { noremap = true })


-- Move to window using the <ctrl> hjkl keys
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Go to left window", remap = true })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Go to lower window", remap = true })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Go to upper window", remap = true })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Go to right window", remap = true })

-- Resize window using <ctrl> arrow keys
vim.keymap.set("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
vim.keymap.set("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
vim.keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
vim.keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

-- better indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")


-- Turns background transparent
vim.cmd [[hi Normal guibg=NONE ctermbg=NONE]]

-- Enable auto pair plugin
require('mini.pairs').setup()

require("copilot").setup({
  suggestion = { enabled = false },
  panel = { enabled = false },
})

