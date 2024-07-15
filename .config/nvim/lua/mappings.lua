require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

map("n", ";", ":", { desc = "CMD enter command mode" })
map("i", "jk", "<ESC>")

-- map({ "n", "i", "v" }, "<C-s>", "<cmd> w <cr>")

-- Disable mappings
local nomap = vim.keymap.del

nomap("n", "<tab>")
nomap("n", "<S-tab>")

map("i", "<C-a>", "<ESC>^i", { desc = "move beginning of line" })
map("i", "<C-e>", "<End>", { desc = "move end of line" })

map("n", ".", function()
  require("nvchad.tabufline").next()
end, { desc = "buffer goto next" })

map("n", ",", function()
  require("nvchad.tabufline").prev()
end, { desc = "buffer goto prev" })


