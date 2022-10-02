local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

-- Normal --
-- Press control and g to escape
keymap("n", "<C-g>", "<ESC>", opts)

keymap("n", "<C-x><C-s>", ":w<CR>", opts)
keymap("n", "<C-q><C-q>", ":q<CR>", opts)

-- Move text up and down
keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-- Insert --
-- Press control and g to exit insert mode 
keymap("i", "<C-g>", "<ESC>", opts)

-- Visual --
-- Press control and g to exit visual mode 
keymap("v", "<C-g>", "<ESC>", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Press control and g to exit visual block mode 
keymap("x", "<C-g>", "<ESC>", opts)

-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Command --
-- Press control and g to escape command
-- keymap("c", "<C-g>", "<ESC>", opts)
-- keymap("t", "<C-g>", "<ESC>", opts)


