local options = {
	cmdheight = 2,
	fileencoding = "utf-8",
	mouse = "a",
	wrap = true,
	number = true,
}

vim.opt.shortmess:append "c"

for k, v in pairs(options) do
	vim.opt[k] = v
end
