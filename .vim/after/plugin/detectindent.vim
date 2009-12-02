if exists("loaded_detectindent") && !exists("disable_detectindent")
let g:detectindent_preferred_expandtab = 0
let g:detectindent_preferred_indent = 4
"let g:debug_tab_detection = 1
autocmd BufReadPost *html let g:detectindent_preferred_indent = 1
autocmd BufReadPost * :DetectIndent
endif
