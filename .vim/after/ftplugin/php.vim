setlocal fdm=syntax
let g:PHP_default_indenting = 1
let g:PHP_vintage_case_default_indent = 1
let php_folding = 2
au Syntax php syn case match | syn keyword phpTodo DEBUG contained | syn case ignore
