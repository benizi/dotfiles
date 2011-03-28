setlocal fdm=syntax
let s:i = match(getline(1,'$'), '<?php')
if s:i >= 0 && line('$') > 1
	let lines = filter(getline(s:i+1,min([line('$'),s:i+20])),'v:val =~ "^\\s*\\S"')
	let leads = map(lines,'strlen(matchstr(v:val,"^\\s*"))')[0:1]
	let g:PHP_default_indenting = (leads[1] > leads[0]) ? 1 : 0
else
	let g:PHP_default_indenting = 1
endif
let g:PHP_vintage_case_default_indent = 1
let php_folding = 2
au Syntax php syn case match | syn keyword phpTodo DEBUG contained | syn case ignore
