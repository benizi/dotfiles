setlocal fdm=syntax
let s:i = match(getline(1,'$'), '<?php')
if s:i >= 0 && line('$') > 1
	let lines = filter(getline(s:i+1,min([line('$'),s:i+20])),'v:val =~ "^\\s*\\S"')
	let leads = map(lines,'strlen(matchstr(v:val,"^\\s*"))')[0:1]
	let g:PHP_default_indenting = (leads[1] > leads[0]) ? 1 : 0
else
	let g:PHP_default_indenting = 0
endif
let g:PHP_vintage_case_default_indent = 1
let php_folding = 2
au Syntax php syn case match | syn keyword phpTodo DEBUG contained | syn case ignore
setlocal makeprg=php\ \%:p
setl efm=''
fun! MyFoldText()
   return '+'.v:folddashes.' '.(1 + v:foldend - v:foldstart).' lines ['.v:foldstart.'-'.v:foldend.']: '.substitute(foldtext(),'^[+\-]*\s*\d*\s*lines:\s*','','')
endfun
setl fdt=MyFoldText()
" highlight very long lines (> the default 3000 chars)
setl smc=0
nmap <LocalLeader>p :let @/='\%(\*\/\s*\)\@<!\n\zs\s*\%(public\<bar>private\<bar>protected\<bar>static\<bar>final\<bar>var\<bar>abstract\)' <bar> norm nzv<CR>
