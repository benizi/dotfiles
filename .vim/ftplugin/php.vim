fun! PHPGetHelp(...)
	let fn = a:0 ? a:1 : expand('<cword>')
	let cmd = '!xdg-open '.shellescape('http://php.net/'.fn)
	silent! exe cmd
endfun
nore K :call PHPGetHelp()<CR>
