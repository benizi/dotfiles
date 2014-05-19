set fo+=w
function! ExitIfTop(else)
	if(line('.') == 1)
		return "ZZ"
	endif
	return a:else
endfunction
"nnoremap <expr> k ExitIfTop('k')
nnoremap <expr> <C-p> ExitIfTop('<C-p>')
set spell
let g:application_name='alpine'
