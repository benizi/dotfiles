set csto=0
set cst
set nocsverb
set csverb
autocmd BufRead * :call cscope#Init()

function! cscope#Help()
	echo '' | echohl ModeMsg | echo '(s)ymbol,(g)definition,calle(d)-by,(c)alling,(t)ext,(e)grep,(f)ile,(i)ncluding' | echohl None
endfunction

fun! cscope#Maps()
	nmap <Leader>? :call cscope#Help()<CR>
	nmap <Leader>s :cs find s <C-R>=expand("<cword>")<CR><CR>
	nmap <Leader>g :cs find g <C-R>=expand("<cword>")<CR><CR>
	nmap <Leader>c :cs find c <C-R>=expand("<cword>")<CR><CR>
	nmap <Leader>t :cs find t <C-R>=expand("<cword>")<CR><CR>
	nmap <Leader>e :cs find e <C-R>=expand("<cword>")<CR><CR>
	nmap <Leader>f :cs find f <C-R>=expand("<cword>")<CR><CR>
	nmap <Leader>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
	nmap <Leader>d :cs find d <C-R>=expand("<cword>")<CR><CR>
endfun

" call cscope#Maps()
