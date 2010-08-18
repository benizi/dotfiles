if exists('g:loaded_setup_makeprg')
	finish
endif

fun! SetupMakePrg(...)
	let prefix = 1
	if &ft == 'c' || &ft == 'cpp'
		let prog = 'gcc %:p -o %:p:r && %:p:r'
		let prefix = 0
	elseif &ft == 'pl'
		let prog = 'perl'
	else
		let prog = &ft
	endif
	let &l:makeprg = prog
	if prefix
		let &l:makeprg .= ' %:p'
	endif
	if a:0 > 0
		let &l:makeprg = input("Make program: ", &l:makeprg, 'shellcmd')
	endif
endfun

aug SetupMakePrg
	au!
	au FileType * call SetupMakePrg()
aug END

map <F10> :call SetupMakePrg(1)<CR>
map <F11> :make<CR>
map <F12> :call SetupMakePrg(1) \| :make<CR>
