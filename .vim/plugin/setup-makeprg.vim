if exists('g:loaded_setup_makeprg')
	finish
endif

fun! s:FindSetupMakePrg(...)
	let maxsearch = a:0 ? a:1 : 10
	let mx = line('$')
	if mx
		let lines = getline(1, min([mx, maxsearch]))
		if mx > maxsearch
			call extend(lines, reverse(getline(max([maxsearch, mx-maxsearch]), mx)))
		endif
		for line in lines
			let m = matchlist(line, 'makeprg=\(.*\)$')
			if len(m)
				return [m[1]]
			endif
		endfor
	endif
	return []
endfun

fun! SetupMakePrg(...)
	let found = s:FindSetupMakePrg()
	if len(found)
		let prog = found[0]
		let prefix = 0
	else
		let prefix = 1
		if &ft == 'c' || &ft == 'cpp'
			let prog = 'gcc %:p -o %:p:r && %:p:r'
			let prefix = 0
		elseif &ft == 'pl'
			let prog = 'perl'
		else
			let prog = &ft
		endif
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
