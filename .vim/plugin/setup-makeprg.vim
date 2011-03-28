if exists('g:loaded_setup_makeprg')
	finish
endif

let s:override_c = { 'makeprg': 'gcc %:p -o %:p:r && %:p:r', 'prefix': 0 }
let s:override_cpp = { 'makeprg': 'g++ %:p -o %:p:r && %:p:r', 'prefix': 0 }
let s:override_pl = { 'makeprg': 'perl' }

fun! s:FindFromModeline(...)
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
	if a:0 > 0
		let newprog = input("Make program: ", &l:makeprg, 'shellcmd')
		if strlen(newprog)
			let &l:makeprg = newprog
			return
		endif
		let yn = confirm('Reset to default?', "&Yes\n&No", 2)
		if yn != 1
			return
		endif
	endif
	let found = s:FindFromModeline()
	if len(found)
		let prog = found[0]
		let prefix = get(found,1,0)
	else
		if exists('g:override_makeprg_{&ft}')
			let from = g:override_makeprg_{&ft}
		elseif exists('s:override_{&ft}')
			let from = s:override_{&ft}
		endif
		if !exists('l:from') " && exists('*service#Exists')
			let fn = expand('%:p')
			let svcs = []
			for pat in ['^/etc/\([^/]\+\)/', '\([^/]\+\)\.conf']
				let m = matchlist(fn, pat)
				if !len(m)
					continue
				endif
				let svc = m[1]
				if strlen(service#Exists(svc))
					let from = {}
					let from.makeprg = 'call service#Do("'.svc.'","reload")'
					let from.prefix = 0
					let b:makeprg_is_cmd = 1
					break
				endif
			endfor
		endif
		if !exists('l:from')
			let from = { 'makeprg': &ft, 'prefix': 1 }
		endif
		let prog = get(from, 'makeprg', &ft)
		let prefix = get(from, 'prefix', 1)
	endif
	let &l:makeprg = prog
	if prefix
		let &l:makeprg .= ' %:p'
	endif
endfun

fun! RunMake()
	if !strlen(&l:makeprg)
		return
	endif
	if exists('b:makeprg_is_cmd')
		exe &l:makeprg
		return
	endif
	make
endfun

aug SetupMakePrg
	au!
	au FileType * call SetupMakePrg()
aug END

map <F10> :call SetupMakePrg(1) \| :call RunMake()<CR>
map <F11> :call RunMake()<CR>
map <F12> :call SetupMakePrg(1)<CR>
