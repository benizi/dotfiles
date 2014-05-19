function! BufferList()
	let l:maxb = bufnr("$")
	let l:i = 1
	let l:ret = []
	while l:i <= l:maxb
		if bufexists(l:i) && buflisted(l:i)
			let l:name = bufname(l:i)
			if strlen(l:name)
				let l:ret += [ l:name ]
			endif
		endif
		let l:i += 1
	endwhile
	return l:ret
endfunction

function! WindowList()
	let l:maxw = winnr("$")
	let l:i = 1
	let l:ret = []
	while l:i <= l:maxw
		let l:wb = winbufnr(l:i)
		if buflisted(l:wb)
			let l:name = bufname(l:wb)
			if strlen(l:name)
				let l:ret += [ l:name ]
			endif
		endif
		let l:i += 1
	endwhile
	return l:ret
endfunction

function! WindowsAndBuffers()
	let l:all = WindowList()
	let l:all += BufferList()
	let l:ret = []
	let l:seen = {}
	for b in l:all
		if !has_key(l:seen,b)
			let l:seen[b] = 1
			let l:ret += [ b ]
		endif
	endfor
	return l:ret
endfunction

function! s:TestBuffers()
	let s:other = 3
	let s:returnto = bufnr("%")
	exe "buf" s:other
	echo &l:modifiable
	exe "buf" s:returnto
endfunction
