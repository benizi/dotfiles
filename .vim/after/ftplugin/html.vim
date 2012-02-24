fun! s:FindBase()
	for line in getline(0,'$')
		let m = matchlist(line, '<base[^>]*href=\([''"]\)\([^''"]*\)\1')
		if len(m)
			return m[2]
		endif
	endfor
	let m = matchlist(expand('%'), '\(https\?://[^/]\+\)')
	if len(m)
		return m[1]
	endif
	return substitute(expand('%'), '/[^/]*$', '', '')
endfun

fun! ExpandHtmlInclude()
	let b = s:FindBase()
	return findfile(substitute(v:fname,'^/','',''),'.;')
endfun

setl inex=<Plug>InEx()
fun! A()
	echom 'base=' s:FindBase()
endfun
