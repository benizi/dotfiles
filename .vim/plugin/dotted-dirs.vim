function! DottedLCD(filename)
	" Get the arglist value if present, otherwise the passed-in <afile>
	let i = argidx()
	let name = i < 0 ? a:filename : argv(i)

	" ignore netrw filenames
	if name =~ '^[a-z]\+:'
		return
	endif

	" find the '/./' that relativizes the path
	let dot = stridx(name, '/./')
	if dot < 0
		return
	end

	" take everything up to the relativizer
	let dir = strpart(name, 0, dot)
	exe ":lcd ".dir
endfun

aug DottedLCD
	au BufReadPost * call DottedLCD(expand("<afile>"))
aug END
