function! DottedLCD(filename)
	" ignore netrw filenames
	if a:filename =~ '^[a-z]\+:'
		return
	endif
	let dir=substitute(a:filename,'\(.*\)/./.*$','\1','')
	if dir != a:filename
		exe ":lcd ".dir
	endif
endfun

aug DottedLCD
	au BufReadPost * call DottedLCD(expand("<afile>"))
aug END
