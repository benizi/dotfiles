function! DottedLCD(filename)
	let l:dir=substitute(a:filename,'\(.*\)/./.*$','\1','')
	if l:dir != a:filename
		exe ":lcd ".l:dir
	endif
endfun

aug DottedLCD
	au BufReadPost * call DottedLCD(expand("<afile>"))
aug END
