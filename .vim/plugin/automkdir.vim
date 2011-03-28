function! automkdir#MakeCurrent()
	call automkdir#MakeDirIfNotExists(expand("%:p:h"))
endfunction
function! automkdir#MakeDirIfNotExists(d)
	if a:d =~ '^[a-z]\+:/'
		return
	endif
	if !isdirectory(a:d)
		call system("mkdir -p ".shellescape(a:d))
		echo "Created directory: ".a:d
	endif
endfunction
