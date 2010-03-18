function cscope#Init()
	if has("cscope")
		let s:fname = expand('%:p')
		let s:a = 1
		while s:fname =~ '/'
			let s:cscope_name = s:fname
			if s:fname == '/'
				let s:a = 1000
			else
				let s:cscope_name .= '/'
			endif
			let s:cscope_name .= 'cscope.out'
			if filereadable(s:cscope_name)
				exec "sil! cs add ".s:cscope_name." ".s:fname
			endif
			let s:fname = fnamemodify(s:fname,':h')
			let s:a += 1
			if s:a > 100
				break
			endif
		endwhile
		if $CSCOPE_DB != ""
			sil! cs add $CSCOPE_DB
		endif
	endif
endfunction
