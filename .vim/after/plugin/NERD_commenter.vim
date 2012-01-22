function s:GetNERDCommentSNR()
	if !exists("s:snr")
		redir => s:scriptnames
			silent scriptnames
		redir END
		let s:scripts = reverse(split(s:scriptnames,"\n"))
		let s:snr = len(s:scripts) - match(s:scripts,'\(\d\+\):.*NERD_commenter\.vim')
	endif
endfunction

function s:FixNERDComment(ft)
	call s:GetNERDCommentSNR()
	if a:ft == 'sql'
		let l:delim="'/*','*/'"
	endif
	exe "call <SNR>".s:snr."_MapDelimiters(".l:delim.")"
endfunction

aug SetupComment
	au!
	au FileType sql call s:FixNERDComment(expand("<amatch>"))
aug END
