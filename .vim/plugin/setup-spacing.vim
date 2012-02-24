if exists('g:loaded_setup_spacing')
	finish
endif
let g:loaded_setup_spacing = 1

fun! ShowTabsettings()
	verbose set ts? sts? sw? et? listchars?
endfun

fun! SetupTabstop(width, expand, ...)
	let &l:ts = a:width
	let &l:sts = a:0 > 2 ? a:3 : a:width
	let &l:sw = a:width
	let &l:et = a:expand ? 1 : 0
	" if not mixed tabs+spaces, hilight as errors
	if (! &l:sts) || (&l:sts == &l:ts)
		call matchadd('Error', '^\t\+\ ')
		call matchadd('Error', '^\ \+\t')
		hi Error cterm=reverse
	endif
endfun

fun! SetInModeline(var)
	redir => last_set
	exe "silent! verbose set ".a:var."?"
	redir END
	return -1 == match(last_set, 'from modeline') ? 0 : 1
endfun

fun! DetectSpacing()
	let expand_tabs = exists('g:spacing_expand_tabs') ? g:spacing_expand_tabs : 0
	let default_spacing = exists('g:spacing_default') ? g:spacing_default : 4
	let max_lines = exists('g:spacing_max_lines') ? g:spacing_max_lines : 100
	let max_consider = exists('g:spacing_max_spaces') ? g:spacing_max_spaces : 8

	let ret = []
	if SetInModeline('ts')
		call add(ret, &l:ts)
		call add(ret, SetInModeline('et') ? &l:et : expand_tabs)
		if &l:ts != &l:sts && SetInModeline('sts')
			call add(ret, &l:sts)
		endif
		return ret
	endif

	let okay = map(range(max_consider+1), 0)
	let chars_scanned = 0
	let lines_scanned = 0
	for lnum in range(0,max_lines)
		let line = getline(lnum)
		let chars_scanned += len(line)
		let lines_scanned += 1

		let ws = matchlist(line, '^\(\s*\)')
		if len(ws)
			if len(matchlist(line, '^\t'))
				let okay[0] += 1
			elseif len(ws[1])
				for n in range(1,max_consider)
					let okay[n] += (len(ws[1]) % n) ? 0 : 1
				endfor
			endif
		endif

		if chars_scanned > max_lines * 100
			break
		endif
	endfor
	let mx = max(okay)
	let mxi = max(map(copy(okay), 'v:val == mx ? v:key : 0'))
	if mx < lines_scanned / 4 " can't tell
		return []
	endif
	if okay[0] > mx / 2 " mostly indented with tabs
		return [ default_spacing, 0 ]
	endif
	return []
endfun

fun! SetupSpacingAutocmd()
	aug SetupSpacing
		au!
		au BufWinEnter * call SetupSpacing()
	aug END
endfun

fun! SetupSpacing(...)
	if exists('b:setup_spacing') && !a:0
		return
	endif
	let b:setup_spacing = 1
	if a:0 > 0
		let homestyle = (a:1 ? 1 : 0)
	else
		let detected = DetectSpacing()
		if len(detected)
			call call('SetupTabstop', detected)
			return
		endif
		if !exists('g:disable_detectindent')
			return
		endif
		let homestyle = -1 != match(expand('<afile>:p'),expand('~/bin/'))
	endif
	let b:homestyle_spacing = homestyle
	if homestyle
		call SetupTabstop(4,0)
		set listchars=tab:\ \ ,trail:·
	else
		call SetupTabstop(3,1)
		set listchars=tab:»·,trail:·
	endif
endfun
