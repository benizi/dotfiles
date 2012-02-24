if !executable('sqlite3')
	finish
endif

fun! s:Run(cmd)
	let escaped = substitute(a:cmd, '%', '\\%', 'g')
	exe "silent! !".escaped
endfun

fun! s:AsRoot()
	if !exists('s:as_root')
		let s:as_root = getftype(expand('~/.vim')) != 'dir'
	endif
	return s:as_root
endfun

fun! s:Chown(file, ...)
	let ref = expand(a:0 ? a:1 : '~/.vim')
	let chown = '!chown --reference '.shellescape(ref).' '.shellescape(a:file)
	silent! exe chown
endfun

fun! DefaultFilename(...)
	if !exists('s:default_filename') || (a:0 && a:1)
		let dir = s:AsRoot() ? '~/.vim/../.vim.local' : '~/.vim.local'
		if isdirectory(expand(dir)) < 1 && exists('*mkdir')
			call mkdir(expand(dir), 'p', 0700)
			call s:Chown(dir)
		endif
		let s:default_filename = expand(dir).'/'.(s:AsRoot() ? $USER.'.' : '').'timetrack.sqlite3'
	endif
	return s:default_filename
endfun

fun! CreateDB(...)
	let fn = a:0 ? a:1 : DefaultFilename()
	if !filereadable(fn)
		call s:Run('sqlite3 '.fnameescape(fn).' '.shellescape('create table edited_files (time integer, filename text)'))
		call s:Chown(fn)
	endif
	return filereadable(fn)
endfun

fun! s:DoSQL(sql, ...)
	if !CreateDB()
		call s:LogDisable()
		return
	endif
	let cmd='sqlite3 '.fnameescape(DefaultFilename()).' '.shellescape(a:sql)
	" echomsg 'sql='.a:sql
	" echomsg 'cmd='.cmd
	silent! call s:Run(cmd)
endfun

fun! LogFile(fn)
	call s:DoSQL('insert into edited_files (time, filename) values (strftime("%s","now"),"'.fnameescape(a:fn).'")')
endfun

let s:last_tracked = {}
fun! DelayLogCurrentFile(...)
	let delay = a:0 ? a:1 : 3 * 60
	let fn = expand('%:p')
	if !strlen(fn) || !&swapfile
		return
	endif
	let now = localtime()
	if now - get(s:last_tracked, fn, 0) > delay
		call LogFile(fn)
		let s:last_tracked[fn] = now
	endif
endfun

fun! s:LogEnable()
	aug TimeTracking
		au!
		au CursorMoved * call DelayLogCurrentFile()
	aug END
endfun

fun! s:LogDisable()
	aug TimeTracking
		au!
	aug END
endfun

call s:LogEnable()
