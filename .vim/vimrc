" different locations if running as root
let owner_home = '~'
let s:script = shellescape(expand('~/.vimrc'))
if executable('stat') && system('stat -c %F '.s:script) =~ 'link'
	let owner_home .= split(system('stat -L -c %U '.s:script))[0]
endif
let s:home = owner_home
let s:home_vim = s:home.'/.vim'

" Get directory under the set of 'bundled' files
fun! s:BundleDir(...)
	return join(extend([s:home_vim.'/bundle'],a:000),'/')
endfun

" Add various directories to &rtp, with their '/after' dirs
let s:dirs = [ s:home.'/.vim', s:home.'/.vim.local' ]
\ + map(['vim-addon-manager','pathogen'],'s:BundleDir(v:val)')
for dir in map(s:dirs, 'expand(v:val)')
	if isdirectory(dir)
		if index(split(&rtp,','), dir) < 0
			let &rtp = join([ dir, &rtp, dir.'/after' ], ',')
		endif
	endif
endfor

" Set leader chars before activating addons
let g:mapleader = ','
let g:maplocalleader = g:mapleader

fun! InGUI()
	return has('gui') && has('gui_running')
endf

" keep some plugins around w/o loading by default...
let g:pathogen_disabled = []
call add(g:pathogen_disabled, 'CSApprox')

" st and konsole support 24-bit color
if $TERM =~ 'st-256color' || exists('$KONSOLE_DBUS_SERVICE')
	se t_Co=1000
end

try
	call vam#ActivateAddons() " set up VAM functions
	call pathogen#infect(s:BundleDir()) " activate everything
catch
	echomsg 'Caught exception:'
	echomsg v:exception
	echomsg 'Perhaps pathogen or vim-addon-manager is not installed?'
endtry

let s:ng = expand(s:home.'/hg/vimclojure/client/ng')
if executable(s:ng)
	let vimclojure#WantNailgun = 1
	let vimclojure#NailgunClient = s:ng
endif

let g:Powerline_symbols = 'unicode'
let g:Powerline_cache_enabled = 0

set noexpandtab softtabstop=4 tabstop=4 shiftwidth=4
set list listchars=tab:\ \ ,trail:·
se nowrap cc=80
if $TERM =~ 'rxvt' || $TERM =~ 'xterm'
	set mouse=a
endif
if exists('$VIM_BACK')
	exec "set background=" . $VIM_BACK
else
	if &term == 'cygwin' || &term == 'linux'
		set background=dark
	else
		set background=light
	endif
endif

fun! PostColorScheme()
	let colo = get(g:, 'colors_name', '')
	if colo == 'dual-converted'
		hi StatusLineNC ctermfg=252 ctermbg=24
	elseif colo == 'jellybeans'
		if &t_Co <= 256
			" set cursor color
			sil! !printf '\e]12;8\a'
		end
		hi Search cterm=NONE ctermfg=0 ctermbg=220
	elseif colo == 'railscasts'
		hi TabLine guibg=#0000cc gui=none,reverse guifg=#ffffff
		hi TabLineSel guibg=#0000cc gui=none guifg=#ffffff
		hi TabLineFill guibg=#0000cc gui=none,reverse guifg=#ffffff
		hi Search gui=reverse guifg=#ff9900 guibg=#000000
	end
endf
aug PostColorScheme
	au! ColorScheme * call PostColorScheme()
aug END
if &t_Co > 16
	let s:colors = 'jellybeans'
end
if exists('s:colors')
	exe 'colo' s:colors
end

se nu
set hidden
set laststatus=2 ruler
aug filetypedetect
	au! BufNewFile,BufRead *.markdown,*.md,*.mkd setf markdown
	au! BufNewFile,BufRead *.cron setf crontab
	au! BufNewFile,BufRead *.watchr,Gemfile*,Capfile,Vagrantfile,*.jbuilder setf ruby
	au! BufNewFile,BufRead *.hsc,*xmobarrc setf haskell
aug END
filetype plugin indent on
syntax enable
syntax sync maxlines=2000

" no maximum syntax column, but only if the first line isn't long
aug NoMaxSyntaxLength
	au!
	au BufReadPre * let &l:smc = 3000
	au BufRead * let &l:smc = len(getline(1)) < 3000 ? 0 : 3000
aug END

set foldmethod=marker
aug NoInsertFolding
	au!
	au InsertEnter * if !exists('w:oldfdm') | let w:oldfdm = &fdm | setl fdm=manual | endif
	au InsertLeave,WinLeave * if exists('w:oldfdm') | let &l:fdm = w:oldfdm | unlet w:oldfdm | endif
aug END
" ensure that unsaveable buffers are also uneditable
aug NoEditUnsaveable
	au!
	au BufWinEnter * if !exists('b:swapname') | let &modifiable = !&readonly | endif
aug END

" Default foldtext to include byte length
fun! FoldTextWithOffsets()
	let txt = foldtext()
	let txt = substitute(txt, ':', printf(':%5d', line2byte(v:foldend+1)-line2byte(v:foldstart)).' bytes:','')
	return txt
endfun
se fdt=FoldTextWithOffsets()

" When editing stdin, set it initially to unmodified
aug StdinNotModified
	au!
	au VimEnter * if !bufname('') && (strlen(&fenc) || &bin) | se nomod | endif
aug END
" improve horizontal scrolling (opens folds, alt+{l,h} = faster)
fun! OpenFoldOrDo(action)
	return foldclosed('.') == -1 ? a:action : 'zv'
endfun
nnoremap <expr> l OpenFoldOrDo('l')
nnoremap <expr> h OpenFoldOrDo('h')
nnoremap <expr> <esc>l OpenFoldOrDo('30l')
nnoremap <expr> <esc>h OpenFoldOrDo('30h')

" map \z to a kind of 'reset the folds'
nnoremap <Leader>z zMzvz.

" map z= to 'set fold level equal to the fold I'm on'
fun! SetFoldEqual()
	let start = line('.')
	let foldstart = foldclosed('.')
	let fdl = foldlevel(foldstart > 0 ? foldstart : '.')
	let lnum = 1
	while 1
		if foldclosed(lnum) < 0 && foldlevel(lnum) >= fdl
			exe lnum
			norm zc
			let lnum = foldclosedend(lnum)
		end
		if lnum >= line('$')
			break
		end
		let lnum += 1
	endw
	exe start
endf
nn <silent> z= :call SetFoldEqual()<CR>

if &diff
	nnoremap > :.diffput <bar> diffupdate<cr>
	nnoremap < :.diffput <bar> diffupdate<cr>
else
	" keep visual mode selection when indenting
	vmap > >gv
	vmap < <gv
endif

" record macros into register 'q', playback with Q
nnoremap Q @q
" allow fully-collapsed windows
set winminheight=0
" allow backspace to erase before insertion point
set backspace=2

" treat '*' in visual mode similarly to normal mode
vmap * y/<C-R>=substitute(tolower(getreg('"')), '\([/$~^*\[\]\\]\)', '\\\1', 'g')<CR><CR>

" window mappings
map <esc>m <C-W>_
map <esc>- <C-W>-
map <esc>= <C-W>+

" alt+backspace on commandline removes one dir
cmap <esc><bs> <C-w><C-w>

" pastetoggle
se pastetoggle=<F7>

" Try adding extensions to the detected filename under the cursor
fun! OpenGlobUnderCursor()
	let paths = split(globpath(&path, expand('<cfile>').'.*'), '\n')
	if len(paths) && filereadable(paths[0])
		exe ':new '.fnameescape(paths[0])
		return 1
	end
	return 0
endf

" Let C-w f open a nonexistent file if it fails to find one
fun! OpenOrNewUnderCursor()
	" If we're going to get a directory, try with extensions
	let paths = split(globpath(&path, expand('<cfile>')), '\n')
	if len(paths) && isdirectory(paths[0]) && OpenGlobUnderCursor()
		return
	end
	try
		wincmd f
	catch
		" If we failed, try files with extensions
		if OpenGlobUnderCursor()
			return
		end
		tabnew <cfile>
	endtry
endfun
nnoremap <C-w>f :call OpenOrNewUnderCursor()<CR>

" Ctrl+Arrow = window movement
map <C-Left> <C-W>h
map <C-Down> <C-W>j
map <C-Up> <C-W>k
map <C-Right> <C-W>l
" Ctrl+jk = window movement (C-h and C-l have other meanings)
map <C-j> <C-W>j
map <C-k> <C-W>k

" ZZ = ZZ for all windows, prompt if more than four windows
fun! QuitAll()
	if winnr('$') > 4
		let ans = confirm('Really quit?', "&Yes\n&One\n&No")
		if ans == 2
			x
			return
		elseif ans == 3
			echom 'Cancelled'
			return
		end
	end
	windo x
endf
nn ZZ :call QuitAll()<CR>

set ofu=syntaxcomplete#Complete
set nostartofline
imap <C-@> <C-Space>
" autocmd FileType * set tabstop=4|set shiftwidth=4|set softtabstop=4|set expandtab
autocmd BufRead *.thtml set syntax=thtml
" map  {gq}
let g:tex_flavor = "context"
"vv from http://items.sjbach.com/319/configuring-vim-right ***
set history=1000
set wildmenu
" set wildmode=list:longest
set modeline
set ignorecase
set smartcase

" complete filenames after equals signs
set isf-==

" make increment work when numbers have leading 0's
se nf=hex

" hlsearch
set hls
" turn off highlighting when refreshing the screen
nn <silent> <C-l> :nohls<CR><C-l>

" mappings for tab navigation
nn <silent> <Esc>t :tabnew<CR>
nn <silent> <Esc>{ gT
nn <silent> <Esc>} gt
fun! CloseAndQuitIfLast()
	bd
	if tabpagenr('$') == 1 && winnr('$') == 1 && bufname('') == ''
		q
	end
endf
nn <silent> <Esc>w :call CloseAndQuitIfLast()<CR>

" settings for TOhtml
let g:html_no_progress=1
let g:html_use_css=1
let g:html_number_lines=1
let g:html_ignore_folding=1
let g:html_dynamic_folds=0

" sample python indent munger - au BufWritePre *.py %s/^\( \{8}\)\+/\=substitute(submatch(0), repeat(' ', 8), repeat(' ', 4), 'g')/e
" sample python indent munger - au BufWritePost *.py u

if isdirectory(expand("~/.vim-tmp")) < 1
	if exists("*mkdir")
		call mkdir(expand("~/.vim-tmp"), "p", 0700)
	endif
endif
set backupdir=~/.vim-tmp//,~/.tmp//,~/tmp//,/tmp//
set directory=~/.vim-tmp//,~/.tmp//,~/tmp//,/tmp//
"^^ from http://items.sjbach.com/319/configuring-vim-right ***
if filereadable(expand(s:home.'/.vimrc.local'))
	exe 'source '.s:home.'/.vimrc.local'
endif

let g:no_time_tracking = 1

"" Ctrl-P settings
" default to horizontal open
let g:ctrlp_prompt_mappings = {
	\ 'AcceptSelection("e")': ['<cr>'],
	\ 'AcceptSelection("h")': ['<c-x>'],
	\ }
" no path management ( == use cwd)
let g:ctrlp_working_path_mode = 0
let g:ctrlp_custom_ignore = {
	\ 'dir': '\.git$\|\.hg$\|\.svn$\|tmp$',
	\ 'file': '\.o$',
	\ }
let g:ctrlp_max_height = 100

nm <Leader>n :CtrlPCurFile<CR>

let g:NERDDefaultAlign = 'left'

let g:rtn_open_with = 'vnew'

if InGUI()
	fun! SetupGUI()
		let &gfn = 'DejaVu Sans Mono 14'
		se go-=m go-=T
	endf
	au GUIEnter * call SetupGUI()
else
	" use degraded 256-color palette for Solarized
	let g:solarized_termcolors = 256
end
