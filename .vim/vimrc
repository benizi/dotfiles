" different locations if running as root
let owner_home = '~'
let s:script = shellescape(expand('~/.vimrc'))
if executable('stat') && system('stat -c %F '.s:script) =~ 'link'
	let owner_home .= split(system('stat -L -c %U '.s:script))[0]
endif
let s:home = owner_home

" Get directory under the set of 'bundled' files
fun! s:BundleDir(...)
	return join(extend([s:home.'/.vim-bundle'],a:000),'/')
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

" keep some plugins around w/o loading by default...
let g:pathogen_disabled = ['powerline']
if !has('gui') || !has('gui_running')
	call add(g:pathogen_disabled, 'CSApprox')
endif
try
	call vam#ActivateAddons() " set up VAM functions
	call pathogen#infect(s:BundleDir()) " activate everything
catch
	echomsg 'Caught exception:'
	echomsg v:exception
	echomsg 'Perhaps pathogen or vim-addon-manager is not installed?'
endtry

" fix it so ~/.vim{.local,} and ~/after/.vim{,.local} are in the right places
let s:rtp = split(&rtp, ',')
let s:special_dirs = map([ '/.vim', '/.vim.local' ], 'expand(s:home.v:val)')
for [ dir, after ] in map(copy(s:special_dirs), '[v:val, 0]')
	\ + map(reverse(copy(s:special_dirs)), '[v:val + "/after", 1]')
	if index(s:rtp, dir) < 0
		continue
	endif
	let s:rtp = filter(copy(s:rtp), 'v:val != dir')
	let s:rtp = after ? (s:rtp + [dir]) : ([dir] + s:rtp)
endfor
"echo join(s:rtp, ',')
let &rtp = join(s:rtp, ',')

set noexpandtab softtabstop=4 tabstop=4 shiftwidth=4
set list listchars=tab:\ \ ,trail:·
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
if &t_Co > 16
	let s:colors = 'jellybeans'
	exe 'colo' s:colors
	if s:colors == 'dual-converted'
		au ColorScheme * hi StatusLineNC ctermfg=252 ctermbg=24
	elseif s:colors == 'jellybeans'
		sil! !printf '\e]12;8\a'
	en
endif
set hidden
set laststatus=2 ruler
aug filetypedetect
	au! BufNewFile,BufRead *.markdown,*.md,*.mkd setf markdown
	au! BufNewFile,BufRead *.cron setf crontab
	au! BufNewFile,BufRead *.watchr,Gemfile*,Capfile,Vagrantfile setf ruby
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

" Let C-w f open a nonexistent file if it fails to find one
fun! OpenOrNewUnderCursor()
	try
		wincmd f
	catch
		new <cfile>
	endtry
endfun
nnoremap <C-w>f :call OpenOrNewUnderCursor()<CR>

fun! CurrentDir()
	let dir = expand('%:h').'/'
	return dir =~ '^\.\?/$' ? '' : dir
endf

cno %% <C-R>=CurrentDir()<cr>
" easy new %:h
nma <Leader>n :new %%
nma <Leader>e :e %%

" Ctrl+Arrow = window movement
map <C-Left> <C-W>h
map <C-Down> <C-W>j
map <C-Up> <C-W>k
map <C-Right> <C-W>l
" Ctrl+jk = window movement (C-h and C-l have other meanings)
map <C-j> <C-W>j
map <C-k> <C-W>k

" ZZ = ZZ for all windows
nnoremap ZZ :windo x<CR>

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

" hlsearch
set hls
" turn off highlighting when refreshing the screen
nn <silent> <C-l> :nohls<CR><C-l>

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
	\ 'AcceptSelection("h")': ['<cr>'],
	\ 'AcceptSelection("e")': ['<c-x>'],
	\ }
" no path management ( == use cwd)
let g:ctrlp_working_path_mode = 0

let g:NERDDefaultAlign = 'left'
