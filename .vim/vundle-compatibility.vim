fun! s:GitGet(git, ...)
  let [user, repo] = split(a:git, '/')
  let dest = a:0 ? a:1 : BundleDir(repo)
  if !isdirectory(dest)
    echom 'Installing' a:git
    call system('git clone https://github.com/'.user.'/'.repo.' '.shellescape(dest))
  end
endf

fun! s:VimballUnpack(lines, dest)
  let lines = copy(a:lines)
  if lines[0] !~ '^" Vimball' || lines[1:2] != ['UseVimball', 'finish']
    throw 'Vimball is malformed'
  end
  let lines = lines[3:]
  while len(lines)
    let filespec = matchlist(lines[0], '^\(\S\+\)\s\+\[\[\[1$')
    if !len(filespec)
      throw 'Vimball is malformed'
    end
    let lines = lines[1:]
    let filename = a:dest.'/'.filespec[1]

    let file_nlines = str2nr(lines[0])
    let lines = lines[1:]
    if file_nlines == 0 || file_nlines > len(lines)
      throw 'Vimball is malformed'
    end

    cal posix#mkdirp(fnamemodify(filename, ':h'))
    cal writefile(lines[:(file_nlines-1)], filename)
    let lines = lines[(file_nlines):]
  endwhile
endf

fun! s:VimballInstall(lines, dest)
  if !isdirectory(a:dest)
    cal s:VimballUnpack(a:lines, a:dest)
  end
  return BundleActivateDir(a:dest)
endf

fun! s:VimballGet(url, ...)
  if a:0
    let dest = a:0
  else
    let match = matchlist(a:url, '^http.*/\(\w\+\)\.vba\.gz$')
    if !len(match)
      throw "Couldn't determine destination for ".a:url
    end
    let dest = match[1]
  end
  if dest !~ '/'
    let dest = BundleDir(dest)
  end
  let vba = dest.'.vba.gz'
  if !filereadable(vba)
    echom 'Downloading' a:url 'to' vba
    cal system('curl '.shellescape(a:url).' -o '.shellescape(vba))
  end
  if !filereadable(vba)
    throw "Couldn't download ".a:url." to ".vba
  end
  let cmd = 'gzip -dc < '.shellescape(vba)
  return s:VimballInstall(vimcompat#systemlist(cmd), dest)
endf

" Not actually Vundle-compatible (yet?)
com! -nargs=* PluginVimball :cal s:VimballGet(<args>)

fun! VundleAvailable()
	return isdirectory(g:vundle_dir)
endf

" Use Vundle if available
let g:use_vundle_p = has_key(g:, 'use_vundle')
let g:use_vundle = get(g:, 'use_vundle', VundleAvailable())

let install = get(g:, 'install_vundle', 0)

" Prompt to install Vundle if it's unavailable and no plugins are installed
if !VundleAvailable() && !isdirectory(g:bundle_dir)
  let choices = "&Yes\n&No\n&Skip plugins\n&Quit"
  let install = confirm('Install Vundle? ['.g:vundle_dir.']', choices)
  if install == 4
    qall
  end
end

" Skip plugins altogether
if install == 3
  com! -nargs=* Plugin :
  finish
end

" Install Vundle
if install == 1
  cal s:GitGet('VundleVim/Vundle.vim', g:vundle_dir)
  let g:use_vundle = g:use_vundle_p ? g:use_vundle : VundleAvailable()
end

if install == 1 || install == 2
  let any_old = 0
  let test_dirs = []
  cal add(test_dirs, g:vim_dir.'/bundles')
  cal add(test_dirs, expand('~/vim-bundles'))
  cal add(test_dirs, expand('~/vim.bundles'))
  for dir in test_dirs
    if isdirectory(dir)
      let any_old = 1
      echoe 'Old bundle dir exists:' dir
    end
  endfor
  if any_old
    echoe 'Quitting installation'
    qall
  end
end

if g:use_vundle && VundleAvailable()
  cal AddToRtp(g:vundle_dir)
  cal vundle#begin(g:bundle_dir)
  run! bundles.vim
  cal vundle#end()
  if install == 1
    PluginInstall
  end
  finish
elseif g:use_vundle
  echoe 'Vundle was requested, but is not available'
  finish
end

" Vundle not installed

if !g:use_vundle_p
  echom 'Falling back to vundle compatibility mode'
end

if install == 2 && !isdirectory(g:bundle_dir)
  let choice = confirm('Install all plugins?', "&Yes\n&No\n&Quit")
  if choice == 2
    finish
  elseif choice == 3
    qall
  end
else
  let failnow = confirm('WTF? Not sure how we got here. Quit?', "&Yes\n&No")
  if choice != 2
    qall
  end
end

com! -nargs=* Plugin :cal s:GitGet(<args>)

for dir in filter(GlobList(g:bundle_dir.'/*', 1), 'isdirectory(v:val)')
  call BundleActivateDir(dir)
endfor
run bundles.vim
run! plugin/pivotal.vim

" TODO: update to Vundle '{{dir}}', {'pinned': 1}
verb call BundleActivateDir($HOME.'/git/forever-inc/forever-tools/vim')
verb call BundleActivateDir($HOME.'/git/urbit/extras/hoon.vim')
