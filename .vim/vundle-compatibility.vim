fun! s:GitGet(git, ...)
  let [user, repo] = split(a:git, '/')
  let dest = a:0 ? a:1 : BundleDir(repo)
  if !isdirectory(dest)
    echom 'Installing' a:git
    call system('git clone https://github.com/'.user.'/'.repo.' '.shellescape(dest))
  end
endf

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
end
cal confirm('wtf','a')

com! -nargs=* Plugin :cal s:GitGet(<args>)

for dir in filter(GlobList(g:bundle_dir.'/*', 1), 'isdirectory(v:val)')
  call BundleActivateDir(dir)
endfor
run bundles.vim
run! plugin/pivotal.vim

" TODO: update to Vundle '{{dir}}', {'pinned': 1}
verb call BundleActivateDir($HOME.'/git/forever-inc/forever-tools/vim')
verb call BundleActivateDir($HOME.'/git/urbit/extras/hoon.vim')
