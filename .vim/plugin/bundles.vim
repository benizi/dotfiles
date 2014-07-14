fun! Plugin(git, ...)
  let [user, repo] = split(a:git, '/')
  let dest = a:0 ? a:1 : BundleDir(repo)
  if a:0
    echoe 'Second argument to Plugin not currently supported'
    return
  end
  if !isdirectory(dest)
    call system('git clone https://github.com/'.user.'/'.repo.' '.shellescape(dest))
  end
endf

com -nargs=* Plugin :call Plugin(<args>)

Plugin 'Twinside/vim-haskellConceal'
Plugin 'Lokaltog/vim-powerline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ervandew/supertab'
Plugin 'benizi/nerdcommenter'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'benizi/vim-automkdir'
Plugin 'kana/vim-filetype-haskell'
Plugin 'nanotech/jellybeans.vim'
Plugin 'tpope/vim-pathogen'
Plugin 'benizi/vim-rails'
Plugin 'benizi/vim-rails-test-navigator'
Plugin 'tpope/vim-rake'
Plugin 'MarcWeber/vim-addon-manager-known-repositories'
Plugin 'MarcWeber/vim-addon-manager'
Plugin 'kchmck/vim-coffee-script'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-vividchalk'
Plugin 'benizi/vim-endwise'
Plugin 'rodjek/vim-puppet'
Plugin 'jnwhiteh/vim-golang'
Plugin 'digitaltoad/vim-jade'
Plugin 'mattn/zencoding-vim'
Plugin 'benizi/vim-yaml'
Plugin 'tpope/vim-unimpaired'
Plugin 'airblade/vim-gitgutter'
Plugin 'nono/vim-handlebars'
Plugin 'slim-template/vim-slim'
Plugin 'tpope/timl'
Plugin 'elixir-lang/vim-elixir'
call AddToRtp($HOME.'/git/rust/src/etc/vim')

finish " God DAMMIT Tim Pope...
" Clojure
Plugin 'guns/vim-clojure-static'
Plugin 'tpope/vim-leiningen'
Plugin 'tpope/vim-fireplace'
