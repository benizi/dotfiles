Plugin 'Twinside/vim-haskellConceal'
Plugin 'Lokaltog/vim-powerline'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ervandew/supertab'
Plugin 'benizi/nerdcommenter'
Plugin 'kien/ctrlp.vim'
Plugin 'junegunn/fzf'
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
Plugin 'mattn/emmet-vim'
Plugin 'benizi/vim-yaml'
Plugin 'tpope/vim-unimpaired'
Plugin 'airblade/vim-gitgutter'
Plugin 'nono/vim-handlebars'
Plugin 'slim-template/vim-slim'
Plugin 'tpope/timl'
Plugin 'elixir-lang/vim-elixir'
Plugin 'benizi/vim-txt-256color'
Plugin 'rust-lang/rust.vim'
Plugin 'keith/swift.vim'
PluginVimball 'http://www.drchip.org/astronaut/vim/vbafiles/hilinks.vba.gz'

Plugin 'derekwyatt/vim-scala'
Plugin 'cespare/vim-toml'
Plugin 'benizi/vim-apl'

" Blech. addon-info.json is a PITA without vim-addon itself...
for dep in ['vim-scripts/tlib'] + map(['actions', 'completion',
  \ 'goto-thing-at-cursor', 'errorformats', 'mw-utils'],
  \ '"MarcWeber/vim-addon-" . v:val')
  exe "Plugin '".dep."'"
endfor
Plugin 'MarcWeber/vim-addon-nix'

" Maktaba requirements...
Plugin 'google/vim-maktaba'
Plugin 'google/vim-codefmt'
Plugin 'google/vim-glaive'

finish " God DAMMIT Tim Pope...
" Clojure
Plugin 'guns/vim-clojure-static'
Plugin 'tpope/vim-leiningen'
Plugin 'tpope/vim-fireplace'
