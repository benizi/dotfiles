echomsg 'Setting things in' expand('<sfile>')
let vimclojure#ParenRainbow = 1
nmap <buffer> K ,lw
au FileType clojure let b:clojure_namespace = 'user'

" set up nailgun if it exists
let s:ng = expand(owner_home.'/hg/vimclojure/client/ng')
if executable(s:ng)
  let vimclojure#WantNailgun = 1
  let vimclojure#NailgunClient = s:ng
endif
