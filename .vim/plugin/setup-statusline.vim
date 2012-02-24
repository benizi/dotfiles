" default statusline
let s:tabstops='(%{repeat(&l:et?"·":"»",&l:sw)}%{&l:sts==&l:ts?"":repeat(&l:et?"¬":"·",&l:sts)}%{&l:sw==&l:ts?"":repeat(&l:et?"|":"¬",&l:ts)})'
let s:ff='%{&l:ff=="unix"?"":" [".&l:ff."]"}'
let s:enc='strlen(&l:fenc)?&l:fenc:&l:enc'
let s:ff.='%{('.s:enc.')=="utf-8"?"":" [".('.s:enc.')."]"}'
let s:typ='%{(expand("%:e")==&l:ft)?"":" <".&l:ft.">"}'
let s:extra='%{(exists("g:stl_extra")&&len(g:stl_extra))?":".join(map(copy(g:stl_extra),"v:val=~\"^!\"?eval(strpart(v:val,1)):v:val"),","):""}'
let &stl='%<%f'.s:typ.s:extra.' %h%m%r'.s:ff.'%='.s:tabstops.(&ruler?' ':'').(&ruler ? '%-14.(%l,%c%V%) %P' : '').'/%{line("$")}'
" %-5.(%{AwkFields#GetVar()}%)'
