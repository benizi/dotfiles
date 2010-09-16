" default statusline
let s:tabstops='(%{repeat(&l:et?"·":"»",&l:sw)}%{&l:sts==&l:ts?"":repeat(&l:et?"¬":"·",&l:sts)}%{&l:sw==&l:ts?"":repeat(&l:et?"|":"¬",&l:ts)})'
let &stl='%<%f %h%m%r%='.s:tabstops.(&ruler?' ':'').(&ruler ? '%-14.(%l,%c%V%) %P' : '')
" %-5.(%{AwkFields#GetVar()}%)'
