" same as vim-codefmt/plugin/register.vim, except for JavaScript changes
let s:registry = maktaba#extension#GetInternalRegistry('codefmt')
call s:registry.SetValidator('codefmt#EnsureFormatter')

" Skip `js-beautify`:
" call s:registry.AddExtension(codefmt#GetJsBeautifyFormatter())

" Modify `clang-format` so it isn't used for JS or Java files:
" call s:registry.AddExtension(codefmt#GetClangFormatFormatter())
let s:clang = codefmt#GetClangFormatFormatter()
fun! s:clang.AppliesToBuffer() abort
  return index(['c','cpp','proto'], &ft) >= 0
endf
call s:registry.AddExtension(s:clang)

" Other formatters are added as-is:
call s:registry.AddExtension(codefmt#GetGofmtFormatter())
call s:registry.AddExtension(codefmt#GetYAPFFormatter())
call s:registry.AddExtension(codefmt#GetAutopep8Formatter())
call s:registry.AddExtension(codefmt#GetGnFormatter())
