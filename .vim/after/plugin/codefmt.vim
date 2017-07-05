" same as vim-codefmt/plugin/register.vim
let s:registry = maktaba#extension#GetInternalRegistry('codefmt')
call s:registry.SetValidator('codefmt#EnsureFormatter')

call s:registry.AddExtension(codefmt#GetJsBeautifyFormatter())
call s:registry.AddExtension(codefmt#GetClangFormatFormatter())
call s:registry.AddExtension(codefmt#GetGofmtFormatter())
call s:registry.AddExtension(codefmt#GetYAPFFormatter())
call s:registry.AddExtension(codefmt#GetAutopep8Formatter())
call s:registry.AddExtension(codefmt#GetGnFormatter())
