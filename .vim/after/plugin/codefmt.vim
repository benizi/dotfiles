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

" And we add `prettier`:
let s:plugin = maktaba#plugin#Get('codefmt')
cal s:plugin.Flag('prettier_executable', 'prettier')

let s:prettier = {'name':'prettier'}

fun! s:prettier.GetExe()
  return s:plugin.Flag('prettier_executable')
endf

fun! s:prettier.IsAvailable()
  return executable(s:prettier.GetExe())
endf

fun! s:prettier.AppliesToBuffer()
  return &ft == 'javascript'
endf

fun! s:prettier.Format() abort
  let cmd = [s:prettier.GetExe(), '--stdin']
  let input = join(getline(1,'$'), "\n")
  let ret = maktaba#syscall#Create(cmd).WithStdin(input).Call(0)
  if !empty(ret.stderr)
    echoerr ret.stderr
    return
  end
  cal maktaba#buffer#Overwrite(1, line('$'), split(ret.stdout, "\n"))
endf

cal s:registry.AddExtension(s:prettier)
