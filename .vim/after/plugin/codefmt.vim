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

fun! s:prettier.FormatRange(startline, endline) abort
  cal maktaba#ensure#IsNumber(a:startline)
  cal maktaba#ensure#IsNumber(a:endline)

  let cmd = [s:prettier.GetExe(), '--stdin']

  " `prettier` ranges are expressed in characters (not lines, which are very
  " easy to find, or bytes, which are easy to find via `line2byte()`), so we
  " calculate line lengths by appending EOL chars to line contents.
  let eol = (&ff != 'unix' ? "\<CR>" : "") . (&ff != 'mac' ? "\<NL>" : "")
  " prepend 0 for empty case and the fact that line numbers are 1-indexed:
  let linechars = extend([0],map(getline(1,a:endline),'strchars(v:val . eol)'))

  " range-start is everything preceding the start line:
  let preceding = copy(linechars[0:(a:startline-1)])
  cal add(cmd, '--range-start='.eval(join(preceding,'+')))

  " range-end includes all lines up through the end line:
  let included = copy(linechars[0:(a:endline)])
  " + 1, since it's exclusive:
  cal add(cmd, '--range-end='.(eval(join(included,'+')) + 1))

  let input = join(getline(1,'$'), eol)
  let ret = maktaba#syscall#Create(cmd).WithStdin(input).Call(0)
  if !empty(ret.stderr)
    echoerr ret.stderr
    return
  end
  cal maktaba#buffer#Overwrite(1, line('$'), split(ret.stdout, eol))
endf

cal s:registry.AddExtension(s:prettier)
