" same as vim-codefmt/plugin/register.vim, except for JavaScript changes
let s:registry = maktaba#extension#GetInternalRegistry('codefmt')
call s:registry.SetValidator('codefmt#EnsureFormatter')

" Skip `js-beautify`:
" call s:registry.AddExtension(codefmt#jsbeautify#GetFormatter())

" Modify `clang-format` so it isn't used for JS or Java files:
" call s:registry.AddExtension(codefmt#clangformat#GetFormatter())
let s:clang = codefmt#clangformat#GetFormatter()
fun! s:clang.AppliesToBuffer() abort
  return index(['c','cpp','proto'], &ft) >= 0
endf
call s:registry.AddExtension(s:clang)

" Other formatters are added as-is:
call s:registry.AddExtension(codefmt#gofmt#GetFormatter())
call s:registry.AddExtension(codefmt#dartfmt#GetFormatter())
call s:registry.AddExtension(codefmt#yapf#GetFormatter())
call s:registry.AddExtension(codefmt#autopep8#GetFormatter())
call s:registry.AddExtension(codefmt#gn#GetFormatter())
call s:registry.AddExtension(codefmt#buildifier#GetFormatter())
call s:registry.AddExtension(codefmt#googlejava#GetFormatter())

let s:plugin = maktaba#plugin#Get('codefmt')

fun! s:GetExe() dict
  return s:plugin.Flag(self.exe_flag)
endf

fun! s:IsAvailable() dict
  return executable(self.GetExe())
endf

fun! s:AppliesToBuffer(...)
  return index(a:000, &ft) >= 0
endf

fun! s:CreateFormatter(name, ...)
  let fmt = {'name': a:name, 'exe_flag': a:name . '_executable'}
  cal s:plugin.Flag(fmt.exe_flag, fmt.name)
  let fmt.GetExe = function('s:GetExe', fmt)
  let fmt.IsAvailable = function('s:IsAvailable', fmt)
  if a:0
    let fmt.AppliesToBuffer = function('s:AppliesToBuffer', a:000)
  end
  return fmt
endf

" Use `prettier` to format JavaScript:
let s:prettier = s:CreateFormatter('prettier', 'javascript')

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

" Use `jq` to format JSON
let s:jq = s:CreateFormatter('jq', 'json')

cal s:plugin.Flag('jq_sort_keys', 0)

fun! s:jq.Format() dict abort
  let cmd = [self.GetExe()]
  if s:plugin.Flag('jq_sort_keys')
    cal add(cmd, '--sort-keys')
  end
  cal add(cmd, '.')
  let input = join(getline(1,'$'), "\n")
  let ret = maktaba#syscall#Create(cmd).WithStdin(input).Call(0)
  if !empty(ret.stderr)
    echoerr ret.stderr
    return
  end
  cal maktaba#buffer#Overwrite(1, line('$'), split(ret.stdout, "\n"))
endf

cal s:registry.AddExtension(s:jq)
