fun! s:ProxSort(a, b)
  return a:a.proximity - a:b.proximity
endf

fun! JumpToNextSign()
  let buf = bufnr('%')
  redir => sign_output
  exe "sil! sign place buffer=".buf
  redir END
  let curline = line('.')
  let maxline = line('$')
  let signs = []
  for line in split(sign_output, '\n')
    if line =~ '^ '
      let sign={}
      for [k, v] in map(split(line), 'split(v:val, "=")')
        if v =~ '^\d\+$'
          let v = str2nr(v)
        end
        let sign[k]=v
      endfor
      let sign.proximity = sign.line - curline + (sign.line < curline ? maxline : 0)
      if !sign.proximity
        let sign.proximity = maxline + 1
      end
      call add(signs, sign)
    end
  endfor

  if !len(signs)
    echoe 'No signs in this buffer'
    return
  end

  call sort(signs, '<SID>ProxSort')
  let sign = signs[0]
  exe "sign jump" sign.id "buffer=".buf
endf
