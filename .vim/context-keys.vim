if exists("g:use_wonky_context_keys") " don't forget the endif
if exists("mapleader")
    let maplocalleader = mapleader
endif

" Make start-stop block out of the previous word
imap <buffer> <LocalLeader>ta \start<Cr>\stop<Cr><Esc>4bhdiw$pj$pO
imap <buffer> <LocalLeader>tb \begin<Cr>\end<Cr><Esc>4bhdiw$pj$pO

" Itemize
imap <buffer> <LocalLeader>it \startitemize<Cr>\stopitemize<Esc>O\item<Space>
imap <buffer> <LocalLeader>en \startitemize[n]<Cr>\stopitemize<Esc>O\item<Space>
imap <buffer> <LocalLeader>i<Return> \item<Space>

" Font switching and emphasize
imap <buffer> <LocalLeader>em {\em }<Left>
imap <buffer> <LocalLeader>sc {\sc }<Left>

" Define... and setup...
imap <buffer> <LocalLeader>de \define
imap <buffer> <LocalLeader>se \setup

" Typing and type
imap <buffer> <LocalLeader>ty \type{}<Left>
imap <buffer> <LocalLeader>typ typing<LocalLeader>ta

" Quote and quotation
imap <buffer> <LocalLeader>" \quotation{}<Left>
imap <buffer> <LocalLeader>' \quote{}<Left>
endif
