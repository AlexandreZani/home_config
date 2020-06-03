function GofmtBuffer()
  let cursor_pos = getpos('.')
  :w !gofmt > /dev/null
  if v:shell_error == 0
    :%!gofmt
  endif
  call setpos('.', cursor_pos)
endfunction

autocmd BufWritePre * :call GofmtBuffer()
