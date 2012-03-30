" Syntax highlighting
syntax on

" Line numbers
set number

set printoptions=number:y,paper:letter
set nocompatible
set autoindent
set ruler
if version >= 600
  set foldenable
  set foldmethod=syntax
endif
let g:tex_flavor = "latex"
filetype plugin indent on
set nocp
filetype plugin on

" Tabs are 2 spaces darn it!
set formatoptions+=ro
set tabstop=2
set shiftwidth=2
set expandtab
set softtabstop=2


" Easier hex editing
nnoremap <C-H> :Hexmode<CR>
inoremap <C-H> <Esc>:Hexmode<CR>
vnoremap <C-H> :<C-U>Hexmode<CR>

au! BufRead,BufNewFile *.json setfiletype json
let javaScript_fold=1

" Auto reload vimrc when you save it
if has("autcmd")
  autocmd BufWritePost .vimrc source $MYVIMRC
  autocmd BufWritePost .vimrc.local source $MYVIMRC
endif

if filereadable(glob("~/.vimrc.local"))
  source ~/.vimrc.local
endif
