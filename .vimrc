syntax on
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
set formatoptions+=ro
set shiftwidth=2
set expandtab
set softtabstop=2
au! BufRead,BufNewFile *.json setfiletype json
let javaScript_fold=1

if filereadable(glob("~/.vimrc.local"))
  source ~/.vimrc.local
endif
