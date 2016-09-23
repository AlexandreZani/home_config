if filereadable(glob("~/.local.conf/.vimrc.early"))
  source ~/.local.conf/.vimrc.early
endif

" Syntax highlighting
syntax on

" No need to backup
set nobackup
set noswapfile

" Line numbers
set number

" Automatic curly-brace indent
set cindent
set autoindent

" Highlight search results
set hlsearch
" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" Incremental search.
set incsearch

set printoptions=number:y,paper:letter
set nocompatible
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

noremap <C-j> <C-d>
noremap <C-k> <C-u>

au! BufRead,BufNewFile *.json setfiletype json
au BufRead,BufNewFile *.as,*.jsfl,*.mxml set filetype=actionscript
au BufRead,BufNewFile *.go set filetype=go
au BufRead,BufNewFile *.mojom set filetype=mojom
let javaScript_fold=1

" Auto reload vimrc when you save it
if has("autcmd")
  autocmd BufWritePost .vimrc source $MYVIMRC
  autocmd BufWritePost .vimrc.local source $MYVIMRC
endif

" Disable arrow keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>
imap jj <Esc>

" Change colors beyond 80 characters
let &colorcolumn=join(range(81, 999), ",")
highlight ColorColumn ctermbg=DarkGray

if filereadable(glob("~/.local.conf/.vimrc"))
  source ~/.local.conf/.vimrc
endif
