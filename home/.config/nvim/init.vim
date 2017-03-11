" ========= Vundle Bundle Management ============
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
" required!
Plugin 'VundleVim/Vundle.vim'

" github repos
Plugin 'airblade/vim-gitgutter'
Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'goldfeld/vim-seek'
Plugin 'jnurmine/Zenburn'
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'rking/ag.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'nixprime/cpsm'
Plugin 'Shougo/denite.nvim'
Plugin 'Shougo/deoplete.nvim'
Plugin 'tpope/vim-fugitive'
Plugin 'ElmCast/elm-vim'
Plugin 'editorconfig/editorconfig-vim'

" NeoVim specific
Plugin 'rhysd/nyaovim-mini-browser'
" Plugin 'neomake/neomake'

"vim-scripts
Plugin 'darkburn'
Plugin 'Rename2'

call vundle#end()

" Enable filetype plugins
filetype plugin on
filetype indent on


" ========= Config ==============================

" Set up custom filetypes
au BufNewFile,BufRead *.tag set filetype=html

" Show line numbers
set number

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","

" Fast saving & exit
nmap <leader>w :w!<cr>
nmap <leader>x :x!<cr>
nmap <leader>q :q<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugin config
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use deoplete
let g:deoplete#enable_at_startup = 1

" == Elm ==

let g:elm_format_autosave = 1
let g:elm_setup_keybindings = 0;

nnoremap <leader>r <Plug>(elm-make)
nnoremap <leader>b <Plug>(elm-make-main)
nnoremap <leader>t <Plug>(elm-test)
nnoremap <leader>r <Plug>(elm-repl)
nnoremap <leader>ed <Plug>(elm-error-detail)
nnoremap <leader>d <Plug>(elm-show-docs)

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:elm_syntastic_show_warnings = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Toggle NERDTree
nmap <leader>e :NERDTreeToggle<cr>

" Exit if NERDTree is the only open window
" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

let NERDTreeShowBookmarks = 1
let NERDTreeChDirMode = 2


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Denite & Ag (search)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use fuzzy matcher for file lookup
call denite#custom#source('file_rec', 'matchers', [ 'matcher_cpsm' ])

if executable('ag')
    call denite#custom#var('file_rec', 'command', [ 'ag', '--nocolor', '--nogroup', '-g', ''])
    call denite#custom#var('grep', 'command', [ 'ag' ])
    call denite#custom#var('grep', 'default_opts', [ '--vimgrep' ])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', [])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
endif

" search for a file in the CWD
nnoremap <leader>f :<C-u>Denite -auto-resize -auto-preview file_rec<cr>
" grep in the CWD
nnoremap <leader>g :<C-u>Denite -auto-resize -auto-preview -mode=normal grep:<c-r>=getcwd()<cr>::

"Custom mappings in the unite buffer
" function! s:unite_settings()
"     " Navigate with control-j/k
"     imap <buffer> <C-j> <Plug>(unite_select_next_line)
"     imap <buffer> <C-k> <Plug>(unite_select_previous_line)
"     " exit unite
"     imap <buffer> <C-q> <Plug>(unite_exit)
"     nmap <buffer> <C-q> <Plug>(unite_exit)
"     " reset unite cache
"     imap <buffer> <C-r> <Plug>(unite_redraw)
" endfunction
" autocmd FileType unite call s:unite_settings()

" --- type * to search for a word in all files
nmap* :<C-u>Denite -auto-resize -auto-preview -mode=normal grep:<c-r>=getcwd()<cr>::<c-r>=expand("<cword>")<cr><cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Highlight column 100
if (exists('+colorcolumn'))
    set colorcolumn=100
endif

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Show trailing whitespace & tabs
set list
set listchars=tab:»\ ,trail:·

" Turn on the WiLd menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc

"Always show current position
set ruler

" A buffer becomes hidden when it is abandoned
set hidden

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
"set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable syntax highlighting
syntax enable

try
    colorscheme darkburn
catch
endtry

command Present colorscheme PaperColor | set background=light
command UnPresent colorscheme darkburn

highlight ColorColumn guibg=#121212

" Set extra options when running in GUI mode
if has("gui_running")
    set guifont=Inconsolata\ for\ Powerline\ Medium\ 13
    "set guifont=Fantasque\ Sans\ Mono\ 14
    set guioptions-=T
    set guioptions-=m
    set guioptions+=e
    set t_Co=256
    set guitablabel=%M\ %t
endif

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

"set ai "Auto indent
"set si "Smart indent
set wrap "Wrap lines


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <M-j> <C-W>j
map <M-k> <C-W>k
map <M-h> <C-W>h
map <M-l> <C-W>l

" Quick movement between errors
map <leader>en :lnext<cr>
map <leader>ep :lprevious<cr>

" Close the current buffer
"map <leader>bd :Bclose<cr>

" Close all the buffers
"map <leader>ba :1,1000 bd!<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <C-h> :tabmove -1<cr>
map <C-l> :tabmove +1<cr>

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
  set switchbuf=useopen,usetab ",newtab
  set stal=2
catch
endtry

" Return to last edit position when opening files (You want this!)
autocmd BufReadPost *
     \ if line("'\"") > 0 && line("'\"") <= line("$") |
     \   exe "normal! g`\"" |
     \ endif
" Remember info about open buffers on close
set viminfo^=%


""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" Format the status line
"set statusline=\ %f%m%r%h\ \ CWD:\ %.40{getcwd()}\ \ \ Line:\ %l.%c

"Airline
let g:airline_powerline_fonts = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap VIM 0 to first non-blank character
map 0 ^

" Let Ctrl+V paste in insert mode
imap <C-v> <esc>"+gpi<right>

" Move a line of text using Ctrl+[jk]
nmap <C-j> mz:m+<cr>`z
nmap <C-k> mz:m-2<cr>`z
vmap <C-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <C-k> :m'<-2<cr>`>my`<mzgv`yo`z

if has("mac") || has("macunix")
  nmap <D-j> <M-j>
  nmap <D-k> <M-k>
  vmap <D-j> <M-j>
  vmap <D-k> <M-k>
endif

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
  exe "normal mz"
  %s/\s\+$//ge
  exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()
autocmd BufWrite *.js :call DeleteTrailingWS()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Misc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Quickly open a buffer for scripbble
" map <leader>q :e ~/buffer<cr>

" Toggle paste mode on and off
map <leader>pp :setlocal paste!<cr>

" Syntax hilight debugging
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
