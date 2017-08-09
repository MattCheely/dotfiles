" set SpaceVim colorscheme
let g:spacevim_colorscheme = 'PaperColor'

" set custom plugins
let g:spacevim_custom_plugins = [
    \ ['ElmCast/elm-vim', { 'on_ft' : 'elm' }],
    \ ['darkburn']
    \ ]

let g:elm_format_autosave = 1

let g:syntastic_javascript_checkers = ['eslint']

" set leader
let mapleader=","

" Smart way to move between windows
map <M-j> <C-W>j
map <M-k> <C-W>k
map <M-h> <C-W>h
map <M-l> <C-W>l
