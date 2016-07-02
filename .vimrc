syntax on
filetype plugin indent on

set hidden
set nocompatible
set number
set nowrap
set showmode
set tw=80
set smartcase
set smarttab
set smartindent
set autoindent
set softtabstop=2
set shiftwidth=2
set expandtab
set incsearch
set mouse=a
set history=1000
set clipboard=unnamedplus,autoselect
set laststatus=2
set completeopt=menuone,menu,longest

set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=longest,list,full
set wildmenu
set completeopt+=longest

set t_Co=256

set cmdheight=1

execute pathogen#infect()

map <Leader>s :SyntasticToggleMode<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif

let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

map <Leader>n :NERDTreeToggle<CR>

let g:haskell_tabular = 1

vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>

map <silent> <Leader>t :CtrlP()<CR>
noremap <leader>b<space> :CtrlPBuffer<cr>
let g:ctrlp_custom_ignore = '\v[\/]dist$'

map <Leader>n <plug>NERDTreeTabsToggle<CR>

let g:airline#extensions#tabline#enabled = 1

noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt

hi CtrlSpaceSearch guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=236 term=bold cterm=bold
hi CtrlSpaceSelected guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=17 term=bold cterm=bold
hi CtrlSpaceNormal guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=28 term=bold cterm=bold
hi CtrlSpaceStatus guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=31 term=bold cterm=bold


"hi Search guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=236 term=none cterm=none
hi PMenuSel guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=17 term=none cterm=none
hi PMenu guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=28 term=none cterm=none
"hi StatusLine guifg=#eeeeee guibg=#303030 gui=bold ctermfg=255 ctermbg=31 term=bold cterm=bold

" For global replace
nnoremap gR gD:%s/<C-R>///gc<left><left><left>

let g:vim_annotations_offset = '/.liquid/'

" Easy align interactive
vnoremap <silent> <Enter> :EasyAlign<cr>

