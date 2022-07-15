
" Let the user have the last word.
if exists('g:snipper_config') && has_key(g:snipper_config, 'disable')
  if g:snipper_config.disable
    redraw
    echomsg("vim-snipper: Disabled by user.")
    finish
  endif
endif

if exists('g:vim_snipper_loaded')
	finish
endif
let g:vim_snipper_loaded = 1

" Defaults.
let s:snipper_config = {
      \    'debug'            : 0,
      \    'disable'          : 0,
      \    'snippet_location' : '~/.vim/snippets/',
      \}

" Override values with user preferences.
if exists('g:snipper_config')
  " If user config exists, add to it any defaults that might be missing.
  call extend(g:snipper_config, s:snipper_config, "keep")
else
  let g:snipper_config = s:snipper_config
endif

" For some reason, this cannot be done in autoload/snipper.vim, because it
" seems to run before the above call to extend()... and so, if the user
" provided a config without specifying snippet_location, when
" autoload/snipper.vim ran, no snippet location would be defined...
let g:snipper#snippets_dir = fnameescape(expand(g:snipper_config.snippet_location))

if g:snipper_config.debug == 0
  let g:snipper_debug = v:false
else
  let g:snipper_debug = v:true
endif

" Set the filetype for .snippets files. Set also the foldmethod to indent.
autocmd BufRead,BufNewFile *.snippets\= set ft=snippet
autocmd FileType snippet setl noet fdm=indent

autocmd BufEnter * call snipper#CheckNeedToBuildSnippetDict()
