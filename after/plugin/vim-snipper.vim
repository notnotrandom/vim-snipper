if exists('s:vim_snipper_loaded')
	finish
endif
let s:vim_snipper_loaded = 1

inoremap <silent> <Tab> <C-r>=snipper#TriggerSnippet()<CR>

" vim:noet:sw=2:ts=2:ft=vim
