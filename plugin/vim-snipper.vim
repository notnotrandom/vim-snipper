" Set the filetype for .snippets files. Set also the foldmethod to indent.
au BufRead,BufNewFile *.snippets\= set ft=snippet
au FileType snippet setl noet fdm=indent

let s:snippets = {}

if !exists('snippets_dir')
	let g:snippets_dir = substitute(globpath(&rtp, 'snippets/'), "\n", ',', 'g')
endif

function TriggerSnippet()
	let text = readfile(g:snippets_dir . "/_.snippets")
	let inSnip = 0
	for line in text + ["\n"]
    echomsg line
	endfor
endfunction

let s:vim_snipper_loaded = 1
