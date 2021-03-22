" Vim snippets plugin
" Languages:    any
" Maintainer:   Óscar Pereira
" Version:      0.1
" License:      GPL

"************************************************************************
"
"                     vim-snipper library: Vim script
"
"    This program is free software: you can redistribute it and/or modify
"    it under the terms of the GNU General Public License as published by
"    the Free Software Foundation, either version 3 of the License, or
"    (at your option) any later version.
"
"    This program is distributed in the hope that it will be useful,
"    but WITHOUT ANY WARRANTY; without even the implied warranty of
"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"    GNU General Public License for more details.
"
"    You should have received a copy of the GNU General Public License
"    along with this program. If not, see <http://www.gnu.org/licenses/>.
"
"    Copyright Óscar Pereira, 2020-2021
"
"************************************************************************

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
