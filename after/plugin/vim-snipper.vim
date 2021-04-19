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

" Let the user have the last word.
if exists('g:snipper_config') && has_key(g:snipper_config, 'disable')
  if g:snipper_config.disable
    redraw
    echomsg("vim-snipper: Disabled by user.")
    finish
  endif
endif

if exists('b:vim_snipper_loaded')
	finish
endif
let b:vim_snipper_loaded = 1

" Defaults.
let b:snipper_config = {
      \    'debug'            : 0,
      \    'disable'          : 0,
      \    'snippet_location' : '~/.vim/snippets/',
      \}

" Override values with user preferences.
if exists('g:snipper_config')
  call extend(b:snipper_config, g:snipper_config)
endif

if b:snipper_config.debug == 0
  let g:snipper_debug = v:false
else
  let g:snipper_debug = v:true
endif

" Set the filetype for .snippets files. Set also the foldmethod to indent.
au BufRead,BufNewFile *.snippets\= set ft=snippet
au FileType snippet setl noet fdm=indent

inoremap <silent> <Tab> <C-r>=snipper#TriggerSnippet()<CR>
