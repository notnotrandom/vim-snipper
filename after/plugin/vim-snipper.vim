" Vim snippets plugin
" Languages:    any
" Maintainer:   Óscar Pereira
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
"    Copyright Óscar Pereira, 2020-2022
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

if exists('g:vim_snipper_loaded')
	finish
endif
let g:vim_snipper_loaded = 1

" Defaults.
let b:snipper_config = {
      \    'debug'            : 0,
      \    'disable'          : 0,
      \    'snippet_location' : '~/.vim/snippets/',
      \}

" Override values with user preferences.
if exists('g:snipper_config')
  call extend(g:snipper_config, b:snipper_config)
else
  let g:snipper_config = b:snipper_config
endif

if g:snipper_config.debug == 0
  let g:snipper_debug = v:false
else
  let g:snipper_debug = v:true
endif

" Set the filetype for .snippets files. Set also the foldmethod to indent.
au BufRead,BufNewFile *.snippets\= set ft=snippet
au FileType snippet setl noet fdm=indent

inoremap <silent> <Tab> <C-r>=snipper#TriggerSnippet()<CR>

" This mapping for the scenario where the user decided to keep the
" placeholder's text, and, with the placeholder text still selected, hits
" <Tab>. In that case, we first disable select mode (^G), which renders us in
" visual. Hitting 'v' disables visual mode, bringing us to normal mode. Now,
" as the the visual selection is done left to right (cf.
" snipper#FigureOutWhatToReturn()), after hitting 'v', the cursor is left on
" the *rightmost* character of the placeholder. And so, hitting 'a' leaves the
" cursor right after the placeholder text, as if the user had typed it
" himself.
snoremap <silent> <Tab> va<C-r>=snipper#JumpToNextTabStop()<CR>

nnoremap <silent> <C-s> :call snipper#SearchForTrigger()<CR>

autocmd BufEnter * call snipper#checkNeedToBuildSnippetDict()
