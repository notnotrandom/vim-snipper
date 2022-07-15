" Vim snippets plugin
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

" If the user started a completion on the last word before going to the next
" tabstop, <Space><BS> forces it to end. See documentation for variable
" s:compensatedForHiddenBS, in file autoload/snipper.vim.
inoremap <silent> <Tab> <Space><BS><C-r>=snipper#TriggerSnippet()<CR>
inoremap <silent> <S-Tab> <Space><BS><C-r>=snipper#JumpToPreviousTabStop(1)<CR>

" This mapping for the scenario where the user decided to keep the
" placeholder's text, and, with the placeholder text still selected, hits
" <Tab>. In that case, we first disable select mode (^G), which renders us in
" visual. Hitting 'v' disables visual mode, bringing us to normal mode. Now,
" as the the visual selection is done left to right (cf.
" snipper#FigureOutWhatToReturn()), after hitting 'v', the cursor is left on
" the *rightmost* character of the placeholder. And so, hitting 'a' leaves the
" cursor right after the placeholder text, as if the user had typed it
" himself. Note that this implies entering in Insert mode.
snoremap <silent> <Tab> va<C-r>=snipper#JumpToNextTabStop()<CR>

snoremap <silent> <S-Tab> va<C-r>=snipper#JumpToPreviousTabStop(0)<CR>

nnoremap <silent> <C-s> :call snipper#SearchForTrigger()<CR>
