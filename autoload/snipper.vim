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

function snipper#TriggerSnippet()
	let l:line = getline(".")
  let l:col = col(".")
  let l:charCol = charcol(".")
  echom l:col

  if l:col == 1 || l:line[l:col - 2] =~ '\m\W'
    " If we are on column 1, or if the char in the column immediately before
    " the cursor is non-alphanumeric, then there is no snippet to expand. So
    " just return an actual <Tab> character.
    return "\<Tab>"
  else
    " Otherwise, before the cursor there is a potential trigger. So I go find
    " out its bounds (array indexes). Basically, start at the char immediately
    " before the cursor, and keep going back until a non-alphanumeric char is
    " found.
    let l:triggerEndCharIdx = l:col - 2
    let l:prevCharIdx = l:triggerEndCharIdx

    while l:prevCharIdx >= 1 && l:line[l:prevCharIdx - 1] =~ '\m\w'
      let l:prevCharIdx -= 1
    endwhile

    " Now, after the above while loop, l:prevCharIdx contain the array idx of
    " the first character of the trigger word.
  endif

  let l:triggerLength = l:triggerEndCharIdx - l:prevCharIdx + 1
  let l:trigger = strpart(l:line, l:prevCharIdx, l:triggerLength) " trigger must be ascii only
  echom l:trigger

  if l:prevCharIdx > 0
    let l:beforeTrigger = l:line[0 : l:prevCharIdx - 1]
  else
    let l:beforeTrigger = ''
  endif

  if l:triggerEndCharIdx < strcharlen(l:line)
    let l:afterTrigger = l:line[l:triggerEndCharIdx + 1 : ]
  else
    let l:afterTrigger = ''
  endif

  if l:trigger == 'ra'
    call setline(".", l:beforeTrigger . "\\rightarrow" . l:afterTrigger)
    call setcharpos('.', [0, line("."), l:charCol + 11 - l:triggerLength])
    return ''
  else
    return "\<Tab>"
  endif
endfunction
