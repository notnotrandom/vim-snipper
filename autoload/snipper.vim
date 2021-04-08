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
	let l:line = getline(".") " Current line.

  " col(".") returns the column the cursor is at, starting at 1. It counts
  " *byte* positions, not visible char positions. charcol(".") does the same
  " thing, but counting char positions. This is needed when the cursor is to
  " be placed after the expanded snippet, if prior to the snippet expansion
  " point, the line contains non-ascii characters. In such a case, the values
  " of col and charcol will differ -- and as we see chars, not bytes, it is
  " the latter that must be used.
  let l:col = col(".")
  let l:charCol = charcol(".")

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

  " The variable l:beforeTrigger contains that part of the line that comes
  " before the trigger text, if any. The reason we need an if condition is
  " because if l:prevCharIdx = 0, then doing l:line[0 : l:prevCharIdx - 1]
  " results in l:line[0:-1], which returns the entire line...
  if l:prevCharIdx > 0
    let l:beforeTrigger = l:line[0 : l:prevCharIdx - 1]
  else
    let l:beforeTrigger = ''
  endif

  " The variable l:afterTrigger contains that part of the line that comes
  " after the trigger text, if any. The reason we need an if condition is to
  " avoid an "array index out of bounds" error...
  if l:triggerEndCharIdx < strcharlen(l:line)
    let l:afterTrigger = l:line[l:triggerEndCharIdx + 1 : ]
  else
    let l:afterTrigger = ''
  endif

  if l:trigger == 'ra'
    let l:triggerExpansion = "\\rightarrow"
    call setline(".", l:beforeTrigger . l:triggerExpansion . l:afterTrigger)
    call setcharpos('.', [0, line("."), l:charCol + strcharlen(l:triggerExpansion) - l:triggerLength])
    return ''
  else
    return "\<Tab>"
  endif
endfunction
