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

" Self-explanatory.
let g:vim_snipper#commentLinePattern = '\m^#'
let g:vim_snipper#emptyLinePattern = '\m^$'

" Matches lines like 'snippet trigger "doc string"', with the doc string
" optional.
let g:vim_snipper#snippetDeclarationLinePattern = '\m^snippet \zs\S\+\ze\(\s\|$\)'

let g:vim_snipper#snippetLinePattern = '\m^\t\zs.*\ze'

let s:snippets = {}

let s:snippets_dir = fnameescape(expand("~/.vim/snippets"))

function snipper#BuildSnippetDict()
  " If .snippet files have been parsed before, then do not parse them again.
  if len(s:snippets) != 0
    return
  endif

  if filereadable(s:snippets_dir . "/" . "_.snippets")
    call snipper#ParseSnippetFile(s:snippets_dir . "/" . "_.snippets")
  endif

  if &filetype != "" && filereadable(s:snippets_dir . "/" . &filetype . ".snippets")
    call snipper#ParseSnippetFile(s:snippets_dir . "/" . &filetype . ".snippets")
  endif
endfunction

function snipper#ParseSnippetFile(snipFile)
  let l:currentSnippetKey = ""
  let l:snippetLinesList = []

  for line in readfile(a:snipFile)
    if line =~ g:vim_snipper#emptyLinePattern
      throw "EmptyLineOnSnippetFile"
    elseif line =~ g:vim_snipper#commentLinePattern
      continue " Skip comments.
    endif

    let l:aux = matchstr(line, g:vim_snipper#snippetDeclarationLinePattern)
    if l:aux != ""
      " We found a "snippet trigger" line. So first, check to see if we have
      " found that trigger before. If so, throw up error, has multiple snips
      " are not supported.
      if has_key(s:snippets, l:aux) == v:true
        throw "DuplicateSnippetKeyFound"
        return
      endif

      " Next, if we had previously found a trigger, then the new trigger marks
      " the end of the previous trigger's expansion.
      if l:currentSnippetKey != ""
        if len(l:snippetLinesList) > 0
          let s:snippets[l:currentSnippetKey] = l:snippetLinesList
        else
          " Control reaches when there is a previous trigger, but no expansion
          " for it. Hence, throw error.
          throw "FoundTriggerWithoutExpansion"
        endif
      endif

      " Finally, as we have found a new trigger, the array (List) where we
      " collect the expansion line(s) is reset to empty.
      let l:snippetLinesList = []
      " And the current snippet key takes the value of trigger we found with
      " the matchstr() above.
      let l:currentSnippetKey = l:aux
    else
      " We didn't find a line like "^snippet trigger ...", so look for other
      " possibilities...

      let l:aux = matchstr(line, g:vim_snipper#snippetLinePattern)
      if l:aux != ""
        " We found a line that starts with a <Tab>; i.e., it is part of the
        " expansion of a snippet. So add it to the list of expansion lines,
        " and continue onto to the next line.
        call add(l:snippetLinesList, l:aux)
        continue
      else
        " We found a line that is not a comment, is not a "snippet trigger"
        " line, and does not start with a <Tab>. So throw error.
        throw "InvalidLineFound"
      endif
    endif
  endfor

  " When we reach the end of the .snippet file, so check if there is any
  " pending trigger with body. If so, add them to the s:snippets dictionary.
  if l:currentSnippetKey != ""
    if len(l:snippetLinesList) > 0
      let s:snippets[l:currentSnippetKey] = l:snippetLinesList
    else
      " Control reaches when there is a previous trigger, but no expansion
      " for it. Hence, throw error.
      throw "FoundTriggerWithoutExpansion"
    endif
  endif
  
endfunction

function snipper#TriggerSnippet()
  if len(s:snippets) == 0
    try
      call snipper#BuildSnippetDict()
    catch /^DuplicateSnippetKeyFound$/
      echoerr "Duplicate snippet key found!"
    catch /^EmptyLineOnSnippetFile$/
      echoerr "Empty lines not allowed in snippet files!"
    catch /^FoundTriggerWithoutExpansion$/
      echoerr "Found trigger without expansion!"
    catch /^InvalidLineFound$/
      echoerr "Invalid snippet line found!"
    endtry
  endif

	let l:line = getline(".") " Current line.
  let l:indent = indent(line("."))

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

  if has_key(s:snippets, l:trigger) == v:true
    let l:triggerExpansionList = s:snippets[l:trigger]
    call setline(".", l:beforeTrigger . l:triggerExpansionList[0] . l:afterTrigger)
    call setcharpos('.', [0, line("."), l:charCol + strcharlen(l:triggerExpansionList[0]) - l:triggerLength])
    return ''
  else
    return "\<Tab>"
  endif
endfunction
