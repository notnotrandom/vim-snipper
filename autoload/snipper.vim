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
let g:snipper#commentLinePattern = '\m^#'
let g:snipper#emptyLinePattern = '\m^$'

" Matches lines like 'snippet trigger "doc string"', with the doc string
" optional.
let g:snipper#snippetDeclarationLinePattern = '\m^snippet \zs\S\+\ze\(\s\|$\)'

let g:snipper#snippetLinePattern = '\m^\t\zs.*\ze'

let s:snippets = {}

let s:snippets_dir = fnameescape(expand(b:snipper_config.snippet_location))

" Variable for processing tabstops.
let s:partialSnipLen      = 0
let s:snippetInsertionPos = -1
let s:cursorStartPos      = -1
let s:nextTabStopNum      = 0

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
  if g:snipper_debug | echomsg "Entering snipper#ParseSnippetFile()" | endif
  if g:snipper_debug | echomsg "Argument 1: " . a:snipFile | endif

  let l:currentSnippetKey = ""
  let l:snippetLinesList = []

  for line in readfile(a:snipFile)
    if line =~ g:snipper#emptyLinePattern
      throw "EmptyLineOnSnippetFile"
    elseif line =~ g:snipper#commentLinePattern
      continue " Skip comments.
    endif

    let l:aux = matchstr(line, g:snipper#snippetDeclarationLinePattern)
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

      let l:aux = matchstrpos(line, g:snipper#snippetLinePattern)
      if ! (l:aux[1] == -1 && l:aux[2] == -1)
        " We found a line that starts with a <Tab>; i.e., it is part of the
        " expansion of a snippet. So add it to the list of expansion lines,
        " and continue onto to the next line.
        call add(l:snippetLinesList, l:aux[0])
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

" Taken from M. Sanders' snipMate, and modified.
function snipper#ProcessSnippet(snip)
	let snippet = a:snip
	" Evaluate eval (`...`) expressions.
	" Backquotes prefixed with a backslash "\" are ignored.
	" Using a loop here instead of a regex fixes a bug with nested "\=".
	if stridx(snippet, '`') != -1
		while match(snippet, '\(^\|[^\\]\)`.\{-}[^\\]`') != -1
			let snippet = substitute(snippet, '\(^\|[^\\]\)\zs`.\{-}[^\\]`\ze',
		                \ substitute(eval(matchstr(snippet, '\(^\|[^\\]\)`\zs.\{-}[^\\]\ze`')),
		                \ "\n\\%$", '', ''), '')
		endw
		let snippet = substitute(snippet, "\r", "\n", 'g')
		let snippet = substitute(snippet, '\\`', '`', 'g')
	endif

  " let snippet = substitute(snippet, '\([^\\]\)${1\(:\([^}]\+\)\)\?}', '\1\3', "g")
  let snippet = substitute(snippet, '\([^\\]\)$1', '\1', "g")

  " let snippet = substitute(snippet, '\([^\\]\)${2\(:[^}]\+\)\?}', '\1', "g")
  let snippet = substitute(snippet, '\([^\\]\)$2', '\1', "g")

	if &expandtab " Expand tabs to spaces if 'expandtab' is set.
		return substitute(snippet, '\t',
          \ repeat(' ', &softtabstop ? &softtabstop : &shiftwidth), 'g')
	endif
	return snippet
endfunction

function snipper#ProcessNthTabStop(snippet, num)
  " echom "|".a:snippet."|"
  let [ l:placeHolder, l:startIdx, l:endIdx ] =
        \ matchstrpos(a:snippet, '[^\\]${'.a:num.'\(:\zs[^}]\+\ze\)\?}')

  if l:placeHolder == "" && l:startIdx == -1 && l:endIdx == -1
    return []
  endif

  " XXX This slicing assumes a:num has just one digit!
  if l:placeHolder[-1:] != '}'
    " 5 is the len of "${1:" plus 1
    let l:snippet = a:snippet[ : l:startIdx - 5]  . l:placeHolder . a:snippet[l:endIdx + 1 : ]
    return [ l:snippet, l:startIdx - 4, strcharlen(l:placeHolder) ]
  else
    let l:snippet = a:snippet[ : l:startIdx ] . a:snippet[l:endIdx : ]
    " echom "|".l:snippet."|"
    return [ l:snippet, l:startIdx, 0 ]
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

  if s:nextTabStopNum != 0
    let l:line = getline(".") " Current line.
    let l:charCol = charcol(".") " cursor column (char-idx) when user hit <Tab> again.
    let l:snippetPartialEndPos =
          \ s:snippetInsertionPos + s:partialSnipLen - 1 + (l:charCol - s:cursorStartPos)
    let l:partiallyProcessedSnippet =
          \ slice(l:line, s:snippetInsertionPos - 1, l:snippetPartialEndPos - 1)
    echom "partial snip len |".s:partialSnipLen."|"
    echom "snip insert pos |".s:snippetInsertionPos."|"
    echom "charcol |".l:charCol."|"
    echom "cursor start pos |".s:cursorStartPos."|"
    echom "partial end pos|".l:snippetPartialEndPos."|"
    echom "|".l:partiallyProcessedSnippet."|"
    let l:res = snipper#ProcessNthTabStop(l:partiallyProcessedSnippet, s:nextTabStopNum)
    if l:res == []
      " There is no ${s:nextTabStopNum} tabstop, so we are done.
      let s:partialSnipLen      = 0
      let s:snippetInsertionPos = -1
      let s:cursorStartPos      = -1
      let s:nextTabStopNum      = 0
      return ""
    else
      let [ l:snip, l:idxForCursor, l:placeHolderLength ] = l:res
      " echom l:snip
      " echom "|".l:line[ s:snippetInsertionPos + (l:idxForCursor + l:placeHolderLength + 4) + 1 : ]."|"
      " echom "|".l:snip."|"
      if l:placeHolderLength == 0
        call setline(".", l:line[ : s:snippetInsertionPos - 1] . l:snip .
              \ l:line[ s:snippetInsertionPos + (l:idxForCursor + l:placeHolderLength + 4) + 1 : ])
      else
        call setline(".", l:line[ : s:snippetInsertionPos - 1] . l:snip .
              \ l:line[ s:snippetInsertionPos + (l:idxForCursor + l:placeHolderLength + 5) + 1 : ])
      endif
      call setcharpos('.', [0, line("."), s:snippetInsertionPos + l:idxForCursor + 1])

      if l:placeHolderLength == 0
        return ""
      else " l:placeHolderLength >= 1
        return "\<Esc>v" . (l:placeHolderLength - 1) . "l"
      endif
    endif
  endif

	let l:line = getline(".") " Current line.
  let l:currLineNum = line(".")
  " let l:indent = indent(l:currLineNum)

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
  endif

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
  " after the trigger text, if any. We do not to check that
  " l:triggerEndCharIdx + 1 is within the bounds of the array, because if it
  " isn't, empty is returned -- which is what we want.
  let l:afterTrigger = l:line[l:triggerEndCharIdx + 1 : ]

  if has_key(s:snippets, l:trigger) == v:true
    let l:triggerExpansion = snipper#ProcessSnippet(join(s:snippets[l:trigger], "\n"))
    let l:triggerProcessedList = split(l:triggerExpansion, "\n", 1)

    if len(l:triggerProcessedList) == 1
      let l:res = snipper#ProcessNthTabStop(l:triggerProcessedList[0], 1)
      if l:res == []
        " There is no ${1} tabstop, so we are done.
        call setline(".", l:beforeTrigger . l:triggerExpansion . l:afterTrigger)
        call setcharpos('.', [0, line("."), l:charCol + strcharlen(l:triggerExpansion) - l:triggerLength])
        return ""
      endif

      " Otherwise we process ${1}, and set s:nextTabStopNum to 2, which is the
      " next tabstop to be processed.
      let [ l:snip, l:idxForCursor, l:placeHolderLength ] = l:res
      call setline(".", l:beforeTrigger . l:snip . l:afterTrigger)
      call setcharpos('.', [0, line("."), l:charCol - l:triggerLength + l:idxForCursor + 1])

      let s:partialSnipLen = strcharlen(l:snip) - l:placeHolderLength
      let s:cursorStartPos = l:charCol - l:triggerLength + l:idxForCursor
      let s:snippetInsertionPos = l:charCol - l:triggerLength
      let s:nextTabStopNum      = 2

      if l:placeHolderLength == 0
        return ""
      else " l:placeHolderLength >= 1
        return "\<Esc>v" . (l:placeHolderLength - 1) . "l"
      endif
    else
      call setline(".", l:beforeTrigger . l:triggerProcessedList[0] . l:afterTrigger)
      let l:numOfInsertedLines = len(l:triggerProcessedList) - 1
      let l:indent = matchend(l:line, '^.\{-}\ze\(\S\|$\)')
      call append(l:currLineNum, map(l:triggerProcessedList[1:],
            \ "'".strpart(l:line, 0, indent)."'.v:val"))
      call setline(l:currLineNum + l:numOfInsertedLines,
            \ getline(l:currLineNum + l:numOfInsertedLines) . l:afterTrigger)
    endif
    return ''
  else
    return "\<Tab>"
  endif
endfunction
