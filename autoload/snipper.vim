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
let s:tabStops = []
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

function snipper#ClearState()
  echom "mode: " . mode()
  let s:tabStops = []
  let s:partialSnipLen      = 0
  let s:snippetInsertionPos = -1
  let s:cursorStartPos      = -1
  let s:nextTabStopNum      = 0

  iunmap <buffer><expr> <Esc>
  sunmap <buffer><expr> <Esc>

  if mode() == 's'
    return "\<Esc>"
  else
    return "\<Esc>"
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
  if s:tabStops != []
    " When this function is called, any previous processing of any snippets
    " should either have been completed, or be properly cleared.
    throw "InternalStateNotProperlyCleared"
  endif

  let l:snippet = a:snip
  " Evaluate eval (`...`) expressions.
  " Backquotes prefixed with a backslash "\" are ignored.
  " Using a loop here instead of a regex fixes a bug with nested "\=".
  if stridx(l:snippet, '`') != -1
    while match(l:snippet, '\(^\|[^\\]\)`.\{-}[^\\]`') != -1
      let snippet = substitute(l:snippet, '\(^\|[^\\]\)\zs`.\{-}[^\\]`\ze',
                    \ substitute(eval(matchstr(l:snippet, '\(^\|[^\\]\)`\zs.\{-}[^\\]\ze`')),
                    \ "\n\\%$", '', ''), '')
    endwhile
    let l:snippet = substitute(l:snippet, "\r", "\n", 'g')
    let l:snippet = substitute(l:snippet, '\\`', '`', 'g')
  endif

  let l:tabStopNum = 1
  while 1
    let [ l:placeHolder, l:startIdx, l:endIdx ] =
          \ matchstrpos(l:snippet, '[^\\]${'.l:tabStopNum.'\(:\zs[^}]\+\ze\)\?}')

    if l:placeHolder == "" && l:startIdx == -1 && l:endIdx == -1
      break
    endif

    let l:startCharIdx = charidx(l:snippet, l:startIdx)
    let l:endCharIdx = charidx(l:snippet, l:endIdx)

    let l:startCharIdxForCurrTabStop = 0
    let l:placeHolderLength = 0
    if l:placeHolder[-1:] != '}'
      " echom "processSnippet with placeholder startIdx |".(l:startCharIdx -4)."|"
      " 5 is the len of "${1:" plus 1
      " let l:snippet = l:snippet[ : l:startIdx - 5]  . l:placeHolder . l:snippet[l:endIdx + 1 : ]
      " XXX This slicing assumes a:num has just one digit!
      let l:snippet = strcharpart(l:snippet, 0, l:startCharIdx - 5 + 1 ) . l:placeHolder .
                    \ strcharpart(l:snippet, l:endCharIdx + 1 )

      let l:startCharIdxForCurrTabStop = l:startCharIdx - 4
      let l:placeHolderLength = strcharlen(l:placeHolder)
    else " No placeholder.
      " let l:snippet = l:snippet[ : l:startIdx ] . l:snippet[l:endIdx : ]
      if l:endCharIdx != -1
        let l:snippet = strcharpart(l:snippet, 0, l:startCharIdx + 1 ) .
                      \ strcharpart(l:snippet, l:endCharIdx )
      else
        let l:snippet = strcharpart(l:snippet, 0, l:startCharIdx + 1 )
      endif
      " echom "processSnippet no placeholder snip to return |".l:snippet."|"
      " echom "processSnippet no placeholder endIdx |".l:endIdx."|"
      " echom "processSnippet no placeholder charidx endIdx |".charidx(l:snippet,l:endIdx)."|"
      let l:startCharIdxForCurrTabStop = l:startCharIdx + 1
    endif

    call add(s:tabStops, [ l:startCharIdxForCurrTabStop,
                         \ l:placeHolderLength, [] ] )

    for idx in range(l:tabStopNum - 1)
      if s:tabStops[idx][0] > l:startCharIdxForCurrTabStop
        call add(s:tabStops[l:tabStopNum - 1][2], idx) " Keep the INDEX, not tabstopnum, of tabstops in front of the current one.
        if l:placeHolderLength == 0
          let s:tabStops[idx][0] -= 4
        else
          let s:tabStops[idx][0] += (l:placeHolderLength - 5)
        endif
      endif
    endfor

    let l:tabStopNum += 1
    if l:tabStopNum == 10
      " XXX throw exception, for no more than 9 tabstops (1--9) are allowed.
      throw "TooManyTabStops"
    endif
  endwhile

  if &expandtab " Expand tabs to spaces if 'expandtab' is set.
    return substitute(l:snippet, '\t',
          \ repeat(' ', &softtabstop ? &softtabstop : &shiftwidth), 'g')
  endif
  return l:snippet
endfunction

function snipper#SetTraps()
  inoremap <buffer><expr> <Esc> snipper#ClearState()
  inoremap <buffer><expr> <C-c> snipper#ClearState()
  snoremap <buffer><expr> <Esc> snipper#ClearState()
  snoremap <buffer><expr> <C-c> snipper#ClearState()
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
    " len(s:tabStops) is the number (1-based) of the last tabstop.
    if s:nextTabStopNum > len(s:tabStops)
      " There is no ${s:nextTabStopNum} tabstop, so just return <Tab>
      " (after all, the user did press the <Tab> key).
      call snipper#ClearState()
      return "\<Tab>"
    else
      let l:line = getline(".") " Current line.
      let l:charCol = charcol(".") " cursor column (char-idx) when user hit <Tab> again.
      let l:lengthOfUserText = strcharlen(slice(l:line, s:cursorStartPos, l:charCol))
      let l:snippetEndPos =  s:snippetInsertionPos + s:partialSnipLen - 1 +
                           \ l:lengthOfUserText
      let l:snippet =  slice(l:line, s:snippetInsertionPos - 1, l:snippetEndPos)
      " echom "partial snip len |".s:partialSnipLen."|"
      " echom "snip insert pos |".s:snippetInsertionPos."|"
      echom "charcol |".l:charCol."|"
      echom "cursor start pos |".s:cursorStartPos."|"
      " echom "partial end pos|".l:snippetPartialEndPos."|"
      " echom "|".l:snippet."|"

      " let l:res = snipper#ProcessNthTabStop(l:partiallyProcessedSnippet, s:nextTabStopNum)

      let [ l:idxForCursor, l:placeHolderLength, l:idxsToUpdate ] =
            \ s:tabStops[s:nextTabStopNum - 1]
      " XXX deal with l:idxsToUpdate
      " echom l:snip
      " echom "|".l:line[ s:snippetInsertionPos + (l:idxForCursor + l:placeHolderLength + 4) + 1 : ]."|"
      " echom "idx cursor |".l:idxForCursor."|"
      " echom "snip |".l:snip."|"
      " echom "line prev to snip |".l:line[ : s:snippetInsertionPos - 2]."|"


      call snipper#SetTraps()

      echom "snip insert pos |".s:snippetInsertionPos."|"
      echom "idx cursor |".l:idxForCursor."|"
      echom "lengthOfUserText |".l:lengthOfUserText."|"
      echom "prev placeholder len |".s:tabStops[s:nextTabStopNum - 2][1]."|"
      echom s:tabStops[s:nextTabStopNum - 2]
      call setcharpos('.', [0, line("."),
                          \ s:snippetInsertionPos + l:idxForCursor + l:lengthOfUserText +
                          \ - s:tabStops[s:nextTabStopNum - 2][1]])

      " Prepare to process ${3}, if it exists. XXX
      let s:nextTabStopNum = 3

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

  " NOTA BENE: trigger must be ASCII only!
  let l:triggerLength = l:triggerEndCharIdx - l:prevCharIdx + 1
  let l:trigger = strpart(l:line, l:prevCharIdx, l:triggerLength)

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
      " let l:res = snipper#ProcessNthTabStop(l:triggerProcessedList[0], 1)
      if len(s:tabStops) == 0
        call setline(".", l:beforeTrigger . l:triggerExpansion . l:afterTrigger)
        call setcharpos('.', [0, line("."), l:charCol + strcharlen(l:triggerExpansion) - l:triggerLength])
        " There is no ${1} tabstop, so we are done.
        return ""
      endif

      " Otherwise we process ${1}, and set s:nextTabStopNum to 2, which is the
      " next tabstop to be processed.

      " l:res contains the expanded snippet, stripped of ${1:arg}, which was
      " replaced with arg, if any. l:idxForCursor contains the array index
      " (0-based) of the position of the '$' in ${1:arg}, in the expanded
      " snippet. l:placeHolderLength is the (char) length of "arg".
      let l:snip = l:triggerExpansion

      let [ l:idxForCursor, l:placeHolderLength ; l:subsequent ] = s:tabStops[0] " index one less than trigger number!

      " As the expansion is a one-liner, the text that goes before it is the
      " text that came before the trigger. Likewise for the text that goes
      " after it.
      call setline(".", l:beforeTrigger . l:snip . l:afterTrigger)

      " Save state information needed for processing ${2:arg}, if any.
      let s:nextTabStopNum = 2 " Number of next tabstop.

      " Length of the (partially processed) snippet that was inserted by
      " setline() above. The length of the current placeholder text is
      " subtracted, to allow, in the next iteration (i.e. after the user
      " having typed the text he wanted, and pressed Tab again), for the total
      " length of the snippet, plus the user inserted text, to be calculated.
      " This is done using the initial cursor position (basically, the
      " cursor's position when the user started to type); cf. s:cursorStartPos
      " below.
      let s:partialSnipLen = strcharlen(l:snip) - l:placeHolderLength

      " The column (char-based) of the cursor at the point the snippet was
      " inserted (e.g., if the snippet was inserted at the start of the line,
      " then s:snippetInsertionPos = 1).
      "   To see why it is computed in this way, imagine a line containing the
      " string "abc ", and cursor is after the ' ', in insert mode, when the
      " user types the trigger "xpto", and hits Tab. So the snippet insert
      " position will be after the "abc " string, and this is column position
      " 5. Now, after typing "xpto", the cursor will be at column 9 -- so this
      " will be the value of l:charCol. And the trigger length is 4. Hence,
      " the snippet insert position will be 9 - 4 = 5, as expected.
      "   NOTA BENE: l:charCol is computed with the function charcol(), and
      " the trigger must be ASCII only, so this works even if the previous
      " text -- "abc " in the example above -- contains non-ASCII characters.
      let s:snippetInsertionPos = l:charCol - l:triggerLength

      " The column (char-based) at which the cursor is placed after the
      " processing of the ${1:arg} tabstop. I.e., the column of the dollar
      " sign.
      "   As explained above, s:snippetInsertionPos contains the column at
      " which the snippet text starts. Now
      let s:cursorStartPos = s:snippetInsertionPos + l:idxForCursor
      " End of saving state information needed for processing the next
      " tabstop.

      " echom "snip insert pos |".s:snippetInsertionPos."|"
      " echom "snip idx for cur |".l:idxForCursor."|"
      " echom "cursor start pos |".s:cursorStartPos."|"

      " Set the mappings to catch <Esc>, or <Ctrl-c>, and do the necessary
      " cleanup actions (e.g., clean the state variables, etc.).
      call snipper#SetTraps()

      echom "placeHolderLength |".l:placeHolderLength."|"
      if l:placeHolderLength == 0
        call setcharpos('.', [0, line("."), s:cursorStartPos])
        return ""
      else " l:placeHolderLength >= 1
        " If there is a placeholder, then the cursor is placed *after the
        " first char* of that placeholder, for <Esc>v to function properly.

        call setcharpos('.', [0, line("."), s:cursorStartPos + 1])
        " call setcharpos('.', [0, line("."), l:charCol - l:triggerLength + l:idxForCursor + 1])

        " If the placeholder is just one char, then just "hit" v and go to
        " select mode. Otherwise, as the cursor will be left at the first char
        " of the placeholder, to visually select it, "hit" l,
        " l:placeHolderLength - 1 times.
        " (Doing v0^G would cause for the visual selection to go to the start
        " of the line...)
        if l:placeHolderLength == 1
          return "\<Esc>v"
        else " l:placeHolderLength > 1
          return "\<Esc>v" . (l:placeHolderLength - 1) . "l"
        endif
      endif
    else
      " If the expansion has more than one line, for the time being, we just
      " strip it of any tabstops, and place it verbatim in the file. This is
      " done respecting the existing indentation level.
      let l:triggerExpansion = substitute(l:triggerExpansion,
            \ '\([^\\]\)${\d\(:\([^}]\+\)\)\?}', '\1', "g")
      let l:triggerProcessedList = split(l:triggerExpansion, "\n", 1)

      call setline(".", l:beforeTrigger . l:triggerProcessedList[0] . l:afterTrigger)
      let l:numOfInsertedLines = len(l:triggerProcessedList) - 1
      let l:indent = matchend(l:line, '^.\{-}\ze\(\S\|$\)')
      call append(l:currLineNum, map(l:triggerProcessedList[1:],
            \ "'".strpart(l:line, 0, indent)."'.v:val"))
      call setline(l:currLineNum + l:numOfInsertedLines,
            \ getline(l:currLineNum + l:numOfInsertedLines) . l:afterTrigger)
    endif
    return ''
  else " If there is no snippet to expand, then just return the Tab key.
    return "\<Tab>"
  endif
endfunction
