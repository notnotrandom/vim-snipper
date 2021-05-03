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

" Dictionary. Entries are like so: { trigger: snippet_text}. trigger contains
" the trigger, snippet_text contains the snippet just as read from the snippet
" file, including the ${1}, etc., but *without* the <Tab>'s at the start of
" the lines.
let s:snippets = {}

let s:snippets_dir = fnameescape(expand(b:snipper_config.snippet_location))

" Dictionary. Entries are like so:
" {trigger : {'snippet': processed_snippet_text, 'tabstops': list}}, where
" processed_snippet_text is the text of the snippet, stripped of tabstops,
" with placeholders taking their place, if applicable. tabstops has the same
" content that would go in s:tabStops, when processing the snippet triggered
" by trigger. See documentation of s:tabStops for more information.
let s:processedSnippets = {}

" Variables for processing tabstops.

" Each element with index i will correspond to tabstop i+1. Each element will
" be a list with three elements:
" - The char based index (so 0-based) of the position where the cursor must be
"   placed (position 0 is the start of the snippet). This is the position of
"   the $ character (of the i+1 tabstop).
" - The char length of the placeholder for tabstop i+1. It is 0 if there is no
"   placeholder.
" - A list of the INDEXES that are to the right of the current tabstop. This
"   is needed because after changing the current tabstop, the positions
"   relative to those tabstops further down the line very likely need to be
"   modified.
"     For example, consider a snippet like: "\emph{${1}}${2}". The list of
"   indexes for the first element (i.e. s:tabStops[0]) will be [1] (the 1
"   corresponds to tabstop 1+1 = 2, and indeed, ${2} is to the right of ${1}).
let s:tabStops = []

let s:partialSnipLen      = 0
let s:snippetInsertionPos = -1
let s:cursorStartPos      = -1
let s:nextTabStopNum      = 0
" End of variables for processing tabstops.

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

function snipper#FigureOutWhatToReturn(placeholder_length)
  " If the placeholder is empty, just return an empty string. If it is just
  " one char, then just "hit" v and go to select mode. Otherwise, as the
  " cursor will be left at the first char of the placeholder; to visually
  " select it, "hit" l, (a:placeholder_length - 1) times.
  "   (Doing v0^G would cause for the visual selection to go to the
  " start of the line...)
  if a:placeholder_length == 0
    return ""
  elseif a:placeholder_length == 1
    return "\<Esc>v"
  else " a:placeholder_length > 1
    return "\<Esc>v" . (a:placeholder_length - 1) . "l"
  endif
endfunction

" Brief: This function is called when the user decided to keep the
" placeholder's text inplace; i.e., with the placeholder visually selected, he
" hit <Tab>. Thus there is not much to do, except a simplified update of state
" information. Then, just call snipper#SetCursorPosBeforeReturning(), and
" snipper#FigureOutWhatToReturn(), and we are done.
function snipper#JumpToNextTabStop()
  if s:nextTabStopNum == 0
    if g:snipper_debug | echomsg "In function snipper#JumpToNextTabStop(): " .
          \ "caught s:nextTabStopNum = 0." | endif
    return ""
  endif

  " If the placeholder for the last tabstop is visually selected, and the user
  " hits <Tab>, then just return empty. (Recall that when this function is
  " called, the cursor is, in insert mode, just to the right of the
  " placeholder text. Cf. the mapping in after/plugin/vim-snipper.vim.)
  if s:nextTabStopNum > len(s:tabStops)
    call snipper#ClearState()
    return ""
  endif

  " Set up <Esc> and <C-c> maps, in case the user decides to terminate the
  " snippet processing.
  call snipper#SetTraps()

  " Grab the info the current tabstop (s:nextTabStopNum - 1 is the *index* for
  " the current tabstop).
  let [ l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
        \ s:tabStops[s:nextTabStopNum - 1]

  " Update the cursor start position for the current snippet.
  let s:cursorStartPos = s:snippetInsertionPos + l:idxForCursor

  " Before finishing, increment the tabstop number. (The only update of this
  " variable that does *not* increment it by one, is when it is 0 (in which
  " case it is incremented by 2). But if control reaches here, then the
  " variable is not 0 -- so its update is just += 1.)
  let s:nextTabStopNum += 1

  call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
  return snipper#FigureOutWhatToReturn(l:placeHolderLength)
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

  " When we reach the end of the .snippet file, check if there is any pending
  " trigger with body. If so, add them to the s:snippets dictionary.
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

" This function sets the s:tabStops variable with the information for the
" a:snip snippet.
function snipper#ProcessSnippet(snip)
  if s:tabStops != []
    " When this function is called, any previous processing of any snippets
    " should either have been completed, or be properly cleared.
    throw "InternalStateNotProperlyCleared"
  endif

  let l:snippet = a:snip
  " Evaluate eval (`...`) expressions. Backquotes prefixed with a backslash
  " "\" are ignored. Using a loop here instead of a regex fixes a bug with
  " nested "\=".
  if stridx(l:snippet, '`') != -1
    while match(l:snippet, '\(^\|[^\\]\)`.\{-}[^\\]`') != -1
      let snippet = substitute(l:snippet, '\(^\|[^\\]\)\zs`.\{-}[^\\]`\ze',
                             \ substitute(eval(matchstr(
                                               \ l:snippet, '\(^\|[^\\]\)`\zs.\{-}[^\\]\ze`')),
                                        \ "\n\\%$", '', ''), '')
    endwhile

    " Fix newlines.
    let l:snippet = substitute(l:snippet, "\r", "\n", 'g')

    " The above while loop dealt with embedded eval expressions. Now replace
    " escaped backticks with "plain" backticks -- as they will no longer be
    " processed, there is no need to have them escaped.
    let l:snippet = substitute(l:snippet, '\\`', '`', 'g')
  endif

  " Iterate over all tabstops in snippet. Replace each of them with either the
  " respective placeholder (if there is one), or with an empty string (if
  " there ain't). Also record relevant information about each tabstop in
  " s:tabStops.
  let l:tabStopNum = 1
  while 1

    " Find tabstop number l:tabStopNum. For the first one, the regexp matches
    " either ${1}, or ${1:placeholder}. For the former case, ${1}:
    " - the start byte index returned is that of the character before $, if
    "   there is one, or the byte index of $, if it is at the beginning of the
    "   snippet.
    " - the end byte index, is one plus the byte index of }.
    "
    " For the latter, ${1:placeholder}:
    " - the start byte index returned is that of the first character of the
    "   placeholder, in this example, it would be the index of p.
    " - the end byte index, is one plus the byte index of the last character
    "   of the placeholder text (in this example, one plus the byte index of
    "   r).
    let [ l:placeHolder, l:startIdx, l:endIdx ] =
          \ matchstrpos(l:snippet, '\([^\\]\)\?${'.l:tabStopNum.'\(:\zs[^}]\+\ze\)\?}')

    " If there was no match, break. This happens when we already went over all
    " tabstops in the snippet, but there might still positional information
    " that has to be stored in s:tabStops.
    if l:placeHolder == "" && l:startIdx == -1 && l:endIdx == -1
      break
    endif

    " The matchstrpos() function used above returns *byte* indexes; the
    " charidx() function converts them to char indexes. This ensures that the
    " code functions properly, even with non-ASCII characters, either in the
    " snippet's expansion, or in the text where it gets inserted.
    let l:startCharIdx = charidx(l:snippet, l:startIdx)
    let l:endCharIdx = charidx(l:snippet, l:endIdx)

    " As their names let slip, these variables will contain the char idx for
    " the current tabstop, and the length of its placeholder, if there one
    " (otherwise it is left at 0).
    let l:startCharIdxForCurrTabStop = 0
    let l:placeHolderLength = 0

    " If there is no placeholder -- i.e. we are dealing with tabstops like
    " ${1} -- then the last character of l:placeholder returned by the
    " matchstrpos() function above will be '}'. Otherwise, it will be some
    " other char.
    if l:placeHolder[-1:] != '}' " There is a placeholder.

      " Here we take the snippet and replace ${1:placeholder} with
      " "placeholder", sans quotes (replace with any other digit for other
      " tabstops). As there is a placeholder, variable l:startCharIdx contain
      " the char index of the first letter of the placeholder, in this case p.
      " This is the index of p in the entire snippet string. So first we want
      " to grab the substring that starts at 0, and goes up to and including
      " index l:startCharIdx - 5 (). The length of a string is one plus the
      " (0-based) index of the last element (e.g. for a string of length 3, if
      " the first element has index 0, then the last has index 2 -- and the
      " length 2 + 1 = 3). So the substring going from 0 to l:startCharIdx - 5
      " has length l:startCharIdx - 4 -- and it is this value that goes as the
      " second argument of the first strcharpart(). But this is also the value
      " of the char index of $ -- if we start at p, and go back 4 chars, we
      " are left on $ -- and so the relevant variable,
      " l:startCharIdxForCurrTabStop, is used as the strcharpart() second
      " argument.
      "   For the substring following the placeholder, recall that in this
      " example l:endCharIdx is one plus the char index of r, i.e., the index
      " of }. Hence we want the substring that starts at l:endCharIdx + 1, and
      " goes to the end of the snippet.
      let l:startCharIdxForCurrTabStop = l:startCharIdx - 4
      let l:snippet = strcharpart(l:snippet, 0, l:startCharIdxForCurrTabStop)
            \ . l:placeHolder . strcharpart(l:snippet, l:endCharIdx + 1 )

      let l:placeHolderLength = strcharlen(l:placeHolder)

    else " There is no placeholder.

      " In the matchstrpos() used above, when there is no placeholder, that
      " start index will be either that of $, if it is the first char of the
      " snippet, or that of the character before $, if it is not the first
      " character of the snippet. In this latter case, we bump l:startCharIdx
      " by one, to make it the char index of $.
      "   The strgetchar() function returns a *character code* (!), using the
      " current encoding. The function nr2char() converts it back to a
      " character, using utf-8 by default. As the $ character is a part of
      " ASCII, encondings do not cause problems.
      if nr2char(strgetchar(l:placeHolder, 0)) != '$'
        let l:startCharIdx += 1
      endif

      " Here we take the snippet and replace ${1} with an empty string, as
      " there is no placeholder (replace with any other digit for other
      " tabstops).
      "   So first, we want to grab the substring that goes from 0, up to and
      " including the char immediately before $. l:startCharIdx will point to
      " the char index of $, in the snippet string. So update
      " l:startCharIdxForCurrTabStop accordingly. But as char indexes start at
      " 0, l:startCharIdx is also the char length of the string going from 0
      " up to and including the char immediately to the left of $. So we use
      " that as the length argument of strcharpart(). (Also, if $ happens to
      " be the first char of the snippet string, then l:startCharIdx will be 0
      " -- but this is also the length of the string to be prepended.)
      "   Lastly, we want to append the substring going from the char
      " immediately to the right of }, up to the end of the string. Here there
      " is a caveat: l:endIdx is one plus the byte index of }. But if } is the
      " last char of the snippet string, this means that charidx() will return
      " -1 -- i.e., l:endCharIdx is -1. If that is the case, then there is
      "  nothing to append (else-clause below). Otherwise (if-clause), we use
      "  strcharpart(), starting from l:endCharIdx, and without giving it a
      "  length -- which means it will slurp all the remaining chars in the
      "  snippet string.
      let l:startCharIdxForCurrTabStop = l:startCharIdx
      if l:endCharIdx != -1
        let l:snippet = strcharpart(l:snippet, 0, l:startCharIdxForCurrTabStop) .
                      \ strcharpart(l:snippet, l:endCharIdx )
      else
        let l:snippet = strcharpart(l:snippet, 0, l:startCharIdxForCurrTabStop)
      endif
      " End replacing ${1} with empty string.
    endif

    " See the comments for s:tabStops, at the start of the file.
    call add(s:tabStops, [ l:startCharIdxForCurrTabStop,
                         \ l:placeHolderLength, [] ] )

    " Update previous tabstops that are to the right of the current one.
    " Because replacing ${d:foo} with foo, or ${d} with nothing, will change
    " the cursor position.
    for idx in range(l:tabStopNum - 1)
      if s:tabStops[idx][0] > l:startCharIdxForCurrTabStop
        if l:placeHolderLength == 0
          " Replacing ${d} with an empty string removes 4 characters.
          let s:tabStops[idx][0] -= 4
        else
          " Replacing ${d:foo} with foo remove 5 characters, and adds
          " len(foo) = 3 characters. The length of the placeholder is in var
          " l:placeHolderLength.
          let s:tabStops[idx][0] += (l:placeHolderLength - 5)
        endif
      endif
    endfor

    let l:tabStopNum += 1
    if l:tabStopNum == 10
      if g:snipper_debug | echomsg "Only 9 tabstops allowed: ${1} to ${9}." | endif
      throw "TooManyTabStops"
    endif
  endwhile

  " For all tabstops, add to s:tabStops the ones that are to the right of the
  " current one (in the same line). See comments of s:tabStops to see why this
  " is done.
  for idx in range(len(s:tabStops))
    let l:startCharIdxForCurrTabStop = s:tabStops[idx][0]
    echom "idx !".idx."! ".l:startCharIdxForCurrTabStop
    for jdx in range(len(s:tabStops))
      if s:tabStops[jdx][0] > l:startCharIdxForCurrTabStop
        echom "foo !".jdx
        call add(s:tabStops[idx][2], jdx) " Keep the INDEX, not tabstopnum, of tabstops in front of the current one.
      endif
    endfor
  endfor

  if &expandtab " Expand tabs to spaces if 'expandtab' is set.
    return substitute(l:snippet, '\t',
          \ repeat(' ', &softtabstop ? &softtabstop : &shiftwidth), 'g')
  endif
  return l:snippet
endfunction

" NOTA BENE: this function requires the value of s:cursorStartPos for the
" current snippet!!! I.e., UpdateState() must be called before calling this
" function!
function snipper#SetCursorPosBeforeReturning(placeholder_length)
  if a:placeholder_length == 0
    " If there is no placeholder, then just place the cursor at the start
    " position determined above, and be done with it.
    call setcharpos('.', [0, line("."), s:cursorStartPos])
  else " a:placeholder_length >= 1

    " If there is a placeholder, then the cursor is placed *after the
    " first char* of that placeholder. The reason for that is that, when there
    " is a placeholder, the function snipper#FigureOutWhatToReturn(), which
    " should be called after this one, will do an <Esc>v, which makes the
    " cursor go back one char. So for the cursor to be at s:cursorStartPos
    " after that <Esc>v, it needs to be placed at s:cursorStartPos + 1 before
    " that <Esc>v.
    call setcharpos('.', [0, line("."), s:cursorStartPos + 1])
  endif
endfunction

" Brief: Allow the processing of a snippet expansion to be interrupted, by
" hitting either <Esc>, or <Ctrl-c>. This function captures that event, and
" call snipper#ClearState(), to remove state information. This clears the way
" for a new snippet expansion to be processed.
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
    " If s:nextTabStopNum is not its default value (0), that means that either
    " ${s:nextTabStopNum - 1} or ${s:nextTabStopNum - 1:arg} exist, so now we
    " must see if there exists a tabstop for the following digit, i.e.,
    " s:nextTabStopNum.
    "   [len(s:tabStops) is the number (1-based) of the last tabstop in the
    " current snippet.]
    if s:nextTabStopNum > len(s:tabStops)
      " There is no ${s:nextTabStopNum} tabstop, so just (clear the state and)
      " return <Tab> (the user did press the <Tab> key, after all).
      call snipper#ClearState()
      return "\<Tab>"
    else
      " There is a ${s:nextTabStopNum} tabstop, so go process it. Control
      " arrives here because at ${s:nextTabStopNum - 1}, the user typed the
      " text he wanted, and then hit <Tab>. And that brought him to the
      " current tabstop, viz. {s:nextTabStopNum}.
      "   Start by obtaining the current line, and current cursor position.
      " From both of those, compute the char length of the text the user
      " entered, at the *previous* tabstop (${s:nextTabStopNum - 1}).
      "
      let l:line = getline(".") " Current line.
      let l:lineNum = line(".") " Current line.
      let l:charCol = charcol(".") " cursor column (char-idx) when user hit <Tab> again.
      let l:lengthOfUserText = strcharlen(slice(l:line, s:cursorStartPos, l:charCol))

      " (cont.) Obtain the record for that previous tabstop. (Recall that the
      " current tabstop has index s:nextTabStopNum - 1, so the index of the
      " previous one is s:nextTabStopNum - 2).
      let [ l:idxForCursor, l:placeHolderLength, l:idxsToUpdate ] =
            \ s:tabStops[s:nextTabStopNum - 2]

      " (cont.) From that previous information, compute the column of the last
      " character of the snippet, i.e., the snippet's end position. To see why
      " we need to subtract 1, imagine that the snippet is at the start of the
      " line, i.e., that s:snippetInsertionPos = 1 (variables with "Pos" in
      " their name refer to column positions). If s:partialSnipLen is, say, 3,
      " that means that the last char of the snippet is at column 3. But 1 + 3
      " = 4, so we need to subtract 1.
      let l:snippetEndPos = s:snippetInsertionPos + s:partialSnipLen - 1 +
            \ l:placeHolderLength + l:lengthOfUserText

      " So now we have the start and ending positions of the snippet, so we
      " can extract it from the current line. Recall that "Pos" variables
      " refer to column positions, but slice works with char indexes. So we
      " have to use s:snippetInsertionPos - 1 for start of the slice. For its
      " ending, the slice() function actually excludes the ending index (much
      " like Python), so we need one plus the ending index l:snippetEndPos -
      " 1, i.e., l:snippetEndPos.
      let l:snippet =  slice(l:line, s:snippetInsertionPos - 1, l:snippetEndPos)

      " Keep in mind that we are still working with the *previous* tabstop,
      " ${s:nextTabStopNum - 1} (which has index s:nextTabStopNum - 2 in
      " s:tabStops). Thus, l:idxsToUpdate contains the *indexes* (not tabstop
      " numbers) of all tabstops that are (in the same line) to the right of
      " ${s:nextTabStopNum - 1}. For those, we need to keep the cursor char
      " index accurate, adding to it the difference between the length of the
      " user inserted text, and the length of the placeholder.
      for idx in l:idxsToUpdate
        let s:tabStops[idx][0] += (l:lengthOfUserText - l:placeHolderLength)
      endfor

      " At this point, processing ${s:nextTabStopNum - 1} is done. So we set
      " the traps for the processing of the current tabsop, ${s:nextTabStopNum}.
      call snipper#SetTraps()

      " (cont.) And retrieve the information about ${s:nextTabStopNum} (which
      " has index s:nextTabStopNum - 1).
      let [ l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
            \ s:tabStops[s:nextTabStopNum - 1]

      " NOTA BENE: function snipper#UpdateState() increaments
      " s:nextTabStopNum, which is a part of the state that needs to be kept.
      call snipper#UpdateState(l:snippet, l:idxForCursor)

      call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
      return snipper#FigureOutWhatToReturn(l:placeHolderLength)
    endif
  endif

  let l:line = getline(".") " Current line.
  let l:currLineNum = line(".")

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
    " The snippet for l:trigger exists, so lets go on, erm, triggering it.

    " Check if we had processed this snippet before. If so, no point in doing
    " it all over again; just retrieve the previously store state information.
    " Otherwise, need to process it from scratch.

    let l:triggerExpansion = ""
    if has_key(s:processedSnippets, l:trigger) == v:true
      " We have seen this snippet before, so retrieve the needed information.
      let l:triggerExpansion = s:processedSnippets[l:trigger]['snippet']
      let s:tabStops = deepcopy(s:processedSnippets[l:trigger]['tabstops'])
    else " has_key(s:processedSnippets, l:trigger) == v:false
      " We have NOT seen this snippet before, so on we go with processing it.
      " The snipper#ProcessSnippet() function will set the s:tabStops
      " variable.
      let l:triggerExpansion = snipper#ProcessSnippet(join(s:snippets[l:trigger], "\n"))

      " Save the information resulting from the processing of the snippet,
      " to use if we have to expand the same snippet again in the future.
      let s:processedSnippets[l:trigger] = { 'snippet': l:triggerExpansion }
      let s:processedSnippets[l:trigger]['tabstops'] = deepcopy(s:tabStops)
    endif " Check s:processedSnippets dict for key l:trigger.

    let l:triggerProcessedList = split(l:triggerExpansion, "\n", 1)

    if len(l:triggerProcessedList) == 1
      if len(s:tabStops) == 0
        call setline(".", l:beforeTrigger . l:triggerExpansion . l:afterTrigger)
        " call snipper#SetCursorPosBeforeReturning()
        call setcharpos('.', [0, l:currLineNum,
                            \ l:charCol + strcharlen(l:triggerExpansion) - l:triggerLength])
        " There is no ${1} tabstop, so we are done.
        return ""
      endif

      " Otherwise we process ${1}, and set s:nextTabStopNum to 2, which is the
      " next tabstop to be processed.

      let l:snip = l:triggerExpansion

      " l:idxForCursor contains the array index (0-based) of the position of
      " the '$' in ${1:arg}, in the expanded snippet. l:placeHolderLength is
      " the (char) length of "arg".
      let [ l:idxForCursor, l:placeHolderLength ; l:subsequent ] = s:tabStops[0] " index one less than trigger number!

      " As the expansion is a one-liner, the text that goes before it is the
      " text that came before the trigger. Likewise for the text that goes
      " after it.
      call setline(".", l:beforeTrigger . l:snip . l:afterTrigger)

      " The column (char-based) of the cursor at the point the snippet was
      " inserted (e.g., if the snippet was inserted at the start of the line,
      " then s:snippetInsertionPos = 1).
      "   To see why it is computed in this way, imagine a line containing the
      " string "abc ", with the cursor after the ' ', in insert mode, when the
      " user types the trigger "xpto", and hits Tab. So the snippet insert
      " position will be after the "abc " string, and this is column position
      " 5. Now, after typing "xpto", the cursor will be at column 9 -- so this
      " will be the value of l:charCol. And the trigger length is 4. Hence,
      " the snippet insert position will be 9 - 4 = 5, as expected.
      "   NOTA BENE: l:charCol is computed with the function charcol(), and
      " the trigger has to be ASCII only, so this works even if the previous
      " text -- "abc " in the example above -- contains non-ASCII characters.
      let s:snippetInsertionPos = l:charCol - l:triggerLength

      " The s:snippetInsertionPos is only set once: after all, the snippet
      " is only inserted at one point. The remaining state information is set
      " in the UpdateState() function.
      call snipper#UpdateState(l:snip, l:idxForCursor)

      " Set the mappings to catch <Esc>, or <Ctrl-c>, and do the necessary
      " cleanup actions (e.g., clean the state variables, etc.).
      call snipper#SetTraps()

      call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
      return snipper#FigureOutWhatToReturn(l:placeHolderLength)

    else " ! len(l:triggerProcessedList) == 1
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
    endif " Checked if snippet has only one line, or more than one line.
    return "\<Esc>"
  else " If there is no snippet to expand, then just return the Tab key.
    return "\<Tab>"
  endif
endfunction

" Brief: Takes care of updating state variables, except s:snippetInsertionPos,
" the value of which is only set once (and hence, no needed for updates after
" that).
function snipper#UpdateState(snippet, idxForCursor)
  if s:nextTabStopNum == 0
    let s:nextTabStopNum = 2
  else
    " It should not be necessary here to check that s:nextTabStopNum only goes
    " up to 9, because function snipper#ProcessSnippet() already checks for
    " this.
    let s:nextTabStopNum += 1
  endif

  " Length of the (partially processed) snippet that was inserted by setline()
  " above. Includes the length of the placeholder text.
  let s:partialSnipLen = strcharlen(a:snippet)

  " The column (char-based) at which the cursor is placed after the processing
  " of the ${1:arg} tabstop. I.e., the column of the dollar sign.
  "   As explained in comments of the line where it is set, the variable
  " s:snippetInsertionPos contains the column at which the snippet text
  " starts. Now l:idxForCursor contains the char-based, 0-based, index of the
  " $ sign. For example, suppose you have a snippet like so (sans quotes):
  " "\label{${1:arg}}". Then, for the first tabstop, l:idxForCursor will be 7.
  " If this snippet is to be inserted in the first column, then
  " s:cursorStartPos is 8. If on the second column, it is 9, and so on. Hence
  " the cursor start position equals the snippet insert position, plus
  " l:idxForCursor.
  "   NOTA BENE: cursor positions, and string lengths are computed using
  " functions that count characters, not bytes (composite characters, e.g. ã,
  " or €, count as only character). Hence, this works even with snippets
  " containing non-ASCII characters.
  let s:cursorStartPos = s:snippetInsertionPos + a:idxForCursor
endfunction
