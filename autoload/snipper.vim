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

let s:charActuallyInserted = v:false

" This variable is here to fix the following problem: when typing his text in
" a given tabstop, the user might use Vim's completion feature. This causes
" the problem of how to detect that completion has finished, because while
" there is events for that, if the user chooses his completion, and this hits
" <Tab> (to go to the next tabstop), the events signalling completion will be
" triggered too late -- namely, when the user is already on the next tabstop.
" This is no good.
"   In order to fix this, I map the <Tab> key in insert mode, to <Space><BS>,
" before calling the function that moves the processing of the snippet onto
" the next tabstop (cf. after/ftplugin/vim-snipper.vim). The problem is this:
" the <Space> triggers the event InsertCharPre, which screws up the
" processing! (This does NOT happen on the first time the user
" hits <Tab> (i.e., when the snippet is expanded), because the autocmd for
" that has not yet been set -- but this causes no problems). It is to remedy
" this problem that this variable is here for.
"   After the user has finished typing his text at some tabstop, he will hit
" <Tab> go to the next (or the previous) tabstop. If the destination tabstop
" has a placeholder, then there is nothing to update, until the user starts
" typing (see below XXX). But if there is no placeholder, then that means that
" the motion of the cursor, from the old tabstop to the new one, will be done
" in insert mode -- which triggers the function snipper#UpdateSnippet().
let s:compensatedForHiddenBS = v:none

" Self-explanatory.
let g:snipper#commentLinePattern = '\m^#'
let g:snipper#emptyLinePattern = '\m^$'

" Matches lines like 'snippet trigger "doc string"', with the doc string
" optional.
let g:snipper#snippetDeclarationLinePattern = '\m^snippet \zs\S\+\ze\(\s\|$\)'

let g:snipper#snippetLinePattern = '\m^\t\zs.*\ze$'

" For LaTeX, inter alia...
let g:snipper#triggerBoundaries = "$()[]{}<>*\"'"

let s:lenghtOfLineWhereCursorWent = 0 " very ugly hack... :-(

" snipper#UpdateSnippet() gets called one time too many, when control enters
" insert mode, but nothing is written (yet) by the user.
let s:numCharsInsertedCurrTabstop = 0

" Key for dict s:passiveTabStops; to be used with nr2char().
let s:passiveTabStopsStartNumHashKey = 97

" Value to use as the special "file type" for global snippets. See the
" comments for g:snipper#snippets, below.
let s:global_key = "global_1234"

" Dictionary. Entries are like so: {filetype: { trigger: snippet_text}}. That
" is, g:snipper#snippets is an hash table, whose keys are the different
" filetypes, and values are themselves hash tables, containing the triggers as
" keys, and the snippet text as values. (The global snippets are stored under
" the special key s:global_key.)
"
" snippet_text contains the snippet just as read from the snippet file,
" including the ${1}, etc., but *without* the <Tab>'s at the start of the
" lines.
let g:snipper#snippets = {}

let g:snipper#snippets_dir = fnameescape(expand(g:snipper_config.snippet_location))

" Dictionary. Entries are like so:
"
" {filetype: {trigger : {'snippet': processed_snippet_text, 'tabstops': list}}}
"
" That is, g:snipper#ProcessedSnippets is an hash table, with the keys being
" the filetypes (this is similar to g:snipper#snippets; cf. its comments).
"
" The values are themselves dictionaries, with two keys, viz., "snippet" and
" "tabstops" (these are strings, not variables). The processed_snippet_text
" variable is the text of the snippet, stripped of tabstops, with placeholders
" taking their place, if applicable. The list variable has the same content
" that would go in s:tabStops, when processing the snippet triggered by
" trigger. See documentation of s:tabStops for more information.
let g:snipper#ProcessedSnippets = {}

" For the trigger search functionality.
let s:filetype = &filetype
let s:triggerList = []

" --- Variables for processing tabstops. ---

" For the tabstop currently being processed, the column position of the '$'
" sign. 
let s:cursorStartPos = -1

" List of Dicts. Index 0 contains a Dictionary { lineIdx => [ a, b, c] } for
" tabstop ${1}. a, b, and c are all the passive tabstops that exist in the
" snippet line with index lineIdx (so the first line is 0, 1 for the second
" line, and so on). The passive tabstops are also sorted by ascending index of
" appearance: in the above example, a appears to the left of b, and b in turn
" appears to the left of c.
"
" Whether or not there are passive tabstops, this contains a (possibly empty)
" Dict for each regular tabstop (i.e. ${1}, ${2}, etc.).
let s:groupedPassiveTabStopsList = []

" Should be self-explanatory.
let s:nextTabStopNum = 0

" Dictionary, containing ALL the passive tabstops -- i.e., things like $1,
" which get the contents of the active tabstop ${1} -- numbered from a to z,
" by order of appearance (left to right, top to bottom). These, letters from a
" to z, are the keys of the hashtables. For each key, its value is an array,
" containing, the same parameters as the elements of s:tabStops, except for
" placeholder length. (So s:tabStops parameters with indexes 3 and 4, here are
" 2 and 3 respectively.)
"
" If there are no passive tabstops, this is empty.
let s:passiveTabStops = {}

" Line number where the trigger was entered. It will contain the first line of
" the snippet.
let s:snippetInsertionLineNum = -1

" Column number (char based), where the trigger started. Will be the start of
" the first line of the snippet.
let s:snippetInsertionPos = -1

" Index (0-based) of the snippet line currently being processed. So the first
" line is line 0, the second line 1, etc.
let s:snippetLineIdx = -1

" Each element with index i will correspond to tabstop i+1. Each element will
" itself be a list with three elements:
" - 0: The index (so 0-based) of the snippet line where the cursor must be
"   placed (index 0 is the first line of the snippet).
" - 1: The char based index (so 0-based) of the position where the cursor must
"   be placed (position 0 is the start of the snippet). This is the position
"   of the $ character (of the i+1 tabstop).
" - 2: The char length of the placeholder for tabstop i+1. It is 0 if there is
"   no placeholder.
" - 3: A list of the INDEXES that are to the right of the current tabstop.
"   This is needed because after changing the current tabstop, the positions
"   relative to those tabstops further down the line very likely need to be
"   modified.
"     For example, consider a snippet like: "\emph{${1}}${2}". The list of
"   indexes for the first element (i.e. s:tabStops[0]) will be [1] (the 1
"   corresponds to tabstop 1+1 = 2, and indeed, ${2} is to the right of ${1}).
" - 4: INDEXES of passive tabstops.
let s:tabStops = []

" --- End of variables for processing tabstops. ---

" This function is called, inter alia, when changing (e.g., :edit'ing) to a
" file with a different filetype.
function snipper#BuildSnippetDict()
  " First, check if we have read global snippets. If we haven't, then, parse
  " them now.
  if has_key(g:snipper#snippets, s:global_key) == v:false &&
        \ filereadable(g:snipper#snippets_dir . "/" . "_.snippets") == v:true
    call snipper#ParseSnippetFile(
          \ g:snipper#snippets_dir . "/" . "_.snippets", s:global_key)
  endif

  " And second, if the current file has a file type set, and we have not read
  " snippets for that file type yet, and the relevant snippet file is
  " readable, then parse that snippet file.
  if &filetype != "" && has_key(g:snipper#snippets, &filetype) == v:false &&
        \ filereadable(g:snipper#snippets_dir . "/" . &filetype . ".snippets")
    call snipper#ParseSnippetFile(
          \ g:snipper#snippets_dir . "/" . &filetype . ".snippets", &filetype)

  endif
endfunction

" Brief: Triggered via an autocmd for the InsertCharPre event. This used
" because function snipper#UpdateSnippet() is triggered when the cursor moves
" in insert mode, which also happens when the cursor goes to a tabstop for
" which there is no place holder, EVEN BEFORE THE USER TYPES ANYTHING (cf. the
" comments in that function). Thus, again in that function, it is necessary to
" know if it was called because an actual character was inserted or not (to
" avoid making spurious updates). This is the purpose of this function.
function snipper#CharActuallyInserted()
  " if g:snipper_debug | echomsg "In function snipper#CharActuallyInserted()."
        " \ " Character inserted: -->" . v:char . "<--" | endif
  echomsg "In function snipper#CharActuallyInserted()."
        \ " Character inserted: -->" . v:char . "<--"
  " if s:compensatedForHiddenBS == v:false
  "   echom "ts " . (s:nextTabStopNum - 1) . " compensating bs..."
  "   let s:compensatedForHiddenBS = v:true
  "   return
  " endif
  let s:charActuallyInserted = v:true
endfunction

function snipper#CheckNeedToBuildSnippetDict()
  if len(g:snipper#snippets) == 0 ||
        \ ( &filetype != "" && has_key(g:snipper#snippets, &filetype) == v:false )
    try
      call snipper#BuildSnippetDict()
      return v:true
    catch /^DuplicateSnippetKeyFound$/
      echoerr "Duplicate snippet key found!"
    catch /^EmptyLineOnSnippetFile$/
      echoerr "Empty lines not allowed in snippet files!"
    catch /^FoundTriggerWithoutExpansion$/
      echoerr "Found trigger without expansion!"
    catch /^InvalidLineFound$/
      echoerr "Invalid snippet line found!"
    catch /^RepeatedParsingOfFiletype$/
      echoerr "Repeated parsing of same file type!"
    endtry
    return v:false
  endif

  return v:true
endfunction

" Brief: If the current file has no file type set, then any trigger for it
" must be under the s:global_key sub-hash in g:snipper#snippets (cf. this
" latter variable's comments, near the top of this file). But if there *is* a
" file type set, then any trigger can be either under the s:global_key
" sub-hash, or under the &filetype sub-hash. The same holds for the
" g:snipper#ProcessedSnippets hash table.
"   This function returns the hash key for the current snippet trigger (either
" global or the filetype), for both g:snipper#snippets and
" g:snipper#ProcessedSnippets. If the trigger does not exist, then return
" v:false.
function snipper#CheckSnippetExists(trigger)
  if has_key(g:snipper#snippets, s:global_key) == v:true &&
        \ has_key(g:snipper#snippets[s:global_key], a:trigger) == v:true
    if g:snipper_debug | echomsg "In function snipper#CheckSnippetExists(): " .
          \ "Returning global key." | endif
    return s:global_key
  endif

  if &filetype != "" && has_key(g:snipper#snippets, &filetype) == v:true &&
        \ has_key(g:snipper#snippets[&filetype], a:trigger) == v:true
    if g:snipper_debug | echomsg "In function snipper#CheckSnippetExists(): " .
          \ "Returning filetype ". &filetype . "." | endif
    return "" . &filetype
  endif
  return v:false
endfunction

" Brief: This function is called when the user decides to interrupt the
" processing of a snippet. He can do this either by hitting <Esc>, or
" <Ctrl-c>, in which case this function is called (the mappings for this are
" in function snipper#SetTraps()).
"
" Synopsis: This functions clears state variables (i.e., sets them to their
" default value), and unmaps the maps set up in snipper#SetTraps().
"   Lastly, the user can decide to halt snippet processing when the mode is
" either insert mode, or select mode. In former case, we just need to return
" <Esc>, but in the latter, we must first exit select mode, by hitting ^G, and
" only then return <Esc>. This is what the final if-else condition in this
" function tests for.
"
function snipper#ClearState()
  if g:snipper_debug |echomsg "In function snipper#ClearState()... " | endif

  let s:compensatedForHiddenBS      = v:false
  let s:curCol                      = -1
  let s:cursorStartPos              = -1
  let s:groupedPassiveTabStopsList  = []
  let s:nextTabStopNum              = 0
  let s:numCharsInsertedCurrTabstop = 0
  let s:passiveTabStops             = {}
  let s:snippetInsertionPos         = -1
  let s:snippetInsertionLineNum     = -1
  let s:snippetLineIdx              = -1
  let s:tabStops                    = []

  " These maps are set in RemovePlaceholders(), which runs only when entering
  " insert mode, for an existing tabstop. Hence they are not set the last time
  " the user hits <Tab> -- but this function has to be called just the same
  " (to clear the rest of the state). Hence, silently ignore the error that
  " Vim throws when trying to unmap an unexisting map...
  " silent! iunmap <buffer><expr> <Esc>
  silent! iunmap <buffer><expr> <C-c>

  " sunmap <buffer><expr> <Esc>
  sunmap <buffer><expr> <C-c>

  " See snipper#SetTraps() for the explanation for the mapping that this line
  " clears.
  sunmap <buffer> <BS>

  if exists("#vimSnipperAutocmds")
    autocmd! vimSnipperAutocmds
  endif

  if mode() == 's'
    return "\<Esc>"
  else
    return "\<Esc>"
  endif
endfunction

function snipper#ClearStateCc()
  call snipper#ClearState()
  echohl WarningMsg | echo  "vim-snipper: Interrupted by user (C-c). Bye..." | echohl None
  return
endfunction

function snipper#FigureOutWhatToReturn(placeholder_length)
  if g:snipper_debug | echomsg "In function snipper#FigureOutWhatToReturn()... " | endif
  if g:snipper_debug | echomsg "Argument a:placeholder_length: " . a:placeholder_length | endif

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

  " The user just hit <Tab>, so disable all autocmd's, if any.
  if exists("#vimSnipperAutocmds")
    autocmd! vimSnipperAutocmds
  endif

  " If the placeholder for the last tabstop is visually selected, and the user
  " hits <Tab>, then just return empty. (Recall that when this function is
  " called, the cursor is, in insert mode, just to the right of the
  " placeholder text. Cf. the mapping in after/plugin/vim-snipper.vim.)
  if s:nextTabStopNum > len(s:tabStops)
    call snipper#ClearState()
    return ""
  endif

  " Otherwise, we would start by clearing autocmd's, if any. (In the above
  " case this is done by ClearState().) However, below the autocmds will be
  " reset for the next tabstop, clearing all previous autocmds, if any. Hence,
  " we skip this step of explicitly clearing the autocmds. (The user will only
  " be able to type after this function has finished, by which time the
  " autocmds will be properly set.)

  " Set the traps for the processing of the current tabsop,
  " ${s:nextTabStopNum}.
  call snipper#SetTraps()

  " Grab the info the tabstop we just jumped to (its *index* is
  " s:nextTabStopNum - 1).
  let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
        \ s:tabStops[s:nextTabStopNum - 1]

  " Update the cursor start line and column position for the current snippet.
  let s:snippetLineIdx = l:idxForLine
  let s:cursorStartPos = s:snippetInsertionPos + l:idxForCursor

  " Before finishing, increment the tabstop number. (The only update of this
  " variable that does *not* increment it by one, is when it is 0 (in which
  " case it is incremented by 2). But if control reaches here, then the
  " variable is not 0 -- so its update is just += 1.)
  let s:nextTabStopNum += 1

  " See snipper#UpdateState() for the role of this function.
  let s:numCharsInsertedCurrTabstop = 0

  let s:compensatedForHiddenBS = v:false

  " To understand why this is here, and in particular, why are the autocmds
  " set after incrementing s:nextTabStopNum, see the comments at the autocmd
  " in snipper#UpdateState().
  augroup vimSnipperAutocmds
    autocmd!
    autocmd InsertCharPre * call snipper#CharActuallyInserted()
    autocmd InsertEnter * call snipper#RemovePlaceholders(s:nextTabStopNum - 1)
    autocmd CursorMovedI,TextChangedP * call snipper#UpdateSnippet(s:nextTabStopNum - 1)
    autocmd CompleteDone * call snipper#CompletionDone(s:nextTabStopNum - 1)
  augroup END

  call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
  return snipper#FigureOutWhatToReturn(l:placeHolderLength)
endfunction

" Param: a:comingFromInsertMode is 1 if insert mode map, 0 otherwise.
function snipper#JumpToPreviousTabStop(comingFromInsertMode)
  if s:nextTabStopNum == 0
    if g:snipper_debug | echomsg "In function snipper#JumpToPreviousTabStop(): " .
          \ "caught s:nextTabStopNum = 0." | endif
    return ""
  endif

  " The user just hit <Tab>, so disable all autocmd's, if any.
  if exists("#vimSnipperAutocmds")
    autocmd! vimSnipperAutocmds
  endif

  if s:nextTabStopNum < 3
    " If s:nextTabStopNum = 2, then the user is currently at tabstop number 1.
    " If he wants to go back even further, we just leave him at tabstop 1
    " again. We do this by simulating he is at tabstop 2 (and so,
    " s:nextTabStopNum = 3) -- and let the remainder of this function carry
    " him "back" to tabstop 1.
    let s:nextTabStopNum = 3
  endif

  " If we want to make the tabstop before the one the user is currently on,
  " the current one, then the next tabstop will be the tabstop the user is
  " currently on.
  let s:nextTabStopNum -= 1

  " Set up <Esc> and <C-c> maps, in case the user decides to terminate the
  " snippet processing.
  call snipper#SetTraps(1)

  if a:comingFromInsertMode == 1
    " If this function is being called from insert mode, then the text for the
    " tabstop the user was just at, s:nextTabStopNum, may have been modified.
    " So I take the text the user inserted (which may have length 0, i.e., no
    " text), and treat it as a place holder for that tabstop. In particular,
    " if the user goes back to said tabstop, that inserted text will be shown
    " selected (if no text was inserted, then just leave the user in insert
    " mode, as done for tabstops without a placeholder).
    "   Recall that here s:nextTabStopNum is at least 2.
    let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
          \ s:tabStops[s:nextTabStopNum - 1]

    " Length of the text the user entered at the tabstop he was just at,
    " before this function being called.
    let l:line = getline(s:snippetInsertionLineNum + l:idxForLine)
    let l:charShift = charcol(".") - (s:snippetInsertionPos + l:idxForCursor)
    let s:tabStops[s:nextTabStopNum - 1][2] = l:charShift

    " We also reset the "starting point" for the passive deps of same tabstop
    " (the one the was just at before this function being called). This is
    " needed because for passive deps, the starting point is increased by 1,
    " for each typed character (cf. snipper#UpdateSnippet()). But here, we
    " need to make that starting point go back, as many chars as the ones the
    " user inserted -- i.e., as many chars as the place holder has.
    for l:key in keys(s:groupedPassiveTabStopsList[s:nextTabStopNum - 1])
      let l:pTabStopsInCurrLine_l =
            \ s:groupedPassiveTabStopsList[s:nextTabStopNum - 1][l:key]

      for idx in range(len(l:pTabStopsInCurrLine_l))
        let s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][1] -= l:charShift
      endfor
    endfor

    " let s:compensatedForHiddenBS = v:false
  else
    " Coming from select mode, so <Space><BS> inserted before calling
    " function...
    " let s:compensatedForHiddenBS = v:true
  endif " if a:comingFromInsertMode == 1

  " See snipper#UpdateState() for the role of this function.
  let s:numCharsInsertedCurrTabstop = 0

  let s:compensatedForHiddenBS = v:false

  augroup vimSnipperAutocmds
    autocmd!
    autocmd InsertCharPre * call snipper#CharActuallyInserted()
    autocmd InsertEnter * call snipper#RemovePlaceholders(s:nextTabStopNum - 1)
    autocmd CursorMovedI,TextChangedP * call snipper#UpdateSnippet(s:nextTabStopNum - 1)
    autocmd CompleteDone * call snipper#CompletionDone(s:nextTabStopNum - 1)
  augroup END

  " Finally, set up the cursor position for the tabstop where the users to go
  " to -- which has index s:nextTabStopNum - 2.
  let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
        \ s:tabStops[s:nextTabStopNum - 2]
  let s:snippetLineIdx = l:idxForLine
  let s:cursorStartPos = s:snippetInsertionPos + l:idxForCursor

  call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
  return snipper#FigureOutWhatToReturn(l:placeHolderLength)
endfunction

function snipper#ParseSnippetFile(snipFile, filetype)
  if g:snipper_debug | echomsg "Entering snipper#ParseSnippetFile()" | endif
  if g:snipper_debug | echomsg "Argument 1: " . a:snipFile | endif
  if g:snipper_debug | echomsg "Argument 2: " . a:filetype | endif

  if has_key(g:snipper#snippets, a:filetype) == v:true
    " We have parsed the snippets file for this particular filetype, so this
    " function should not have been called (cf. snipper#BuildSnippetDict()).
    " Hence, fire away exception.
    throw "RepeatedParsingOfFiletype"
  endif

  " Otherwise, add sub-hash for a:filetype.
  let g:snipper#snippets[a:filetype] = {}

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
      " found that trigger before. If so, throw up error, as multiple snips
      " are not supported.
      if has_key(g:snipper#snippets[a:filetype], l:aux) == v:true
        throw "DuplicateSnippetKeyFound: " . l:aux
        return
      endif

      " Next, if we had previously found a trigger, then the new trigger marks
      " the end of the previous trigger's expansion.
      if l:currentSnippetKey != ""
        if len(l:snippetLinesList) > 0
          let g:snipper#snippets[a:filetype][l:currentSnippetKey] = l:snippetLinesList
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
  " trigger with body. If so, add them to the g:snipper#snippets dictionary.
  if l:currentSnippetKey != ""
    if len(l:snippetLinesList) > 0
      let g:snipper#snippets[a:filetype][l:currentSnippetKey] = l:snippetLinesList
    else
      " Control reaches when there is a previous trigger, but no expansion
      " for it. Hence, throw error.
      throw "FoundTriggerWithoutExpansion"
    endif
  endif
endfunction

" Brief: This function sets the s:tabStops variable with the information for
" the a:snip snippet. It also expands the snippet (see Return: below).
"
" Return: The snippet a:snip, as a string, with both ${1}, ${2}, etc., and $1,
" $2, etc., replaced with their respective placeholders (or an empty string,
" for tabstops without a placeholder).
"
" Parameters: a:snip is a list, containing the lines of the snippet expansion.
function snipper#ProcessSnippet(snip)
  if s:tabStops != []
    " When this function is called, any previous processing of any snippets
    " should either have been completed, or be properly cleared.
    throw "InternalStateNotProperlyCleared"
  endif

  let l:snippet = join(a:snip, "\n")

  if &expandtab " Expand tabs to spaces if 'expandtab' is set.
    let l:snippet = substitute(l:snippet, '\t',
          \ repeat(' ', &softtabstop ? &softtabstop : &shiftwidth), 'g')
  endif

  " Evaluate eval (`...`) expressions. Backquotes prefixed with a backslash
  " "\" are ignored. Using a loop here instead of a regex fixes a bug with
  " nested "\=".
  if stridx(l:snippet, '`') != -1
    while match(l:snippet, '\(^\|[^\\]\)`.\{-}[^\\]`') != -1
      let l:snippet = substitute(l:snippet, '\(^\|[^\\]\)\zs`.\{-}[^\\]`\ze',
                             \ substitute(eval(matchstr(
                                               \ l:snippet, '\(^\|[^\\]\)`\zs.\{-}[^\\]\ze`')),
                                        \ "\n\\%$", '', ''), '')
    endwhile

    " The above while loop dealt with embedded eval expressions. Now replace
    " escaped backticks with "plain" backticks -- as they will no longer be
    " processed, there is no need to have them escaped.
    let l:snippet = substitute(l:snippet, '\\`', '`', 'g')
  endif

  let l:snippetLineList = split(l:snippet, "\n", 1)
  let l:placeHolders = []

  " Iterate over all tabstops in snippet. Replace each of them with either the
  " respective placeholder (if there is one), or with an empty string (if
  " there ain't). Also record relevant information about each tabstop in
  " s:tabStops.
  let l:tabStopNum = 1

  while 1

    " Find tabstop number l:tabStopNum. E.g., for the first one, the regexp
    " matches either ${1}, or ${1:placeholder}. For the former case, ${1}:
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
    "
    " As we need the line number of the tabstop, what we actually do is to
    " iterate over its lines, until there is a match. ("Line number" for the
    " tabstop is within the snippet; e.g., if it is on the first snippet line,
    " or the second, or so on...)
    let [ l:placeHolder, l:startIdx, l:endIdx ] = [ "", -1, -1 ]
    let l:snippetCurrLineIdx = 0

    for idx in range(len(l:snippetLineList))
      let l:line = l:snippetLineList[idx]
      let [ l:placeHolder, l:startIdx, l:endIdx ] =
            \ matchstrpos(l:line, '\([^\\]\)\?${'.l:tabStopNum.'\(:\zs[^}]\+\ze\)\?}')

      if l:startIdx != -1 && l:endIdx != -1
        " There was a match, so save the line number (actually the index), and
        " break the for loop.
        let l:snippetCurrLineIdx = idx
        break
      endif
    endfor

    " If no line matched for l:tabStopNum -- i.e., if the for loop above ran
    " to completion -- then break. This happens when we already went over all
    " tabstops in the snippet, but there might still positional information
    " that has to be stored in s:tabStops. This storing is done after the
    " "while 1" loop.
    if l:placeHolder == "" && l:startIdx == -1 && l:endIdx == -1
      break
    endif

    " Otherwise, set the current line to the line that matched.
    let l:snippetCurrLine = l:snippetLineList[l:snippetCurrLineIdx]

    " The matchstrpos() function used above returns *byte* indexes; the
    " charidx() function converts them to char indexes. This ensures that the
    " code functions properly, even with non-ASCII characters, either in the
    " snippet's expansion, or in the text where it gets inserted.
    let l:startCharIdx = charidx(l:snippetCurrLine, l:startIdx)
    let l:endCharIdx = charidx(l:snippetCurrLine, l:endIdx)

    " As their names let slip, these variables will contain the char idx for
    " the current tabstop, and the length of its placeholder, if there is one
    " (otherwise it is left at 0).
    let l:startCharIdxForCurrTabStop = 0
    let l:placeHolderLength = 0

    " If there is no placeholder -- i.e. we are dealing with tabstops like
    " ${1} -- then the last character of l:placeholder returned by the
    " matchstrpos() function above will be '}'. Otherwise, it will be some
    " other char.
    if l:placeHolder[-1:] != '}' " There is a placeholder.
      call add(l:placeHolders, l:placeHolder) " Eg: placeholder for ${1:arg} has index 0

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
      let l:snippetCurrLine =
            \ strcharpart(l:snippetCurrLine, 0, l:startCharIdxForCurrTabStop)
            \ . l:placeHolder . strcharpart(l:snippetCurrLine, l:endCharIdx + 1 )

      let l:placeHolderLength = strcharlen(l:placeHolder)

      " End replacing ${1:placeholder} with "placeholder", sans quotes.

    else " There is no placeholder.
      call add(l:placeHolders, "") " Eg: empty placeholder for ${1} has index 0

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
      "  snippet's current line.
      let l:startCharIdxForCurrTabStop = l:startCharIdx
      if l:endCharIdx != -1
        let l:snippetCurrLine =
              \ strcharpart(l:snippetCurrLine, 0, l:startCharIdxForCurrTabStop) .
              \ strcharpart(l:snippetCurrLine, l:endCharIdx )
      else
        let l:snippetCurrLine =
              \ strcharpart(l:snippetCurrLine, 0, l:startCharIdxForCurrTabStop)
      endif
      " End replacing ${1} with empty string.
    endif

    " Update the snippet line list with the current line, suitably modified to
    " remove the tabstop currently being processed (starts with ${1}, and
    " moves on...).
    let l:snippetLineList[l:snippetCurrLineIdx] = l:snippetCurrLine

    " See the comments for s:tabStops, at the start of the file.
    call add(s:tabStops, [ l:snippetCurrLineIdx, l:startCharIdxForCurrTabStop,
                         \ l:placeHolderLength, [], []] )

    " Update previous tabstops that are on the same line, and to the right of
    " the current one. Because in that case, replacing ${d:foo} with foo, or
    " ${d} with nothing, will change the cursor position.
    for idx in range(l:tabStopNum - 1)
      if s:tabStops[idx][0] == l:snippetCurrLineIdx &&
            \ s:tabStops[idx][1] > l:startCharIdxForCurrTabStop
        if l:placeHolderLength == 0
          " Replacing ${d} with an empty string removes 4 characters.
          let s:tabStops[idx][1] -= 4
        else
          " Replacing ${d:foo} with foo removes 5 characters (recall that
          " tabstop numbers go from 1 to 9).
          let s:tabStops[idx][1] -= 5
        endif
      endif
    endfor

    let l:tabStopNum += 1
    if l:tabStopNum == 10
      if g:snipper_debug | echomsg "Exceeded limit of 9 tabstops (${1} to ${9})." | endif
      throw "TooManyTabStops"
    endif
  endwhile " while 1

  " For all tabstops, add to s:tabStops the ones that are to the right of the
  " current one (in the same line). See comments of s:tabStops to see why this
  " is done.
  "
  " Note: This cannot be done inside the while loop above, because when
  " processing, say, tabstop n, we still do not have information about
  " tabstops n+1 onwards. And conversely, the updating of previous tabstops
  " that is done inside a for loop, which is itself inside the while loop,
  " could be done here, but the code becomes VERY error-prone (I've tried...).
  for idx in range(len(s:tabStops))
    let l:lineIdx = s:tabStops[idx][0]
    let l:colPos = s:tabStops[idx][1]
    for jdx in range(len(s:tabStops))
      if s:tabStops[jdx][0] == l:lineIdx && s:tabStops[jdx][1] > l:colPos
        " Above if-cond's first condition holds if snippets idx and jdx are on
        " the same line...

        call add(s:tabStops[idx][3], jdx) " Keep the INDEX, not tabstopnum, of tabstops in front of the current one.
      endif
    endfor
  endfor

  return snipper#ProcessSnippetPassiveTabStops(l:snippetLineList, l:placeHolders)
endfunction

function snipper#ProcessSnippetPassiveTabStops(snippetLineList, placeHoldersList)
  if s:passiveTabStops != {} && s:groupedPassiveTabStopsList != []
    " When this function is called, any previous processing of any snippets
    " should either have been completed, or be properly cleared.
    throw "InternalStateNotProperlyCleared"
  endif
  let l:currKey = s:passiveTabStopsStartNumHashKey

  let l:snippetLineList = a:snippetLineList
  for tsn in range(1, len(a:placeHoldersList))
    " if len(s:groupedPassiveTabStopsList) < tsn
    "   " This condition holds when there no element corresponding to the
    "   " current tabstop.
    "   call add(s:groupedPassiveTabStopsList, {})
    " endif
    call add(s:groupedPassiveTabStopsList, {})

    for idx in range(len(a:snippetLineList))
      let l:snippetCurrLineIdx = idx
      let l:line = l:snippetLineList[idx]

      let [ l:matchedText, l:startIdx, l:endIdx ] = ["", -1, -1]

      let l:startPosForMatch = 0
      while 1
        let [ l:matchedText, l:startIdx, l:endIdx ] =
            \ matchstrpos(l:line, '\([^\\]\)\?\zs$'.tsn.'\ze', l:startPosForMatch)

        if l:matchedText == "" && l:startIdx == -1 && l:endIdx == -1
          " If there are no (more) matches for tabstop tsn in this line, then go
          " check the next one.
          break
        endif

        let l:startPosForMatch = l:endIdx

        let l:placeHolder = a:placeHoldersList[tsn-1]
        let l:placeHolderLength = s:tabStops[tsn-1][2]
        let l:startCharIdx = charidx(l:line, l:startIdx) " Start char idx for curr tabstop.
        let l:endCharIdx = charidx(l:line, l:endIdx)

        " Replace $1 or whatever with its placeholder (i.e., ${1:placeholder}).
        let l:line = strcharpart(l:line, 0, l:startCharIdx)
              \ . l:placeHolder . strcharpart(l:line, l:endCharIdx)

        " Update previous passive tabstops, in the same line, to the right of
        " the current one.
        for elem in values(s:passiveTabStops)
          if elem[0] == l:snippetCurrLineIdx && elem[1] > l:startCharIdx
            let elem[1] += (l:placeHolderLength - 2)
          endif
        endfor

        " Also update *active* tabstops.
        for idx in range(len(s:tabStops))
          if s:tabStops[idx][0] == l:snippetCurrLineIdx &&
                \ s:tabStops[idx][1] > l:startCharIdx
            let s:tabStops[idx][1] += (l:placeHolderLength - 2)
          endif
        endfor

        let s:passiveTabStops[nr2char(l:currKey)] =
              \ [ l:snippetCurrLineIdx, l:startCharIdx, [], [] ]

        " s:groupedPassiveTabStopsList is a list of Dicts. Each dict is like
        " { lineIdx => [ a,b,c ] }, where a, b, c, are the passive tabstops
        " that exist in line with offset lineIdx.
        if has_key(s:groupedPassiveTabStopsList[tsn - 1], l:snippetCurrLineIdx)
          call add(s:groupedPassiveTabStopsList[tsn - 1][l:snippetCurrLineIdx],
                \ nr2char(l:currKey))
        else
          let s:groupedPassiveTabStopsList[tsn - 1][l:snippetCurrLineIdx] =
                \ [ nr2char(l:currKey) ]
        endif

        let l:currKey += 1
        if l:currKey == 123 " 122 corresponds to z, so that's it!!
          if g:snipper_debug | echomsg "Exceeded limit of 26 passive tabstops!" | endif
          throw "MaxNumberOfPassiveTabStopsExceeded"
        endif
      endwhile

      let l:snippetLineList[l:snippetCurrLineIdx] = l:line
    endfor
  endfor

  " Now sort the lines of s:groupedPassiveTabStopsList.
  for idx in range(len(s:groupedPassiveTabStopsList))
    let l:currDict = s:groupedPassiveTabStopsList[idx]

    for l:lineIdx in keys(l:currDict)
      let l:passiveTabstopsList = l:currDict[l:lineIdx]
      call sort(l:passiveTabstopsList, {a, b ->
            \ s:passiveTabStops[a][1] == s:passiveTabStops[b][1] ? 0 :
            \ s:passiveTabStops[a][1] > s:passiveTabStops[b][1] ? 1 : -1
            \ })
      let l:currDict[l:lineIdx] = l:passiveTabstopsList " Just to be sure.
    endfor
  endfor

  " We have finished parsing the passive tabstops (and replacing them with
  " placehoders, where applicable). So now have to establish "same line
  " dependencies". That is, if two tabstops (passive or not), are in the same
  " line, then one must be to the right of the other. Here we store that
  " information. (Actually, in the above function snipper#ProcessSnippet(), we
  " deal with the case of two regular tabstops in the same line. Here deal
  " with the 3 remaining possibilities.)
  for l:key in keys(s:passiveTabStops)
    let l:lineIdx = s:passiveTabStops[l:key][0]
    let l:colPos  = s:passiveTabStops[l:key][1]

    for idx in range(len(s:tabStops))
      let l:rts = s:tabStops[idx] " rts = regular tabstop.
      if l:lineIdx == l:rts[0] && l:rts[1] > l:colPos
        call add(s:passiveTabStops[l:key][2], idx) " Kept IDX, not tabstop number.
      elseif l:lineIdx == l:rts[0] && l:rts[1] < l:colPos
        call add(s:tabStops[idx][4], l:key)
      " elseif l:lineIdx == l:rts[0] &&
      "       \ l:rts[1] == l:colPos <-- Cannot happen.
      endif
    endfor

    for l:key2 in keys(s:passiveTabStops)
      if l:lineIdx == s:passiveTabStops[l:key2][0] &&
            \ s:passiveTabStops[l:key2][1] > l:colPos
        call add(s:passiveTabStops[l:key][3], l:key2)

      " There is no else clause, for we do not need to check the "opposite"
      " condition, viz., s:passiveTabStops[l:key2][1] < l:colPos. This is
      " because in this for loop, we generate all possible ORDERED PAIRS, and
      " so just one inequality condition suffices. (After all, if a != b, then
      " either a > b, or a < b.)
      endif
    endfor
  endfor

  let l:snippet = join(l:snippetLineList, "\n")
  return l:snippet
endfunction

" Say the user expands a snippet, which has a tabstop like so: ${1:foo}. After
" hitting <Tab> for the first time to expand the snippet, he will be left in
" select mode, which "foo" selected. If the user now presses <Tab>, this
" function is called with the placeholder "foo" inplace. Otherwise, it is
" called with that placeholder *removed*. To discover which scenario we are
" in, we use the s:lenghtOfLineWhereCursorWent variable (hack...)...
"   The argument a:tabStopNum contains the placeholder number (in the above
" example it would have been 1).
"   This function is called via an autocmd on the InsertEnter event, which is
" triggered when the user either deletes the place holder, or types something,
" overwriting the place holder. In both cases he ends up in insert mode, which
" implies an InsertEnter event.
function snipper#RemovePlaceholders(tabStopNum)
  echomsg "In function snipper#RemovePlaceholders()... "
  if g:snipper_debug | echomsg "In function snipper#RemovePlaceholders()... " | endif
  if g:snipper_debug | echomsg "Argument a:tabStopNum: " . a:tabStopNum | endif

  " if s:lenghtOfLineWhereCursorWent == len(getline(s:snippetInsertionLineNum))
  if s:lenghtOfLineWhereCursorWent == len(getline("."))
    if g:snipper_debug | echomsg "snipper#RemovePlaceholders(): " .
          \ "User pressed <Tab> or <S-Tab>, so keeping placeholders in place..." | endif
    return
  endif

  let s:compensatedForHiddenBS = v:true

  " Otherwise, user either hit <BS>, deleting the placeholder, or typed
  " something (overwriting the placeholder).

  " This is not a part of the state. It is just to keep tabs on the cursor
  " position, so as to be able to detect, in function snipper#UpdateSnippet(),
  " if the user has pressed backspace (<BS>).
  let s:curCol = charcol(".")

  let l:placeholderLen = s:tabStops[a:tabStopNum-1][2]

  " Update passive deps of current tabstop.
  for elem in s:tabStops[a:tabStopNum - 1][4]
    let s:passiveTabStops[elem][1] -= l:placeholderLen
  endfor

  " Update active deps of current tabstop.
  for elem in s:tabStops[a:tabStopNum - 1][3]
    let s:tabStops[elem][1] -= l:placeholderLen
  endfor

  for l:key in keys(s:groupedPassiveTabStopsList[a:tabStopNum - 1])
    let l:lineNum = l:key + s:snippetInsertionLineNum
    let l:lineOriginal = getline(l:lineNum)

    " Names of passive TS, e.g., a, b, etc., already sorted (ascending).
    let l:pTabStopsInCurrLine_l =
          \ s:groupedPassiveTabStopsList[a:tabStopNum - 1][l:key]
    let l:newLine = l:lineOriginal

    " let l:start = 0
    for idx in range(len(l:pTabStopsInCurrLine_l))

      let l:idxOfCurrPassiveTS =
            \ s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][1]

      let l:newLine = slice(l:newLine, 0, s:snippetInsertionPos + l:idxOfCurrPassiveTS - 1) .
            \ slice(l:newLine, s:snippetInsertionPos + l:idxOfCurrPassiveTS + l:placeholderLen - 1)

      " Update passive deps of the passive tabstop we just got rid of.
      for elem in s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][3]
        let s:passiveTabStops[elem][1] -= l:placeholderLen
      endfor

      " Update active deps of the passive tabstop we just got rid of.
      for elem in s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][2]
        let s:tabStops[elem][1] -= l:placeholderLen
      endfor
    endfor

    " We have a finished new line; so added to the Dict of new lines to be
    " inserted into the file.
    call setline(l:lineNum, l:newLine)
  endfor
endfunction

" Brief: Asks the user for a string, and then searches the list of triggers,
" both for the current filetype, and global triggers, for those that match
" that string.
function snipper#SearchForTrigger()
  " The list of triggers need to be (re-) constructed if, either it hasn't
  " been constructed for the first time yet, or if the filetype changed.
  if s:triggerList == [] || &filetype != s:filetype

    " Store the filetype for which we are building the list.
    let s:filetype = &filetype

    let s:triggerList = keys(g:snipper#snippets[s:global_key])
    if &filetype != ""
      let s:triggerList = s:triggerList + keys(g:snipper#snippets[&filetype])
    endif
  endif

  let l:str = input('Enter string: ')

  " Return the (sorted, ignoring case) list of triggers that contain the user
  " supplied string.
  let l:res = filter(copy(s:triggerList), 'v:val =~? "' . l:str . '"')
  echo "\nTriggers: " . join(sort(l:res, 'i'), ', ')
  return ""
endfunction

" NOTA BENE: this function requires the value of s:cursorStartPos and
" s:snippetLineIdx for the current snippet!!! I.e., UpdateState() must be
" called before calling this function!
function snipper#SetCursorPosBeforeReturning(placeholder_length)
  let l:lineNrToPlaceCursor = s:snippetInsertionLineNum + s:snippetLineIdx
  if a:placeholder_length == 0
    " If there is no placeholder, then just place the cursor at the start
    " position determined above, and be done with it.
    call setcharpos('.', [0, l:lineNrToPlaceCursor, s:cursorStartPos])
  else " a:placeholder_length >= 1

    " If there is a placeholder, then the cursor is placed *after the
    " first char* of that placeholder. The reason for that is that, when there
    " is a placeholder, the function snipper#FigureOutWhatToReturn(), which
    " should be called after this one, will do an <Esc>v, which makes the
    " cursor go back one char. So for the cursor to be at s:cursorStartPos
    " after that <Esc>v, it needs to be placed at s:cursorStartPos + 1 before
    " that <Esc>v.
    call setcharpos('.', [0, l:lineNrToPlaceCursor, s:cursorStartPos + 1])
  endif
  let s:lenghtOfLineWhereCursorWent = len(getline(l:lineNrToPlaceCursor)) " ugly hack :-(
endfunction

" Brief: Allow the processing of a snippet expansion to be interrupted, by
" hitting either <Esc>, or <Ctrl-c>. This function captures that event, and
" call snipper#ClearState(), to remove state information. This clears the way
" for a new snippet expansion to be processed.
"   This function also remaps the backspace key, to either go do append or
" insert afterwards, depending or whether the current placeholder ends the
" line or not...
function snipper#SetTraps(reverse = 0)
  " inoremap <buffer><expr> <Esc> snipper#ClearState()
  inoremap <buffer><silent> <C-c> <Esc>:call snipper#ClearStateCc()<CR>
  " snoremap <buffer><expr> <Esc> snipper#ClearState()
  snoremap <buffer><silent> <C-c> <Esc>:call snipper#ClearStateCc()<CR>

  " Hereinafter, the only thing that is left to do, is map the <BS> in select
  " mode, in case an user decides delete the placeholder text. But this is
  " only needed if there are tabstops -- if not, we are done.
  if len(s:tabStops) == 0
    return
  endif

  " If there is at least one tabstop, untupple the list here, to initialise
  " the variables.
  let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:subsequent ]
        \ = s:tabStops[0]
  let l:placeHolderEndsLine_b = v:false

  " s:nextTabStopNum goes from 0 to 2. When control reaches here,
  " s:nextTabStopNum has the number of the tabstop the user is currently at.
  if s:nextTabStopNum >= 2
    " If we are cycling forward, then we want the data for the current
    " tabstop, which has index s:nextTabStopNum.
    if a:reverse == 0
      let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:subsequent ]
            \ = s:tabStops[s:nextTabStopNum - 1]
    else
      " If we are cycling backwards, then the next tabstop is actually the
      " one before the current one, which has index s:nextTabStopNum - 2.
      let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:subsequent ]
            \ = s:tabStops[s:nextTabStopNum - 2]
    endif
  endif

  " Get line where the placeholder for the current tabstop was inserted.
  let l:line = getline(s:snippetInsertionLineNum + l:idxForLine)

  " The below if-cond checks if the placeholder is the last (rightmost) text
  " of the line. To see why it is computed like this, imagine a line "abc",
  " where "bc" is the placeholder.
  "   (Recall that s:snippetInsertionPos + l:idxForCursor yields the COLUMN of
  " the first placeholder char.) So the "b" is at col 2, and the placeholder
  " has length 2, and 2+2 = 4 = 3 + 1, where 3 is the length of the line, with
  " the placeholder in place.
  " (Recall that s:snippetInsertionPos + l:idxForCursor == COLUMN of the first
  " placeholder char.)
  if s:snippetInsertionPos + l:idxForCursor + l:placeHolderLength
        \ == strcharlen(l:line) + 1
    let l:placeHolderEndsLine_b = v:true
  endif

  " This <BS> map is here because if the user, having a visually selected
  " placeholder (select mode), hits backspace, the placeholder text is
  " deleted, *but the user is left in normal mode*. To fix this, i.e. to allow
  " the user to continue either inserting text, or jumping to the next
  " tabstop, I remap the backspace key to do backspace, and then go to insert
  " mode, either with (i)nsert or (a)ppend, depending on whether or not the
  " placeholder ends the line.
  if l:placeHolderEndsLine_b == v:true
    " If the placeholder ends the line, and the user hits <BS> in select mode,
    " then the cursor will left over the *previous* character -- so we need to
    " hit (a)ppend.
    snoremap <buffer> <BS> <BS>a
  else
    " Otherwise, the cursor will be left over the *next* character, so we hit
    " (i)nsert.
    snoremap <buffer> <BS> <BS>i
  endif
endfunction

" Brief: This function is basically divided into two parts. As it is called
" when the <Tab> key is pressed in Insert mode (i.e., after the user has typed
" or deleted something), it detects wether what the user typed is a trigger
" for a snippet, or a tabstop for a snippet currently being processed. The
" upper part of the function deals with the latter case; the lower, with the
" former.
function snipper#TriggerSnippet()

  " The user just hit <Tab>, so disable all autocmd's, if any.
  if exists("#vimSnipperAutocmds")
    autocmd! vimSnipperAutocmds
  endif

  if s:nextTabStopNum != 0
    " If s:nextTabStopNum is not its default value (0), that means that either
    " ${s:nextTabStopNum - 1} or ${s:nextTabStopNum - 1:arg} exist, so now we
    " must see if there exists a tabstop for the following digit, i.e.,
    " s:nextTabStopNum.
    "   [len(s:tabStops) is the number (1-based) of the last tabstop in the
    " current snippet.]

    if s:nextTabStopNum > len(s:tabStops)
      " There is no ${s:nextTabStopNum} tabstop, so first, clear the state.
      " And next, call snipper#TriggerSnippet() again, to check if the user
      " wants to expand another snippet trigger.
      "   We then return the result of that trigger check.
      call snipper#ClearState()

      let l:ret = snipper#TriggerSnippet()
      echo "what fuck: " . l:ret
      return l:ret
    endif

    " There is a ${s:nextTabStopNum} tabstop, so go process it. Control
    " arrives here because at ${s:nextTabStopNum - 1}, the user typed the
    " text he wanted, and then hit <Tab>. And that brought him to the
    " current tabstop, viz., s:nextTabStopNum.

    " Here s:nextTabStopNum is at least 2. We update l:placeHolderLength of the
    " tabstop the user is leaving...
    let l:tabStopNumTheUserJustCameFrom = s:nextTabStopNum - 1
    let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
          \ s:tabStops[l:tabStopNumTheUserJustCameFrom - 1]
    let l:charShift = charcol(".") - (s:snippetInsertionPos + l:idxForCursor) " length of the text the user entered at the placeholder he came from
    let s:tabStops[l:tabStopNumTheUserJustCameFrom - 1][2] = l:charShift

    " We also reset the "starting point" for the passive deps of
    " l:tabStopNumTheUserJustCameFrom
    for l:key in keys(s:groupedPassiveTabStopsList[l:tabStopNumTheUserJustCameFrom - 1])
      let l:pTabStopsInCurrLine_l =
            \ s:groupedPassiveTabStopsList[l:tabStopNumTheUserJustCameFrom - 1][l:key]

      for idx in range(len(l:pTabStopsInCurrLine_l))
        let s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][1] -= l:charShift
      endfor
    endfor

    " And retrieve the information about ${s:nextTabStopNum} (which has index
    " s:nextTabStopNum - 1). This is needed for snipper#UpdateState().
    let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
          \ s:tabStops[s:nextTabStopNum - 1]

    " Set the traps for the processing of the current tabsop,
    " ${s:nextTabStopNum}.
    call snipper#SetTraps()

    " NOTA BENE: function snipper#UpdateState() increaments
    " s:nextTabStopNum, which is a part of the state that needs to be kept.
    call snipper#UpdateState(l:idxForLine, l:idxForCursor)

    call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
    return snipper#FigureOutWhatToReturn(l:placeHolderLength)
  endif " if s:nextTabStopNum != 0

  " If control reaches here, it means the user just entered a trigger and hit
  " <Tab>. So let us see if there is anything to process, or if we just have
  " to return a <Tab>.

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

  if l:col == 1 || l:line[l:col - 2] =~ '\m\s'
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

  while l:prevCharIdx >= 1 && l:line[l:prevCharIdx - 1] =~ '\m\S'
    " See comments for g:snipper#triggerBoundaries.
    if stridx(g:snipper#triggerBoundaries, l:line[l:prevCharIdx - 1]) != -1
      break
    endif
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

  " l:key is the key under which the trigger is, in the g:snipper#snippets
  " hash table. It is either the file type, or global (or v:false, if the
  " trigger could not be found).
  let l:key = snipper#CheckSnippetExists(l:trigger)
  if l:key == v:false
    " If there is no snippet to expand, then just return the Tab key.
    return "\<Tab>"
  endif

  " The snippet for l:trigger exists, so lets go on, erm, triggering it.

  " Check if we had processed this snippet before. If so, no point in doing
  " it all over again; just retrieve the previously store state information.
  " Otherwise, need to process it from scratch.

  let l:triggerExpansion = ""
  if has_key(g:snipper#ProcessedSnippets, l:key) == v:true &&
        \ has_key(g:snipper#ProcessedSnippets[l:key], l:trigger) == v:true
    " We have seen this snippet before, so retrieve the needed information.
    let l:triggerExpansion = g:snipper#ProcessedSnippets[l:key][l:trigger]['snippet']
    let s:tabStops = deepcopy(g:snipper#ProcessedSnippets[l:key][l:trigger]['tabstops'])
    let s:passiveTabStops =
          \ deepcopy(g:snipper#ProcessedSnippets[l:key][l:trigger]['passiveTS'])
    let s:groupedPassiveTabStopsList =
          \ deepcopy(g:snipper#ProcessedSnippets[l:key][l:trigger]['groupedPassiveTS'])
  else
    " We have NOT seen this snippet before, so on we go with processing it.
    " The snipper#ProcessSnippet() function will set the s:tabStops
    " variable.
    let l:triggerExpansion =
          \ snipper#ProcessSnippet(g:snipper#snippets[l:key][l:trigger])

    " Save the information resulting from the processing of the snippet,
    " to use if we have to expand the same snippet again in the future.
    " First, create the sub-hash, if needed.
    if has_key(g:snipper#ProcessedSnippets, l:key) == v:false
      let g:snipper#ProcessedSnippets[l:key] = {}
    endif
    " Then store the processing information.
    let g:snipper#ProcessedSnippets[l:key][l:trigger] =
          \ { 'snippet': l:triggerExpansion,
          \ 'tabstops': deepcopy(s:tabStops),
          \ 'passiveTS': deepcopy(s:passiveTabStops),
          \ 'groupedPassiveTS': deepcopy(s:groupedPassiveTabStopsList)}
  endif " Check g:snipper#ProcessedSnippets[ftype|global] dict for key l:trigger.

  let l:triggerProcessedList = split(l:triggerExpansion, "\n", 1)

  " Insert the processed and expanded lines into the snippet. These line have
  " the tabstops removed, and replaced with placeholders, if applicable.
  call setline(".", l:beforeTrigger . l:triggerProcessedList[0])
  let l:numOfInsertedLinesMinusOne = len(l:triggerProcessedList) - 1
  let l:indent = matchend(l:line, '^.\{-}\ze\(\S\|$\)')
  call append(l:currLineNum,
            \ map(l:triggerProcessedList[1:], "'".strpart(l:line, 0, l:indent)."'.v:val"))
  call setline(l:currLineNum + l:numOfInsertedLinesMinusOne,
        \ getline(l:currLineNum + l:numOfInsertedLinesMinusOne) . l:afterTrigger)

  if len(s:tabStops) == 0
    " If there are no tabstops, then just place the cursor at the end of
    " inserted snippet, and return an empty string.

    if l:numOfInsertedLinesMinusOne == 0
      " If we just inserted one line, then the cursor pos is the current pos
      " plus the length of the expansion, minus the length of the trigger
      " text.
      call setcharpos('.', [0, l:currLineNum,
                          \ l:charCol + strcharlen(l:triggerExpansion) - l:triggerLength])
    else
      " If we inserted more than one line, then place the cursor at the end of
      " the last inserted line. That line is line number l:currLineNum +
      " l:numOfInsertedLinesMinusOne; and the column position is the length of
      " that line. (Remember that column positions start at 1. Also, if we
      " enter a 3-char string at the start of the line, then the cursor pos is
      " 4. Hence we need to add + 1.)
      call setcharpos('.', [0, l:currLineNum + l:numOfInsertedLinesMinusOne, l:indent +
            \ strcharlen(l:triggerProcessedList[l:numOfInsertedLinesMinusOne]) + 1])
    endif

    return ""
  endif " if len(s:tabStops) == 0

  " Otherwise there are tabstops to process. So we process the first one,
  " ${1}, and call UpdateState(), to prepare the processing of the next
  " tabstop, should it exist.

  " The column (char-based) of the cursor at the point the snippet was
  " inserted (e.g., if the snippet was inserted at the start of the line,
  " then s:snippetInsertionPos = 1).
  "   To see why it is computed in this way, imagine a line containing the
  " string "abc ", with the cursor after the ' ', in insert mode, when the
  " user types the trigger "xpto", and hits Tab. So the snippet insert
  " position will be after the "abc " string, and this is column position 5.
  " Now, after typing "xpto", the cursor will be at column 9 -- so this will
  " be the value of l:charCol. And the trigger length is 4. Hence, the snippet
  " insert position will be 9 - 4 = 5, as expected.
  "   NOTA BENE: l:charCol is computed with the function charcol(), and the
  " trigger has to be ASCII only, so this works even if the previous text
  " -- "abc " in the example above -- contains non-ASCII characters.
  let s:snippetInsertionPos = l:charCol - l:triggerLength
  " Like s:snippetInsertionPos, this var (s:snippetInsertionLineNum) is only
  " set once.
  let s:snippetInsertionLineNum = l:currLineNum

  " l:idxForCursor contains the array index (0-based) of the position of the
  " '$' in ${1:arg}, in the expanded snippet. l:placeHolderLength is the
  " (char) length of "arg".
  let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:subsequent ]
        \ = s:tabStops[0] " index one less than trigger number!

  " Set the mappings to catch <Esc>, or <Ctrl-c>, and do the necessary cleanup
  " actions (e.g., clean the state variables, etc.).
  call snipper#SetTraps()

  " The s:snippetInsertionPos is only set once: after all, the snippet is only
  " inserted at one point. The remaining state information is set in the
  " UpdateState() function.
  call snipper#UpdateState(l:idxForLine, l:idxForCursor)

  " The next two functions place the cursor at the first tabstop location
  " (remember that the "${1}" string is no longer there), and visually select
  " the placeholder, if any, and go to select mode. (If there is no
  " placeholder, just place the cursor at the location where the $ in ${1}
  " used to be.)
  call snipper#SetCursorPosBeforeReturning(l:placeHolderLength)
  return snipper#FigureOutWhatToReturn(l:placeHolderLength)
endfunction

" Brief: This function is called, via an autocmd, on the CursorMovedI event.
" Its purpose is to, as the user is typing his text for tabstop a:tabStopNum,
" put that text also in the passive tabstops of a:tabStopNum, if any.
"
" This function is also called when there is no place holder -- see the
" explanation in the comments inside the function.
function snipper#UpdateSnippet(tabStopNum, text = '')
  " XXX this function is only called when jumping to a tab with no
  " placeholder..
  echom "ts " . a:tabStopNum . " UpdateSnippet..."
  if g:snipper_debug | echomsg "In function snipper#UpdateSnippet()... " | endif
  if g:snipper_debug | echomsg "Argument a:tabStopNum: " . a:tabStopNum | endif
  if g:snipper_debug | echomsg "Argument a:text: " . a:text | endif

  let l:charShift = 1
  let l:curCol = charcol(".")
  let l:insertedChar = ""
  let l:insertedCharIsBackspace = v:false
  let l:line = getline(".")

  if (a:text == '')
    " If no a:text was supplied, that means this function was invoked via an
    " autocmd on the InsertEnter event. So we have to detect whether a char
    " was actually entered or deleted, or if the user just deleted the
    " placeholder with backspace, etc.

    " If current tabstop has no placeholders, s:curCol will be undefined.

    if s:charActuallyInserted == v:true && s:compensatedForHiddenBS == v:false
      echom "ts " . a:tabStopNum . " compensating bs..."
      " if g:snipper_debug | echomsg "snipper#UpdateSnippet(): " .
      "       \ "Compensating for the 'extra' <BS>... " | endif

      let s:compensatedForHiddenBS = v:true
      return
    elseif exists("s:curCol") && l:curCol == s:curCol
    " if exists("s:curCol") && l:curCol == s:curCol
      echom "user deleted placeholder..."
      " The current function is called after deleting the placeholder, only if
      " that deletion leads to redrawing (setline()) of the line where that
      " placeholder was...

      " User deleted the placeholder with backspace or delete, inserting no
      " char. Hence, no need to update anything...
      return
    elseif exists("s:curCol") && l:curCol == s:curCol - 1
      echom "User hit backspace in insert mode."
      " User hit backspace in insert mode.
      let l:insertedCharIsBackspace = v:true
      let l:charShift = -1
    else
      " Control gets here either when an actual character was inserted in insert
      " mode, or when there is no placeholder for the current tabstop.
      "
      " About the second case: this is because before this tabstop, the user
      " either expanded the trigger (insert mode), or jumped from a previous
      " tabstop, WHICH IS ALWAYS DONE IN INSERT MODE, even when that previous
      " tabstop has a placeholder, and the user does not modify it (cf.  the map
      " of function snipper#JumpToPreviousTabStop() in file
      " after/plugin/vim-snipper.vim). So we always end up with a motion of
      " cursor in insert mode, which triggers event CursorMovedI.

      " strgetchar() returns a decimal number for char, which nr2char() converts
      " to a char proper.
      let l:insertedChar = nr2char(strgetchar(l:line, l:curCol-2))
      echom "ins char found " . l:insertedChar
    endif
    let s:curCol = l:curCol

    " Now we know that we have to update the snippet if either there was a
    " character actually inserted, or if the user hit backspace in insert mode,
    " deleting an actual character. The negated form of this condition thus
    " tells us when there is nothing to do...
    if s:charActuallyInserted == v:false && l:insertedCharIsBackspace == v:false
      echomsg "snipper#UpdateSnippet: No character was inserted, so nothing to do..."
      return
    endif
    let s:charActuallyInserted = v:false
  else " a:text != ''
    let l:insertedChar = a:text
    let l:charShift = strcharlen(a:text)
  endif " if (a:text == '')

  " Now we know for sure that there was character(s) either inserted, or
  " deleted. So save the compounding character shift.
  let s:numCharsInsertedCurrTabstop += l:charShift

  " Update passive deps of current tabstop, if any.
  if len(s:tabStops[a:tabStopNum - 1]) >= 4
    for elem in s:tabStops[a:tabStopNum - 1][4]
      let s:passiveTabStops[elem][1] += l:charShift
    endfor
  endif

  " Update active deps of current tabstop.
  for elem in s:tabStops[a:tabStopNum - 1][3]
    let s:tabStops[elem][1] += l:charShift
  endfor

  for l:key in keys(s:groupedPassiveTabStopsList[a:tabStopNum - 1])
    let l:lineNum = l:key + s:snippetInsertionLineNum
    let l:lineOriginal = getline(l:lineNum)

    " Names of passive TS, e.g., a, b, etc., already sorted (ascending).
    let l:pTabStopsInCurrLine_l =
          \ s:groupedPassiveTabStopsList[a:tabStopNum - 1][l:key]
    let l:newLine = l:lineOriginal

    for idx in range(len(l:pTabStopsInCurrLine_l))

      let l:idxOfCurrPassiveTS =
            \ s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][1]

      if l:insertedCharIsBackspace == v:false
        let l:newLine = slice(l:newLine, 0, s:snippetInsertionPos + l:idxOfCurrPassiveTS - 1) .
              \ l:insertedChar .
              \ slice(l:newLine, s:snippetInsertionPos + l:idxOfCurrPassiveTS - 1)
      else " User entered <BS>.
        let l:newLine = slice(l:newLine, 0, s:snippetInsertionPos + l:idxOfCurrPassiveTS - 2) .
              \ slice(l:newLine, s:snippetInsertionPos + l:idxOfCurrPassiveTS - 1)
      endif

      let s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][1] += l:charShift

      " Update passive deps of the passive tabstop we just added a char to.
      for elem in s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][3]
        let s:passiveTabStops[elem][1] += l:charShift
      endfor

      " Update active deps of the passive tabstop we just added a char to.
      for elem in s:passiveTabStops[l:pTabStopsInCurrLine_l[idx]][2]
        let s:tabStops[elem][1] += l:charShift
      endfor
    endfor

    " We have a finished new line; so added to the Dict of new lines to be
    " inserted into the file.
    call setline(l:lineNum, l:newLine)
  endfor " for l:key in keys(s:groupedPassiveTabStopsList[a:tabStopNum - 1])
  echom "--- snippetUpdate() end ---"
endfunction

" Brief: Takes care of updating state variables, except s:snippetInsertionPos,
" the value of which is only set once (and hence, no needed for updates after
" that).
function snipper#UpdateState(idxForLine, idxForCursor)
  if s:nextTabStopNum == 0
    let s:nextTabStopNum = 2
    " let s:compensatedForHiddenBS = v:true
  else
    " let s:compensatedForHiddenBS = v:false
    " It should not be necessary here to check that s:nextTabStopNum only goes
    " up to 9, because function snipper#ProcessSnippet() already checks for
    " this.
    let s:nextTabStopNum += 1
  endif

  echom "setting compensatedForHiddenBS to false in UpdateState()"
  let s:compensatedForHiddenBS = v:false

  " XXX
  let s:numCharsInsertedCurrTabstop = 0

  " Set autocmds for the tab stop we just finished processing, i.e.,
  " s:nextTabStopNum - 1. This way, as the user types his text, the
  " autocommands will update any (possible passive) tabstops that need to be
  " updated as a result of the text entered, for the current tabstop.
  "   The reason this is done like this, instead of setting the autocmds for
  " s:nextTabStopNum, and then incrementing it, is that, if done like that,
  " when the autocmds are actually run, they will use the current value of
  " s:nextTabStopNum, AFTER IT HAS BEEN INCREMENTED -- which is not what is
  " desired.
  "   This approach does not suffer from the above problem, because
  " s:nextTabStopNum will only be changed after the user hits <Tab> again --
  " and so, while he is typing (i.e., before he hits <Tab>), s:nextTabStopNum
  " - 1 will have the same value as it had when the autocmds where set:
  " namely, the value of the tabstop at which the user is typing.
  augroup vimSnipperAutocmds
    autocmd!
    autocmd InsertCharPre * call snipper#CharActuallyInserted()
    autocmd InsertEnter * call snipper#RemovePlaceholders(s:nextTabStopNum - 1)
    autocmd CursorMovedI,TextChangedP * call snipper#UpdateSnippet(s:nextTabStopNum - 1)
    autocmd CompleteDone * call snipper#CompletionDone(s:nextTabStopNum - 1)
  augroup END

  " See comments for s:snippetLineIdx, in the top part of the file.
  let s:snippetLineIdx = a:idxForLine

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

function snipper#CompletionDone(tabStopNum)
  let [ l:idxForLine, l:idxForCursor, l:placeHolderLength ; l:whatever_notNeeded ] =
        \ s:tabStops[a:tabStopNum - 1]
  let l:curLine = getline(".")
  let l:curEndPos = charcol(".") " Cursor column when CompleteDone triggered.

  let l:newPlaceHolder = slice(l:curLine, s:snippetInsertionPos + l:idxForCursor - 1
        \ + s:numCharsInsertedCurrTabstop, l:curEndPos - 1)

  " Now update them with l:newPlaceHolder.
  call snipper#UpdateSnippet(a:tabStopNum, l:newPlaceHolder)
endfunction

" --- Miscellaneous... sort of. ---

" Brief: This function is borrowed from Michael Sanders' original snipMate.
" Its function is to allow snippets to have access to the file name of the
" file where the snippet is being expanded. Cf. cpp.snippets
" XXX document this
function! Filename(...)
	let l:filename = expand('%:t:r')
	if l:filename == '' | return a:0 == 2 ? a:2 : '' | endif
	return !a:0 || a:1 == '' ? l:filename : substitute(a:1, '$1', l:filename, 'g')
endfunction
