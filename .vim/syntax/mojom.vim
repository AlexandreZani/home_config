" Vim syntax file
" Language:     Mojom
" Maintainer:   Alexandre Zani <alex@zfc.io>
" Last Change:  2016 Sept 23
" Version:      0.1

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn keyword mojomClause import module

syn keyword mojomType struct union const interface
syn keyword mojomType int8 int16 utint32 int64
syn keyword mojomType uint8 uint16 utint32 uint64
syn keyword mojomType string

" Commants
syn keyword     mojomTodo           contained TODO FIXME XXX
syn match       mojomLineComment    "\/\/.*" contains=@Spell,mojomTodo
syn match       mojomCommentSkip    "^[ \t]*\*\($\|[ \t]\+\)"
syn region      mojomComment        start="/\*"  end="\*/" contains=@Spell,mojomTodo

" Literals
syn region      mojomString         start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell

syn match       mojomSpecial        display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn match       mojomCharacter      "L\='[^\\]'"
syn match       mojomCharacter      "L'[^']*'" contains=mojomSpecial

hi def link mojomString        String
hi def link mojomClause        Preproc
hi def link mojomType          Type
hi def link mojomSpecial       Special
hi def link mojomCharacter     Character
hi def link mojomTodo          Todo
hi def link mojomLineComment   mojomComment
hi def link mojomComment       Comment

let b:current_syntax = "mojom"
