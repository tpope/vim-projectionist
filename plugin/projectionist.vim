" Location:     plugin/projectionist.vim
" Author:       Tim Pope <http://tpo.pe/>
" Version:      1.1
" GetLatestVimScripts: 4989 1 :AutoInstall: projectionist.vim

if exists("g:loaded_projectionist") || v:version < 700 || &cp
  finish
endif
let g:loaded_projectionist = 1

if !exists('g:projectionist_heuristics')
  let g:projectionist_heuristics = {}
endif

function! s:has(root, file) abort
  let file = matchstr(a:file, '[^!].*')
  if file =~# '\*'
    let found = !empty(glob(a:root . '/' . file))
  elseif file =~# '/$'
    let found = isdirectory(a:root . '/' . file)
  else
    let found = filereadable(a:root . '/' . file)
  endif
  return a:file =~# '^!' ? !found : found
endfunction

function! ProjectionistDetect(path) abort
  let b:projectionist = {}
  unlet! b:projectionist_file
  if a:path =~# '^\a[[:alnum:].+-]\+:'
    let file = substitute(a:path, '[\/]$', '', '')
  else
    let file = simplify(fnamemodify(resolve(a:path), ':p:s?[\/]$??'))
  endif

  let root = file
  let previous = ""
  while root !=# previous && root !=# '.'
    if s:has(root, '.projections.json')
      try
        let value = projectionist#json_parse(readfile(root.'/.projections.json'))
        call projectionist#append(root, value)
      catch /^invalid JSON:/
      endtry
    endif
    for [key, value] in items(g:projectionist_heuristics)
      for test in split(key, '|')
        if empty(filter(split(test, '&'), '!s:has(root, v:val)'))
          call projectionist#append(root, value)
          break
        endif
      endfor
    endfor
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile

  if exists('#User#ProjectionistDetect')
    if v:version >= 704 || (v:version == 703 && has('patch442'))
      try
        let g:projectionist_file = file
        doautocmd <nomodeline> User ProjectionistDetect
      finally
        unlet! g:projectionist_file
      endtry
    else
      let modelines = &modelines
      try
        set modelines=0
        let g:projectionist_file = file
        doautocmd User ProjectionistDetect
      finally
        let &modelines = modelines
        unlet! g:projectionist_file
      endtry
    endif
  endif

  if !empty(b:projectionist)
    let b:projectionist_file = file
    call projectionist#activate()
  endif
endfunction

augroup projectionist
  autocmd!
  autocmd FileType *
        \ if (&filetype ==# 'netrw' && !exists('b:projectionist')) ||
        \     &buftype !~# 'nofile\|quickfix' |
        \   call ProjectionistDetect(expand('%:p')) |
        \ endif
  autocmd BufFilePost * call ProjectionistDetect(expand('<afile>:p'))
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call ProjectionistDetect(expand('<afile>:p')) |
        \ endif
  autocmd CmdWinEnter *
        \ if !empty(getbufvar('#', 'projectionist_file')) |
        \   let b:projectionist_file = getbufvar('#', 'projectionist_file') |
        \   let b:projectionist = getbufvar('#', 'projectionist') |
        \   call projectionist#activate() |
        \ endif
  autocmd User NERDTreeInit,NERDTreeNewRoot
        \ call ProjectionistDetect(b:NERDTree.root.path.str())
  autocmd VimEnter *
        \ if empty(expand('<afile>:p')) |
        \   call ProjectionistDetect(getcwd()) |
        \ endif
  autocmd BufWritePost .projections.json call ProjectionistDetect(expand('<afile>:p'))
  autocmd BufNewFile *
        \ if !empty(get(b:, 'projectionist')) |
        \   call projectionist#apply_template() |
        \   setlocal nomodified |
        \ endif
augroup END
