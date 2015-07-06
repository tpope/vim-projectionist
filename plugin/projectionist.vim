" Location:     plugin/projectionist.vim
" Author:       Tim Pope <http://tpo.pe/>
" Version:      1.0
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
  let file = simplify(fnamemodify(a:path, ':p:s?[\/]$??'))

  let root = file
  let previous = ""
  while root !=# previous
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

  let modelines = &modelines
  try
    set modelines=0
    let g:projectionist_file = file
    silent doautocmd User ProjectionistDetect
  finally
    let &modelines = modelines
    unlet! g:projectionist_file
  endtry

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
        \   call ProjectionistDetect(resolve(expand('%:p'))) |
        \ endif
  autocmd BufFilePost * call ProjectionistDetect(resolve(expand('<afile>:p')))
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call ProjectionistDetect(resolve(expand('<afile>:p'))) |
        \ endif
  autocmd CmdWinEnter *
        \ if !empty(getbufvar('#', 'projectionist_file')) |
        \   let b:projectionist_file = getbufvar('#', 'projectionist_file') |
        \   let b:projectionist = getbufvar('#', 'projectionist') |
        \   call projectionist#activate() |
        \ endif
  autocmd User NERDTreeInit,NERDTreeNewRoot
        \ call ProjectionistDetect(b:NERDTreeRoot.path.str())
  autocmd VimEnter *
        \ if empty(expand('<afile>:p')) |
        \   call ProjectionistDetect(resolve(getcwd())) |
        \ endif
  autocmd BufWritePost .projections.json call ProjectionistDetect(resolve(expand('<afile>:p')))
  autocmd BufNewFile *
        \ if !empty(get(b:, 'projectionist')) |
        \   call projectionist#apply_template() |
        \   setlocal nomodified |
        \ endif
augroup END
