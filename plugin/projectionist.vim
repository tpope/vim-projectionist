" Location:     plugin/projectionist.vim
" Author:       Tim Pope <http://tpo.pe/>

if exists("g:loaded_projectionist") || v:version < 700 || &cp
  finish
endif
let g:loaded_projectionist = 1

if !exists('g:projectiles')
  let g:projectiles = {}
endif

function! s:has(root, file) abort
  if a:file =~# '\*'
    return !empty(glob(a:root . '/' . a:file))
  elseif a:file =~# '/$'
    return isdirectory(a:root . '/' . a:file)
  else
    return filereadable(a:root . '/' . a:file)
  endif
endfunction

function! ProjectionistDetect(path) abort
  let b:projectionist = {}
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
    for [key, value] in items(g:projectiles)
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

  try
    let g:projectile_file = file
    let g:projectionist_file = file
    let b:projectionist_file = file
    if v:version + has('patch438') >= 704
      silent doautocmd <nomodeline> User ProjectileDetect
      silent doautocmd <nomodeline> User ProjectionistDetect
    else
      silent doautocmd User ProjectileDetect
      silent doautocmd User ProjectionistDetect
    endif
  finally
    unlet! g:projectile_file
    unlet! g:projectionist_file
    unlet! b:projectionist_file
  endtry

  if !empty(b:projectionist)
    let b:projectionist_file = file
    call projectionist#activate()
  endif
endfunction

augroup projectionist
  autocmd!
  autocmd FileType *
        \ if &filetype ==# 'netrw' || &buftype !~# 'nofile\|quickfix' |
        \   call ProjectionistDetect(expand('%:p')) |
        \  endif
  autocmd BufFilePost * call ProjectionistDetect(expand('<afile>:p'))
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call ProjectionistDetect(expand('<afile>:p')) |
        \ endif
  autocmd CmdWinEnter * call ProjectionistDetect(expand('#:p'))
  autocmd User NERDTreeInit,NERDTreeNewRoot
        \ call ProjectionistDetect(b:NERDTreeRoot.path.str())
  autocmd VimEnter *
        \ if empty(expand('<afile>:p')) |
        \   call ProjectionistDetect(getcwd()) |
        \ endif
  autocmd BufWritePost .projections.json call ProjectionistDetect(expand('<afile>:p'))
  autocmd BufNewFile *
        \ if !empty(b:projectionist) |
        \   call projectionist#apply_template() |
        \ endif
augroup END
