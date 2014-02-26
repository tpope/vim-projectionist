" Location:     plugin/projectile.vim
" Author:       Tim Pope <http://tpo.pe/>

if exists("g:loaded_projectile") || v:version < 700 || &cp
  finish
endif
let g:loaded_projectile = 1

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

function! ProjectileDetect(path) abort
  let b:projectiles = {}
  let file = simplify(fnamemodify(a:path, ':p:s?[\/]$??'))

  let root = file
  let previous = ""
  while root !=# previous
    if s:has(root, '.projections.json')
      try
        let value = projectile#json_parse(readfile(root.'/.projections.json'))
        call projectile#append(root, value)
      catch /^invalid JSON:/
      endtry
    endif
    for [key, value] in items(g:projectiles)
      for test in split(key, '|')
        if empty(filter(split(test, '&'), '!s:has(root, v:val)'))
          call projectile#append(root, value)
          break
        endif
      endfor
    endfor
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile

  try
    let g:projectile_file = file
    silent doautocmd User ProjectileDetect
  finally
    unlet! g:projectile_file
  endtry

  if !empty(b:projectiles)
    call projectile#activate()
  endif
endfunction

augroup projectile
  autocmd!
  autocmd FileType * call ProjectileDetect(expand('<afile>:p'))
  autocmd BufNewFile,BufReadPost *
        \ if empty(&filetype) |
        \   call ProjectileDetect(expand('<afile>:p')) |
        \ endif
  autocmd BufWritePost .projections.json call ProjectileDetect(expand('<afile>:p'))
  autocmd BufNewFile *
        \ if !empty(b:projectiles) |
        \   call projectile#apply_template() |
        \ endif
augroup END
