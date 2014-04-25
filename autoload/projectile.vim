" Location:     autoload/projectile.vim
" Author:       Tim Pope <http://tpo.pe/>

if exists("g:autoloaded_projectile")
  finish
endif
let g:autoloaded_projectile = 1

function! projectile#append(...) abort
  return call('projectionist#append', a:000)
endfunction

function! projectile#json_parse(...) abort
  return call('projectionist#json_parse', a:000)
endfunction

function! projectile#path(...) abort
  return call('projectionist#path', a:000)
endfunction

function! projectile#query(...) abort
  return call('projectionist#query', a:000)
endfunction
