" Author: Tim Pope <http://tpo.pe/>

if exists('g:autoloaded_projectionist')
  finish
endif
let g:autoloaded_projectionist = 1

function! projectionist#append(...) abort
  return call('projectile#append', a:000)
endfunction
