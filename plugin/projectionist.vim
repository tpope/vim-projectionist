" Location:     plugin/projectionist.vim
" Author:       Tim Pope <http://tpo.pe/>
" Version:      1.3
" GetLatestVimScripts: 4989 1 :AutoInstall: projectionist.vim

if exists("g:loaded_projectionist") || v:version < 700 || &cp
  finish
endif
let g:loaded_projectionist = 1

" ProjectionistHas('Gemfile&lib/|*.gemspec', '/path/to/root')
function! ProjectionistHas(req, ...) abort
  if type(a:req) != type('')
    return
  endif
  let ns = matchstr(a:0 ? a:1 : a:req, '^\a\a\+\ze:')
  call s:load(ns)
  if !a:0
    return s:nscall(ns, a:req =~# '[\/]$' ? 'isdirectory' : 'filereadable', a:req)
  endif
  for test in split(a:req, '|')
    if s:has(ns, a:1, test)
      return 1
    endif
  endfor
endfunction

if !exists('g:projectionist_heuristics')
  let g:projectionist_heuristics = {}
endif

if !exists('s:loaded')
  let s:loaded = {}
endif

function! s:load(ns) abort
  if len(a:ns) && !has_key(s:loaded, a:ns) && len(findfile('autoload/' . a:ns . '.vim', escape(&rtp, ' ')))
    exe 'runtime! autoload/' . a:ns . '.vim'
    let s:loaded[a:ns] = 1
  endif
endfunction

function! s:nscall(ns, fn, path, ...) abort
  if len(a:ns) && !get(g:, 'projectionist_ignore_' . a:ns) && exists('*' . a:ns . '#' . a:fn)
    return call(a:ns . '#' . a:fn, [a:path] + a:000)
  else
    return call(a:fn, [a:path] + a:000)
  endif
endfunction

function! s:has(ns, root, requirements) abort
  if empty(a:requirements)
    return 0
  endif
  for test in split(a:requirements, '&')
    let relative = '/' . matchstr(test, '[^!].*')
    if relative =~# '\*'
      let found = !empty(s:nscall(a:ns, 'glob', escape(a:root, '[?*') . relative))
    elseif relative =~# '/$'
      let found = s:nscall(a:ns, 'isdirectory', a:root . relative)
    else
      let found = s:nscall(a:ns, 'filereadable', a:root . relative)
    endif
    if test =~# '^!' ? found : !found
      return 0
    endif
  endfor
  return 1
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
  let ns = matchstr(file, '^\a\a\+\ze:')
  if len(ns) && get(g:, 'projectionist_ignore_' . ns)
    return
  endif
  call s:load(ns)
  let previous = ""
  while root !=# previous && root !=# '.'
    if s:nscall(ns, 'filereadable', root . '/.projections.json')
      try
        let value = projectionist#json_parse(projectionist#readfile(root . '/.projections.json'))
        call projectionist#append(root, value)
      catch /^invalid JSON:/
      endtry
    endif
    for [key, value] in items(g:projectionist_heuristics)
      for test in split(key, '|')
        if s:has(ns, root, test)
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

if !exists('g:did_load_ftplugin')
  filetype plugin on
endif

augroup projectionist
  autocmd!
  autocmd FileType *
        \ if (&filetype !=# 'netrw' || !exists('b:projectionist')) &&
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
        \ if exists('b:NERDTree.root.path.str') |
        \   call ProjectionistDetect(b:NERDTree.root.path.str()) |
        \ endif
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
