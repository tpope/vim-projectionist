" Location:     plugin/projectile.vim
" Author:       Tim Pope <http://tpo.pe/>

if exists("g:autoloaded_projectile")
  finish
endif
let g:autoloaded_projectile = 1

" Section: Utility

function! s:sub(str, pat, repl) abort
  return substitute(a:str, '\v\C'.a:pat, a:repl, '')
endfunction

function! s:gsub(str, pat, repl) abort
  return substitute(a:str, '\v\C'.a:pat, a:repl, 'g')
endfunction

function! s:startswith(str, prefix) abort
  return strpart(a:str, 0, len(a:prefix)) ==# a:prefix
endfunction

function! s:endswith(str, suffix) abort
  return strpart(a:str, len(a:str) - len(a:suffix)) ==# a:suffix
endfunction

function! s:uniq(list) abort
  if exists('*uniq')
    return uniq(a:list)
  endif
  let i = 0
  let seen = {}
  while i < len(a:list)
    let str = string(a:list[i])
    if has_key(seen, str)
      call remove(a:list, i)
    else
      let seen[str] = 1
      let i += 1
    endif
  endwhile
  return a:list
endfunction

function! projectile#lencmp(i1, i2) abort
  return len(a:i1) - len(a:i2)
endfunction

function! projectile#slash() abort
  return exists('+shellslash') && !&shellslash ? '\' : '/'
endfunction

function! projectile#json_parse(string) abort
  let [null, false, true] = ['', 0, 1]
  let string = type(a:string) == type([]) ? join(a:string, ' ') : a:string
  let stripped = substitute(string, '\C"\(\\.\|[^"\\]\)*"', '', 'g')
  if stripped !~# "[^,:{}\\[\\]0-9.\\-+Eaeflnr-u \n\r\t]"
    try
      return eval(substitute(string, "[\r\n]", ' ', 'g'))
    catch
    endtry
  endif
  throw "invalid JSON: ".string
endfunction

" Section: Querying

function! s:paths() abort
  return reverse(sort(keys(b:projectiles), function('projectile#lencmp')))
endfunction

function! projectile#path(...) abort
  let path = get(s:paths(), a:0 > 1 ? a:2 - 1 : 0, '')
  if !empty(path) && a:0
    return path . projectile#slash() . a:1
  else
    return path
  endif
endfunction

function! s:projectiles() abort
  let projectiles = []
  for key in s:paths()
    for value in b:projectiles[key]
      call add(projectiles, [key, value])
    endfor
  endfor
  return projectiles
endfunction

if !exists('g:projectile_transformations')
  let g:projectile_transformations = {}
endif

function! g:projectile_transformations.dot(input, o) abort
  return substitute(a:input, '/', '.', 'g')
endfunction

function! g:projectile_transformations.underscore(input, o) abort
  return substitute(a:input, '/', '_', 'g')
endfunction

function! g:projectile_transformations.backslash(input, o) abort
  return substitute(a:input, '/', '\\', 'g')
endfunction

function! g:projectile_transformations.colons(input, o) abort
  return substitute(a:input, '/', '::', 'g')
endfunction

function! g:projectile_transformations.hyphenate(input, o) abort
  return tr(a:input, '_', '-')
endfunction

function! g:projectile_transformations.blank(input, o) abort
  return tr(a:input, '_-', '  ')
endfunction

function! g:projectile_transformations.uppercase(input, o) abort
  return toupper(a:input)
endfunction

function! g:projectile_transformations.camelcase(input, o) abort
  return substitute(a:input, '[_-]\(.\)', '\u\1', 'g')
endfunction

function! g:projectile_transformations.capitalize(input, o) abort
  return substitute(a:input, '\%(^\|/\)\zs\(.\)', '\u\1', 'g')
endfunction

function! g:projectile_transformations.dirname(input, o) abort
  return substitute(a:input, '.[^'.projectile#slash().'/]*$', '', '')
endfunction

let g:projectile_transformations.head = g:projectile_transformations.dirname

function! g:projectile_transformations.basename(input, o) abort
  return substitute(a:input, '.*['.projectile#slash().'/]', '', '')
endfunction

let g:projectile_transformations.tail = g:projectile_transformations.basename

function! g:projectile_transformations.open(input, o) abort
  return '{'
endfunction

function! g:projectile_transformations.close(input, o) abort
  return '}'
endfunction

function! s:expand_placeholder(placeholder, expansions) abort
  let transforms = split(a:placeholder[1:-2], '|')
  if has_key(a:expansions, get(transforms, 0, '}'))
    let value = a:expansions[remove(transforms, 0)]
  elseif has_key(a:expansions, 'match')
    let value = a:expansions.match
  else
    return "\001"
  endif
  for transform in transforms
    if !has_key(g:projectile_transformations, transform)
      return "\001"
    endif
    let value = g:projectile_transformations[transform](value, a:expansions)
  endfor
  return value
endfunction

function! s:expand_placeholders(value, expansions) abort
  if type(a:value) ==# type([]) || type(a:value) ==# type({})
    return map(copy(a:value), 's:expand_placeholders(v:val, a:expansions)')
  endif
  let legacy = {
        \ '%s': 'replace %s with {}',
        \ '%d': 'replace %d with {dot}',
        \ '%u': 'replace %u with {underscore}'}
  let value = substitute(a:value, '{[^{}]*}', '\=s:expand_placeholder(submatch(0), a:expansions)', 'g')
  let value = substitute(value, '%[sdu]', '\=get(legacy, submatch(0), "\001")', 'g')
  return value =~# "\001" ? '' : value
endfunction

let s:valid_key = '^[^*{}]*\*\=[^*{}]*$'

function! s:match(file, pattern) abort
  let [prefix, suffix; _] = split(a:pattern, '\*', 1)
  if s:startswith(a:file, prefix) && s:endswith(a:file, suffix)
    return tr(name[strlen(prefix) : -strlen(suffix)-1], projectile#slash(), '/')
  endif
  return ''
endfunction

function! projectile#query(key, ...) abort
  let candidates = []
  let file = a:0 > 1 ? a:2 : expand('%:p')
  for [path, projections] in s:projectiles()
    let pre = path . projectile#slash()
    let expansions = extend({'project': path, 'file': file}, a:0 ? a:1 : {})
    let name = file[strlen(path)+1:-1]
    if strpart(file, 0, len(path)) !=# path
      let name = ''
    endif
    if has_key(projections, name) && has_key(projections[name], a:key)
      call add(candidates, [pre, s:expand_placeholders(projections[name][a:key], expansions)])
    endif
    for pattern in reverse(sort(filter(keys(projections), 'v:val =~# s:valid_key && v:val =~# "\\*"'), function('projectile#lencmp')))
      let expansions.match = s:match(name, pattern)
      if (!empty(expansions.match) || pattern ==# '*') && has_key(projections[pattern], a:key)
        call add(candidates, [pre, s:expand_placeholders(projections[pattern][a:key], expansions)])
      endif
    endfor
  endfor
  return candidates
endfunction

function! projectile#query_file(key) abort
  let files = []
  let _ = {}
  for [root, _.match] in projectile#query(a:key)
    call extend(files, map(type(_.match) == type([]) ? copy(_.match) : [_.match], 'root . v:val'))
  endfor
  return files
endfunction

function! projectile#query_exec(key) abort
  return filter(map(projectile#query(a:key), '[v:val[0], s:shellcmd(v:val[1])]'), '!empty(v:val[1])')
endfunction

function! projectile#query_scalar(key) abort
  let values = []
  for [root, match] in projectile#query(a:key)
    if type(match) == type([])
      call extend(values, match)
    elseif type(match) !=# type({})
      call add(values, match)
    endif
    unlet match
  endfor
  return values
endfunction

function! projectile#query_with_alternate(key) abort
  let values = projectile#query(a:key)
  for file in projectile#query_file('alternate')
    for [root, match] in projectile#query(a:key, {}, file)
      if filereadable(file)
        call add(values, [root, match])
      endif
      unlet match
    endfor
  endfor
  return values
endfunction

" Section: Activation

function! projectile#append(root, ...) abort
  if !has_key(b:projectiles, a:root)
    let b:projectiles[a:root] = []
  endif
  call add(b:projectiles[a:root], get(a:000, -1, {}))
endfunction

function! projectile#define_navigation_command(command, patterns)
  for [prefix, excmd] in items(s:prefixes)
    execute 'command! -buffer -bar -bang -nargs=* -complete=customlist,s:projection_complete'
          \ prefix . substitute(a:command, '\A', '', 'g')
          \ ':execute s:open_projection("'.excmd.'<bang>",'.string(a:patterns).',<f-args>)'
  endfor
endfunction

function! s:shellcmd(arg) abort
  if type(a:arg) == type([])
    return join(map(copy(a:arg), 'v:val =~# "^[[:alnum:]_/.:=-]\\+$" ? v:val : shellescape(v:val)'), ' ')
  elseif type(a:arg) == type('')
    return a:arg
  endif
endfunction

function! projectile#activate() abort
  if empty(b:projectiles)
    finish
  endif
  command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Cd
        \ exe 'cd<bang>'  fnameescape(projectile#path(<q-args>, <line2>))
  command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Lcd
        \ exe 'lcd<bang>' fnameescape(projectile#path(<q-args>, <line2>))
  for [command, patterns] in items(projectile#navigation_commands())
    call projectile#define_navigation_command(command, patterns)
  endfor
  for [prefix, excmd] in items(s:prefixes)
    execute 'command! -buffer -bar -bang -nargs=* -range=1 -complete=customlist,s:edit_complete'
          \ 'A'.prefix
          \ ':execute s:edit_command("'.excmd.'<bang>", <line2>, <f-args>)'
  endfor
  command! -buffer -bar -bang -nargs=* -complete=customlist,s:edit_complete A AE<bang> <args>
  command! -buffer -bang -nargs=1 -range=0 -complete=command ProjectDo execute s:do('<bang>', <count>==<line1>?<count>:-1, <q-args>)

  for [root, makeopt] in projectile#query('make')
    let makeprg = s:shellcmd(makeopt)
    if empty(makeprg)
      continue
    endif
    unlet! b:current_compiler
    setlocal errorformat<
    let compiler = fnamemodify(matchstr(makeprg, '\S\+'), ':t:r')
    if exists(':Dispatch')
      silent! let compiler = dispatch#compiler_for_program(makeprg)
    endif
    if !empty(findfile('compiler/'.compiler.'.vim', escape(&rtp, ' ')))
      execute 'compiler' compiler
    endif
    let &l:makeprg = makeprg
    if type(makeopt) ==# type([]) && empty(filter(copy(makeopt), 'stridx(v:val, root) >= 0'))
      let &l:errorformat .= ',projectile.vim@'.escape(root, ',')
    endif
    break
  endfor

  for [root, command] in projectile#query_exec('start')
    let offset = index(s:paths(), root[0:-2]) + 1
    let b:start = ':ProjectDo ' . (offset == 1 ? '' : offset) . ' ' .
          \ substitute('Start '.command, 'Start :', '', '')
    break
  endfor

  for [root, dispatch] in projectile#query_with_alternate('dispatch')
    let command = s:shellcmd(dispatch)
    let offset = index(s:paths(), root[0:-2]) + 1
    if !empty(command)
      let b:dispatch = ':ProjectDo ' . (offset == 1 ? '' : offset) . ' ' .
            \ substitute('Dispatch '.command, 'Dispatch :', '', '')
      break
    endif
    unlet dispatch b:dispatch
  endfor

  silent doautocmd User ProjectileActivate
endfunction

" Section: Completion

function! s:completion_filter(results, A) abort
  let sep = projectile#slash()
  let results = s:uniq(sort(copy(a:results)))
  call filter(results,'v:val !~# "\\~$"')
  let filtered = filter(copy(results),'v:val[0:strlen(a:A)-1] ==# a:A')
  if !empty(filtered) | return filtered | endif
  let regex = s:gsub(a:A,'[^'.sep.']','[&].*')
  let filtered = filter(copy(results),'v:val =~# "^".regex')
  if !empty(filtered) | return filtered | endif
  let filtered = filter(copy(results),'sep.v:val =~# ''['.sep.']''.regex')
  if !empty(filtered) | return filtered | endif
  let regex = s:gsub(a:A,'.','[&].*')
  let filtered = filter(copy(results),'sep.v:val =~# regex')
  return filtered
endfunction

function! s:dir_complete(lead, cmdline, _) abort
  let base = substitute(a:lead, '^[\/]', '', '')
  let slash = projectile#slash()
  let c = matchstr(a:cmdline, '^\d\+')
  let matches = split(glob(projectile#path(substitute(base, '[\/]', '*&',  'g') . '*' . slash, c ? c : 1)), "\n")
  call map(matches,'matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectile#path())+1 : -1 ]')
  return matches
endfunction

" Section: Navigation commands

let s:prefixes = {
      \ 'E': 'edit',
      \ 'S': 'split',
      \ 'V': 'vsplit',
      \ 'T': 'tabedit',
      \ 'D': 'read'}

function! projectile#navigation_commands() abort
  let commands = {}
  for [path, projections] in s:projectiles()
    for [pattern, projection] in items(projections)
      let name = s:gsub(get(projection, 'command', get(projection, 'type', get(projection, 'name', ''))), '\A', '')
      if !empty(name) && pattern =~# s:valid_key
        if !has_key(commands, name)
          let commands[name] = []
        endif
        let command = [path, pattern]
        call add(commands[name], command)
      endif
    endfor
  endfor
  call filter(commands, '!empty(v:val)')
  return commands
endfunction

function! s:open_projection(cmd, variants, ...) abort
  let formats = []
  for variant in a:variants
    call add(formats, variant[0] . projectile#slash() . substitute(variant[1], '\*', '%s', ''))
  endfor
  if a:0 && a:1 ==# '&'
    let s:last_formats = formats
    return ''
  endif
  if a:0
    call filter(formats, 'v:val =~# "%s"')
    call map(formats, 'printf(v:val, a:1)')
  else
    call filter(formats, 'v:val !~# "%s"')
  endif
  if empty(formats)
    return 'echoerr "Invalid number of arguments"'
  endif
  let target = formats[0]
  for format in formats
    if filereadable(format)
      let target = format
    endif
  endfor
  if !isdirectory(fnamemodify(target, ':h'))
    call mkdir(fnamemodify(target, ':h'), 'p')
  endif
  return a:cmd . ' ' . fnameescape(target)
endfunction

function! s:projection_complete(lead, cmdline, _) abort
  execute matchstr(a:cmdline, '\a\@<![' . join(keys(s:prefixes), '') . ']\w\+') . ' &'
  let results = []
  for format in s:last_formats
    if format !~# '%s'
      continue
    endif
    let [before, after] = split(format, '%s', 1)
    let dir = matchstr(before,'.*/')
    let prefix = matchstr(before,'.*/\zs.*')
    let matches = split(glob(dir . '**/*' . after), "\n")
    call filter(matches, 'v:val[strlen(dir) : strlen(before)-1] ==# prefix')
    let results += map(matches, 'v:val[strlen(before) : -strlen(after)-1]')
  endfor
  return s:completion_filter(results, a:lead)
endfunction

" Section: :A

function! s:edit_command(cmd, count, ...) abort
  if a:0
    let file = projectile#path(a:1, a:count)
    if empty(file)
      return 'echoerr "Invalid count"'
    endif
  else
    let alternates = projectile#query_file('alternate')
    let warning = get(filter(copy(alternates), 'v:val =~# "replace %.*}"'), 0, '')
    if !empty(warning)
      return 'echoerr '.string(matchstr(warning, 'replace %.*}').' in alternate projection')
    endif
    let file = get(filter(copy(alternates), '!empty(getftype(v:val))'), 0, '')
    if empty(file)
      return 'echoerr "No alternate file"'
    endif
  endif
  if !isdirectory(fnamemodify(file, ':h'))
    call mkdir(fnamemodify(file, ':h'), 'p')
  endif
  return a:cmd . ' ' . fnameescape(file)
endfunction

function! s:edit_complete(lead, cmdline, _) abort
  let base = substitute(a:lead, '^[\/]', '', '')
  let c = matchstr(a:cmdline, '^\d\+')
  let matches = split(glob(projectile#path(substitute(base, '[\/]', '*&',  'g') . '*', c ? c : 1)), "\n")
  call map(matches, 'matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectile#path())+1 : -1 ] . (isdirectory(v:val) ? projectile#slash() : "")')
  return matches
endfunction

" Section: :ProjectDo

function! s:do(bang, count, cmd) abort
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  let cmd = substitute(a:cmd, '^\d\+ ', '', '')
  let offset = cmd ==# a:cmd ? 1 : matchstr(a:cmd, '^\d\+')
  try
    execute cd fnameescape(projectile#path('', offset))
    execute (a:count >= 0 ? a:count : '').substitute(cmd, '\>', a:bang, '')
  catch
    return 'echoerr '.string(v:exception)
  finally
    execute cd fnameescape(cwd)
  endtry
  return ''
endfunction

" Section: Make

function! s:qf_pre() abort
  let dir = substitute(matchstr(&l:errorformat, 'projectile\.vim@\zs\%(\\.\|[^,]\)*'), '\\,' ,',', 'g')
  if !empty(dir)
    let cwd = getcwd()
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    execute cd fnameescape(dir)
    let s:qf_post = cd . ' ' . fnameescape(cwd)
  endif
endfunction

augroup projectile_make
  autocmd!
  autocmd QuickFixCmdPre  dispatch,make,lmake call s:qf_pre()
  autocmd QuickFixCmdPost dispatch,make,lmake
        \ if exists('s:qf_post') | execute remove(s:, 'qf_post') | endif
augroup END

" Section: Templates

function! projectile#apply_template() abort
  let template = get(projectile#query('template'), 0, ['', ''])[1]
  if type(template) == type([])
    let l:.template = join(template, "\n")
  endif
  if !empty(template)
    let template = s:gsub(template, '\t', repeat(' ', &sw))
    if !&et
      let template = s:gsub(template, repeat(' ', &ts), "\t")
    endif
    %delete_
    call setline(1, split(template, "\n"))
    setlocal nomodified
    doautocmd BufReadPost
  endif
  return ''
endfunction
