" Location:     autoload/projectionist.vim
" Author:       Tim Pope <http://tpo.pe/>

if exists("g:autoloaded_projectionist")
  finish
endif
let g:autoloaded_projectionist = 1

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

function! projectionist#lencmp(i1, i2) abort
  return len(a:i1) - len(a:i2)
endfunction

function! projectionist#slash() abort
  return exists('+shellslash') && !&shellslash ? '\' : '/'
endfunction

function! s:slash(str)
  return tr(a:str, projectionist#slash(), '/')
endfunction

function! projectionist#json_parse(string) abort
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
  return reverse(sort(keys(b:projectionist), function('projectionist#lencmp')))
endfunction

function! projectionist#path(...) abort
  let path = get(s:paths(), a:0 > 1 ? a:2 - 1 : 0, '')
  if !empty(path) && a:0
    return path . projectionist#slash() . a:1
  else
    return path
  endif
endfunction

function! s:all() abort
  let all = []
  for key in s:paths()
    for value in b:projectionist[key]
      call add(all, [key, value])
    endfor
  endfor
  return all
endfunction

if !exists('g:projectionist_transformations')
  let g:projectionist_transformations = {}
endif

function! g:projectionist_transformations.dot(input, o) abort
  return substitute(a:input, '/', '.', 'g')
endfunction

function! g:projectionist_transformations.underscore(input, o) abort
  return substitute(a:input, '/', '_', 'g')
endfunction

function! g:projectionist_transformations.backslash(input, o) abort
  return substitute(a:input, '/', '\\', 'g')
endfunction

function! g:projectionist_transformations.colons(input, o) abort
  return substitute(a:input, '/', '::', 'g')
endfunction

function! g:projectionist_transformations.hyphenate(input, o) abort
  return tr(a:input, '_', '-')
endfunction

function! g:projectionist_transformations.blank(input, o) abort
  return tr(a:input, '_-', '  ')
endfunction

function! g:projectionist_transformations.uppercase(input, o) abort
  return toupper(a:input)
endfunction

function! g:projectionist_transformations.camelcase(input, o) abort
  return substitute(a:input, '\%([_-]\|/\@<=\|^\)\(.\)', '\u\1', 'g')
endfunction

function! g:projectionist_transformations.capitalize(input, o) abort
  return substitute(a:input, '\%(^\|/\)\zs\(.\)', '\u\1', 'g')
endfunction

function! g:projectionist_transformations.dirname(input, o) abort
  return substitute(a:input, '.[^'.projectionist#slash().'/]*$', '', '')
endfunction

let g:projectionist_transformations.head = g:projectionist_transformations.dirname

function! g:projectionist_transformations.basename(input, o) abort
  return substitute(a:input, '.*['.projectionist#slash().'/]', '', '')
endfunction

let g:projectionist_transformations.tail = g:projectionist_transformations.basename

function! g:projectionist_transformations.open(input, o) abort
  return '{'
endfunction

function! g:projectionist_transformations.close(input, o) abort
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
    if !has_key(g:projectionist_transformations, transform)
      return "\001"
    endif
    let value = g:projectionist_transformations[transform](value, a:expansions)
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

let s:valid_key = '^\%([^*{}]*\*\*[^*{}]\{2\}\)\=[^*{}]*\*\=[^*{}]*$'

function! s:match(file, pattern) abort
  if a:pattern =~# '^[^*{}]*\*[^*{}]*$'
    let pattern = s:slash(substitute(a:pattern, '\*', '**/*', ''))
  elseif a:pattern =~# '^[^*{}]*\*\*[^*{}]\+\*[^*{}]*$'
    let pattern = s:slash(a:pattern)
  else
    return ''
  endif
  let [prefix, infix, suffix] = split(pattern, '\*\*\=', 1)
  let file = s:slash(a:file)
  if !s:startswith(file, prefix) || !s:endswith(file, suffix)
    return ''
  endif
  let match = file[strlen(prefix) : -strlen(suffix)-1]
  if infix ==# '/'
    return match
  endif
  let clean = substitute('/'.match, '\V'.infix.'\ze\[^/]\*\$', '/', '')[1:-1]
  return clean ==# match ? '' : clean
endfunction

function! projectionist#query_raw(key, ...) abort
  let candidates = []
  let file = a:0 ? a:1 : get(b:, 'projectionist_file', expand('%:p'))
  for [path, projections] in s:all()
    let pre = path . projectionist#slash()
    let attrs = {'project': path, 'file': file}
    let name = file[strlen(path)+1:-1]
    if strpart(file, 0, len(path)) !=# path
      let name = ''
    endif
    if has_key(projections, name) && has_key(projections[name], a:key)
      call add(candidates, [projections[name][a:key], attrs])
    endif
    for pattern in reverse(sort(filter(keys(projections), 'v:val =~# s:valid_key && v:val =~# "\\*"'), function('projectionist#lencmp')))
      let match = s:match(name, pattern)
      if (!empty(match) || pattern ==# '*') && has_key(projections[pattern], a:key)
        let expansions = extend({'match': match}, attrs)
        call add(candidates, [projections[pattern][a:key], expansions])
      endif
    endfor
  endfor
  return candidates
endfunction

function! projectionist#query(key, ...) abort
  let candidates = []
  let file = a:0 > 1 ? a:2 : get(a:0 ? a:1 : {}, 'file', get(b:, 'projectionist_file', expand('%:p')))
  for [value, expansions] in projectionist#query_raw(a:key, file)
    call extend(expansions, a:0 ? a:1 : {}, 'keep')
    call add(candidates, [expansions.project, s:expand_placeholders(value, expansions)])
    unlet value
  endfor
  return candidates
endfunction

function! projectionist#query_file(key) abort
  let files = []
  let _ = {}
  for [root, _.match] in projectionist#query(a:key)
    call extend(files, map(type(_.match) == type([]) ? copy(_.match) : [_.match], 'simplify(root . projectionist#slash() . v:val)'))
  endfor
  return files
endfunction

function! projectionist#query_exec(key, ...) abort
  return filter(map(projectionist#query(a:key, a:0 ? a:1 : {}), '[v:val[0], s:shellcmd(v:val[1])]'), '!empty(v:val[1])')
endfunction

function! projectionist#query_scalar(key) abort
  let values = []
  for [root, match] in projectionist#query(a:key)
    if type(match) == type([])
      call extend(values, match)
    elseif type(match) !=# type({})
      call add(values, match)
    endif
    unlet match
  endfor
  return values
endfunction

function! s:query_exec_with_alternate(key) abort
  let values = projectionist#query_exec(a:key)
  for file in projectionist#query_file('alternate')
    for [root, match] in projectionist#query_exec(a:key, {'file': file})
      if filereadable(file)
        call add(values, [root, match])
      endif
      unlet match
    endfor
  endfor
  return values
endfunction

" Section: Activation

function! projectionist#append(root, ...) abort
  if !has_key(b:projectionist, a:root)
    let b:projectionist[a:root] = []
  endif
  call add(b:projectionist[a:root], get(a:000, -1, {}))
endfunction

function! projectionist#define_navigation_command(command, patterns)
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

function! projectionist#activate() abort
  if empty(b:projectionist)
    finish
  endif
  command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Cd
        \ exe 'cd<bang>'  fnameescape(projectionist#path(<q-args>, <line2>))
  command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Lcd
        \ exe 'lcd<bang>' fnameescape(projectionist#path(<q-args>, <line2>))
  for [command, patterns] in items(projectionist#navigation_commands())
    call projectionist#define_navigation_command(command, patterns)
  endfor
  for [prefix, excmd] in items(s:prefixes)
    execute 'command! -buffer -bar -bang -nargs=* -range=1 -complete=customlist,s:edit_complete'
          \ 'A'.prefix
          \ ':execute s:edit_command("'.excmd.'<bang>", <line2>, <f-args>)'
  endfor
  command! -buffer -bar -bang -nargs=* -complete=customlist,s:edit_complete A AE<bang> <args>
  command! -buffer -bang -nargs=1 -range=0 -complete=command ProjectDo execute s:do('<bang>', <count>==<line1>?<count>:-1, <q-args>)

  for [root, makeopt] in projectionist#query('make')
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
    if !(type(makeopt) ==# type([]) && !empty(filter(copy(makeopt), 'stridx(v:val, root) >= 0')))
      let &l:errorformat .= ',chdir '.escape(root, ',')
    endif
    break
  endfor

  for [root, command] in projectionist#query_exec('start')
    let offset = index(s:paths(), root) + 1
    let b:start = ':ProjectDo ' . (offset == 1 ? '' : offset.' ') .
          \ substitute('Start '.command, 'Start :', '', '')
    break
  endfor

  for [root, command] in s:query_exec_with_alternate('dispatch')
    let offset = index(s:paths(), root) + 1
    let b:dispatch = ':ProjectDo ' . (offset == 1 ? '' : offset.' ') .
          \ substitute('Dispatch '.command, 'Dispatch :', '', '')
    break
  endfor

  silent doautocmd User ProjectileActivate
  silent doautocmd User ProjectionistActivate
endfunction

" Section: Completion

function! s:completion_filter(results, A) abort
  let sep = projectionist#slash()
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
  let slash = projectionist#slash()
  let c = matchstr(a:cmdline, '^\d\+')
  let matches = split(glob(projectionist#path(substitute(base, '[\/]', '*&',  'g') . '*' . slash, c ? c : 1)), "\n")
  call map(matches,'matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectionist#path())+1 : -1 ]')
  return matches
endfunction

" Section: Navigation commands

let s:prefixes = {
      \ 'E': 'edit',
      \ 'S': 'split',
      \ 'V': 'vsplit',
      \ 'T': 'tabedit',
      \ 'D': 'read'}

function! projectionist#navigation_commands() abort
  let commands = {}
  for [path, projections] in s:all()
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
    call add(formats, variant[0] . projectionist#slash() . (variant[1] =~# '\*\*'
          \ ? variant[1] : substitute(variant[1], '\*', '**/*', '')))
  endfor
  if a:0 && a:1 ==# '&'
    let s:last_formats = formats
    return ''
  endif
  if a:0
    call filter(formats, 'v:val =~# "\\*"')
    let dir = matchstr(a:1, '.*\ze/')
    let base = matchstr(a:1, '[^\/]*$')
    call map(formats, 'substitute(substitute(v:val, "\\*\\*\\([\\/]\\=\\)", empty(dir) ? "" : dir . "\\1", ""), "\\*", base, "")')
  else
    call filter(formats, 'v:val !~# "\\*"')
  endif
  if empty(formats)
    return 'echoerr "Invalid number of arguments"'
  endif
  let target = formats[0]
  for format in formats
    if filereadable(format)
      let target = format
      break
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
    if format !~# '\*'
      continue
    endif
    let glob = substitute(format, '[^\/]*\ze\*\*[\/]\*', '', 'g')
    let results += map(split(glob(glob), "\n"), 's:match(v:val, format)')
  endfor
  return s:completion_filter(results, a:lead)
endfunction

" Section: :A

function! s:edit_command(cmd, count, ...) abort
  if a:0
    let file = projectionist#path(a:1, a:count)
    if empty(file)
      return 'echoerr "Invalid count"'
    endif
  else
    let alternates = projectionist#query_file('alternate')
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
  let matches = split(glob(projectionist#path(substitute(base, '[\/]', '*&',  'g') . '*', c ? c : 1)), "\n")
  call map(matches, 'matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectionist#path())+1 : -1 ] . (isdirectory(v:val) ? projectionist#slash() : "")')
  return matches
endfunction

" Section: :ProjectDo

function! s:do(bang, count, cmd) abort
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  let cmd = substitute(a:cmd, '^\d\+ ', '', '')
  let offset = cmd ==# a:cmd ? 1 : matchstr(a:cmd, '^\d\+')
  try
    execute cd fnameescape(projectionist#path('', offset))
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
  let dir = substitute(matchstr(','.&l:errorformat, ',chdir \zs\%(\\.\|[^,]\)*'), '\\,' ,',', 'g')
  let cwd = getcwd()
  if !empty(dir) && dir !=# cwd
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    execute cd fnameescape(dir)
    let s:qf_post = cd . ' ' . fnameescape(cwd)
  endif
endfunction

augroup projectionist_make
  autocmd!
  autocmd QuickFixCmdPre  dispatch,*make call s:qf_pre()
  autocmd QuickFixCmdPost dispatch,*make
        \ if exists('s:qf_post') | execute remove(s:, 'qf_post') | endif
augroup END

" Section: Templates

function! projectionist#apply_template() abort
  let template = get(projectionist#query('template'), 0, ['', ''])[1]
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
