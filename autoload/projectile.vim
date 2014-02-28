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

function! projectile#path(...) abort
  let path = get(sort(keys(b:projectiles), function('projectile#lencmp')), -1, '')
  if !empty(path) && a:0
    return path . projectile#slash() . a:1
  else
    return path
  endif
endfunction

function! s:projectiles() abort
  let projectiles = []
  for key in reverse(sort(keys(b:projectiles), function('projectile#lencmp')))
    for value in b:projectiles[key]
      call add(projectiles, [key, value])
    endfor
  endfor
  return projectiles
endfunction

function! s:expand_placeholders(string, placeholders) abort
  if type(a:string) ==# type([]) || type(a:string) ==# type({})
    return map(copy(a:string), 's:expand_placeholders(v:val, a:placeholders)')
  endif
  let ph = extend({'%%': '%'}, a:placeholders)
  let value = substitute(a:string, '%[^: ]\|{[^{}]*}', '\=get(ph, submatch(0), "\001")', 'g')
  return value =~# "\001" ? '' : value
endfunction

function! projectile#query(key) abort
  let candidates = []
  for [path, projections] in s:projectiles()
    let pre = path . projectile#slash()
    let name = expand('%:p')[strlen(path)+1:-1]
    if has_key(projections, name) && has_key(projections[name], a:key)
      call add(candidates, [pre, projections[name][a:key]])
    endif
    for pattern in reverse(sort(filter(keys(projections), 'v:val =~# "^[^*]*\\*[^*]*$"'), function('projectile#lencmp')))
      let [prefix, suffix; _] = split(pattern, '\*', 1)
      if s:startswith(name, prefix) && s:endswith(name, suffix) && has_key(projections[pattern], a:key)
        let root = tr(name[strlen(prefix) : -strlen(suffix)-1], projectile#slash(), '/')
        let ph = {
              \ '{}': root,
              \ '%s': root,
              \ '%d': tr(root, '/', '.'),
              \ '%u': tr(root, '/', '_'),
              \ }
        call add(candidates, [pre, s:expand_placeholders(projections[pattern][a:key], ph)])
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

function! projectile#query_scalar(key) abort
  let files = []
  let _ = {}
  for [root, _.match] in projectile#query(a:key)
    call extend(files, type(_.match) == type([]) ? _.match : [_.match])
  endfor
  return files
endfunction

" Section: Activation

function! projectile#append(root, value) abort
  if !has_key(b:projectiles, a:root)
    let b:projectiles[a:root] = []
  endif
  call add(b:projectiles[a:root], a:value)
endfunction

function! projectile#activate() abort
  if empty(b:projectiles)
    finish
  endif
  command! -buffer -bar -bang -nargs=? -complete=customlist,s:dir_complete Cd   :cd<bang>  `=projectile#path(<q-args>)`
  command! -buffer -bar -bang -nargs=? -complete=customlist,s:dir_complete Lcd  :lcd<bang> `=projectile#path(<q-args>)`
  let commands = items(projectile#commands())
  for [prefix, excmd] in items(s:commands)
    execute 'command! -buffer -bar -bang -nargs=* -complete=customlist,s:edit_complete'
          \ 'A'.prefix
          \ ':execute s:edit_command("'.excmd.'<bang>",<f-args>)'
    for [command, patterns] in commands
      execute 'command! -buffer -bar -bang -nargs=* -complete=customlist,s:projection_complete'
            \ prefix . substitute(command, '\s', '', 'g')
            \ ':execute s:open_projection("'.excmd.'<bang>",'.string(patterns).',<f-args>)'
    endfor
  endfor
  command! -buffer -bar -bang -nargs=* -complete=customlist,s:edit_complete A AE<bang> <args>
  for compiler in projectile#query_scalar('compiler')
    if !empty(findfile('compiler/'.compiler.'.vim', &rtp))
      execute 'compiler '.compiler
      break
    endif
  endfor
  for b:dispatch in projectile#query_scalar('dispatch')
    break
  endfor
  for &l:shiftwidth in projectile#query_scalar('indent')
    break
  endfor
  silent doautocmd User ProjectileActivate
endfunction

" Section: Completion

function! s:completion_filter(results, A) abort
  let sep = projectile#slash()
  let results = sort(copy(a:results))
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

function! s:dir_complete(lead, _, __) abort
  let base = substitute(a:lead, '^[\/]', '', '')
  let slash = projectile#slash()
  let matches = split(glob(projectile#path(substitute(base, '[\/]', '*&',  'g') . '*' . slash)), "\n")
  call map(matches,'matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectile#path())+1 : -1 ]')
  return matches
endfunction

" Section: Navigation commands

let s:commands = {
      \ 'E': 'edit',
      \ 'S': 'split',
      \ 'V': 'vsplit',
      \ 'T': 'tabedit',
      \ 'D': 'read'}

function! projectile#commands() abort
  let commands = {}
  for [path, projections] in s:projectiles()
    for [pattern, projection] in items(projections)
      if has_key(projection, 'command')
        let name = projection.command
        if !has_key(commands, name)
          let commands[name] = []
        endif
        let command = {'root': path, 'pattern': pattern}
        call extend(commands[name], [command])
      endif
    endfor
  endfor
  call filter(commands, '!empty(v:val)')
  return commands
endfunction

function! s:open_projection(cmd, variants, ...) abort
  let formats = []
  for variant in a:variants
    call add(formats, variant.root . projectile#slash() . substitute(variant.pattern, '\*', '%s', ''))
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

function! s:projection_commands_pattern()
  let command_patterns = []
  for prefix in keys(s:commands)
    for suffix in keys(projectile#commands())
      let command = prefix . suffix
      for i in range(len(command) - 1, 0, -1)
        if exists(':' . strpart(command, 0, i)) != 1
          let cutoff = i + 1
          break
        endif
      endfor
      if cutoff == len(command)
        let command_patterns += [command]
      else
        let command_patterns += [strpart(command, 0, cutoff) . '%[' . strpart(command, cutoff) . ']']
      endif
    endfor
  endfor
  return '\v<(' . join(command_patterns, '|') . ')>'
endfunction

function! s:projection_complete(lead, cmdline, _) abort
  execute matchstr(a:cmdline, s:projection_commands_pattern()) . ' &'
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

function! s:edit_command(cmd, ...) abort
  if a:0
    let file = a:1
  else
    let alternates = projectile#query_file('alternate')
    if empty(alternates)
      return 'echoerr "No alternate file"'
    endif
    let file = get(filter(copy(alternates), '!empty(getftype(v:val))'), 0, alternates[0])
  endif
  if !isdirectory(fnamemodify(file, ':h'))
    call mkdir(fnamemodify(file, ':h'), 'p')
  endif
  return a:cmd . ' ' . fnameescape(file)
endfunction

function! s:edit_complete(lead, _, __) abort
  let base = substitute(a:lead, '^[\/]', '', '')
  let matches = split(glob(projectile#path(substitute(base, '[\/]', '*&',  'g') . '*')), "\n")
  call map(matches, 'matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectile#path())+1 : -1 ] . (isdirectory(v:val) ? projectile#slash() : "")')
  return matches
endfunction

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
