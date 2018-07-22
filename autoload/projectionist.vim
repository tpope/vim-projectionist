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

function! s:real(file) abort
  let pre = substitute(matchstr(a:file, '^\a\a\+\ze:'), '^.', '\u&', '')
  if empty(pre)
    let path = a:file
  elseif exists('*' . pre . 'Real')
    let path = {pre}Real(a:file)
  elseif exists('*' . pre . 'Path')
    let path = {pre}Path(a:file)
  else
    return ''
  endif
  return exists('+shellslash') && !&shellslash ? tr(path, '/', '\') : path
endfunction

function! projectionist#slash() abort
  return exists('+shellslash') && !&shellslash ? '\' : '/'
endfunction

function! s:slash(str) abort
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

function! projectionist#shellescape(arg) abort
  return a:arg =~# "^[[:alnum:]_/.:-]\\+$" ? a:arg : shellescape(a:arg)
endfunction

function! projectionist#shellpath(arg) abort
  return projectionist#shellescape(s:real(a:arg))
endfunction

function! s:join(arg) abort
  if type(a:arg) == type([])
    return join(a:arg, ' ')
  elseif type(a:arg) == type('')
    return a:arg
  else
    return ''
  endif
endfunction

function! s:parse(mods, args) abort
  let flags = ''
  let pres = []
  let cmd = {'args': []}
  if a:mods ==# '' || a:mods ==# '<mods>'
    let cmd.mods = ''
  else
    let cmd.mods = a:mods . ' '
  endif
  let args = copy(a:args)
  while !empty(args)
    if args[0] =~# "^++mods="
      let cmd.mods .= args[0][8:-1] . ' '
    elseif args[0] =~# '^++'
      let flags .= ' ' . args[0]
    elseif args[0] =~# '^+.'
      call add(pres, args[0][1:-1])
    elseif args[0] !=# '+'
      call add(cmd.args, args[0])
    endif
    call remove(args, 0)
  endwhile

  let cmd.pre = flags . (empty(pres) ? '' : ' +'.escape(join(pres, '|'), '| '))
  return cmd
endfunction

function! s:fcall(fn, path, ...) abort
  let ns = matchstr(a:path, '^\a\a\+\ze:')
  if len(ns) && exists('*' . ns . '#' . a:fn)
    return call(ns . '#' . a:fn, [a:path] + a:000)
  else
    return call(a:fn, [a:path] + a:000)
  endif
endfunction

function! s:mkdir_p(path) abort
  if a:path !~# '^\a[[:alnum:].+-]\+:' && !isdirectory(a:path)
    call mkdir(a:path, 'p')
  endif
endfunction

function! projectionist#glob(path) abort
  let ns = matchstr(a:path, '^\a\a\+\ze:')
  if len(ns) && exists('*' . ns . '#glob')
    return call(ns . '#glob', [a:path, 0, 1])
  else
    return split(glob(a:path), "\n")
  endif
endfunction

" Section: Querying

function! s:roots() abort
  return reverse(sort(keys(get(b:, 'projectionist', {})), function('projectionist#lencmp')))
endfunction

function! projectionist#path(...) abort
  if a:0 && a:1 =~# '^/\|^\a\+:'
    return a:1
  endif
  if a:0 > 1 && type(a:2) == type('')
    let root = a:2
  else
    let root = get(s:roots(), a:0 > 1 ? a:2 - 1 : 0, '')
  endif
  if !empty(root) && a:0
    return root . projectionist#slash() . a:1
  else
    return root
  endif
endfunction

function! s:all() abort
  let all = []
  for key in s:roots()
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
  return substitute(a:input, '[_-]\(.\)', '\u\1', 'g')
endfunction

function! g:projectionist_transformations.capitalize(input, o) abort
  return substitute(a:input, '\%(^\|/\)\zs\(.\)', '\u\1', 'g')
endfunction

function! g:projectionist_transformations.snakecase(input, o) abort
  let str = a:input
  let str = substitute(str, '\v(\u+)(\u\l)', '\1_\2', 'g')
  let str = substitute(str, '\v(\l|\d)(\u)', '\1_\2', 'g')
  let str = tolower(str)
  return str
endfunction

function! g:projectionist_transformations.dirname(input, o) abort
  return substitute(a:input, '.[^'.projectionist#slash().'/]*$', '', '')
endfunction

function! g:projectionist_transformations.basename(input, o) abort
  return substitute(a:input, '.*['.projectionist#slash().'/]', '', '')
endfunction

function! g:projectionist_transformations.singular(input, o) abort
  let input = a:input
  let input = s:sub(input, '%([Mm]ov|[aeio])@<!ies$', 'ys')
  let input = s:sub(input, '[rl]@<=ves$', 'fs')
  let input = s:sub(input, '%(nd|rt)@<=ices$', 'exs')
  let input = s:sub(input, 's@<!s$', '')
  let input = s:sub(input, '%([nrt]ch|tatus|lias|ss)@<=e$', '')
  return input
endfunction

function! g:projectionist_transformations.plural(input, o) abort
  let input = a:input
  let input = s:sub(input, '[aeio]@<!y$', 'ie')
  let input = s:sub(input, '[rl]@<=f$', 've')
  let input = s:sub(input, '%(nd|rt)@<=ex$', 'ice')
  let input = s:sub(input, '%([osxz]|[cs]h)$', '&e')
  let input .= 's'
  return input
endfunction

function! g:projectionist_transformations.open(input, o) abort
  return '{'
endfunction

function! g:projectionist_transformations.close(input, o) abort
  return '}'
endfunction

function! g:projectionist_transformations.nothing(input, o) abort
  return ''
endfunction

function! s:expand_placeholder(placeholder, expansions) abort
  let transforms = split(a:placeholder[1:-2], '|')
  if has_key(a:expansions, get(transforms, 0, '}'))
    let value = a:expansions[remove(transforms, 0)]
  else
    let value = get(a:expansions, 'match', "\030")
  endif
  for transform in transforms
    if !has_key(g:projectionist_transformations, transform)
      return "\030"
    endif
    let value = g:projectionist_transformations[transform](value, a:expansions)
    if value =~# "\030"
      return "\030"
    endif
  endfor
  if has_key(a:expansions, 'post_function')
    let value = call(a:expansions.post_function, [value])
  endif
  return value
endfunction

function! s:expand_placeholders(value, expansions, ...) abort
  if type(a:value) ==# type([]) || type(a:value) ==# type({})
    return filter(map(copy(a:value), 's:expand_placeholders(v:val, a:expansions, 1)'), 'type(v:val) !=# type("") || v:val !~# "[\001-\006\016-\037]"')
  endif
  let value = substitute(a:value, '{[^{}]*}', '\=s:expand_placeholder(submatch(0), a:expansions)', 'g')
  return !a:0 && value =~# "[\001-\006\016-\037]" ? '' : value
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

function! s:absolute(path, in) abort
  if a:path =~# '^\%([[:alnum:].-]\+:\)\|^\.\?[\/]'
    return a:path
  else
    return substitute(a:in, '[\/]$', '', '') . projectionist#slash() . a:path
  endif
endfunction

function! projectionist#query_file(key, ...) abort
  let files = []
  let _ = {}
  for [root, _.match] in projectionist#query(a:key, a:0 ? a:1 : {})
    call extend(files, map(filter(type(_.match) == type([]) ? copy(_.match) : [_.match], 'len(v:val)'), 's:absolute(v:val, root)'))
  endfor
  return s:uniq(files)
endfunction

function! s:shelljoin(val) abort
  return substitute(s:join(a:val), '["'']\([{}]\)["'']', '\1', 'g')
endfunction

function! projectionist#query_exec(key, ...) abort
  let opts = extend({'post_function': 'projectionist#shellpath'}, a:0 ? a:1 : {})
  return filter(map(projectionist#query(a:key, opts), '[s:real(v:val[0]), s:shelljoin(v:val[1])]'), '!empty(v:val[0]) && !empty(v:val[1])')
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
  let projections = get(a:000, -1, {})
  if type(projections) == type({})
    call add(b:projectionist[a:root], filter(projections, 'type(v:val) == type({})'))
  endif
endfunction

function! projectionist#define_navigation_command(command, patterns) abort
  for [prefix, excmd] in items(s:prefixes)
    execute 'command! -buffer -bar -bang -nargs=* -complete=customlist,s:projection_complete'
          \ prefix . substitute(a:command, '\A', '', 'g')
          \ ':execute s:open_projection("<mods>", "'.excmd.'<bang>",'.string(a:patterns).',<f-args>)'
  endfor
endfunction

function! projectionist#activate() abort
  if empty(b:projectionist)
    return
  endif
  if len(s:real(s:roots()[0]))
    command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Pcd
          \ exe 'cd' s:real(projectionist#path(<q-args>, <line2>))
    command! -buffer -bar -bang -nargs=* -range=1 -complete=customlist,s:dir_complete Plcd
          \ exe (<bang>0 ? 'cd' : 'lcd') s:real(projectionist#path(<q-args>, <line2>))
    if exists(':Cd') != 2
      command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Cd
            \ exe 'cd' s:real(projectionist#path(<q-args>, <line2>))
    endif
    if exists(':Lcd') != 2
      command! -buffer -bar -bang -nargs=? -range=1 -complete=customlist,s:dir_complete Lcd
            \ exe (<bang>0 ? 'cd' : 'lcd') s:real(projectionist#path(<q-args>, <line2>))
    endif
    command! -buffer -bang -nargs=1 -range=0 -complete=command ProjectDo
          \ exe s:do('<bang>', <count>==<line1>?<count>:-1, <q-args>)
  endif
  for [command, patterns] in items(projectionist#navigation_commands())
    call projectionist#define_navigation_command(command, patterns)
  endfor
  for [prefix, excmd] in items(s:prefixes) + [['', 'edit']]
    execute 'command! -buffer -bar -bang -nargs=* -range=-1 -complete=customlist,s:edit_complete'
          \ 'A'.prefix
          \ ':execute s:edit_command("<mods>", "'.excmd.'<bang>", <count>, <f-args>)'
  endfor

  for [root, makeprg] in projectionist#query_exec('make')
    unlet! b:current_compiler
    let compiler = fnamemodify(matchstr(makeprg, '\S\+'), ':t:r')
    setlocal errorformat=%+I%.%#,
    if exists(':Dispatch')
      silent! let compiler = dispatch#compiler_for_program(makeprg)
    endif
    if !empty(findfile('compiler/'.compiler.'.vim', escape(&rtp, ' ')))
      execute 'compiler' compiler
    elseif compiler ==# 'make'
      setlocal errorformat<
    endif
    let &l:makeprg = makeprg
    let &l:errorformat .= ',%\&chdir '.escape(root, ',')
    break
  endfor

  for [root, command] in projectionist#query_exec('console')
    let offset = index(s:roots(), root) + 1
    let b:start = '-dir=' . fnameescape(root) .
          \ ' -title=' . escape(fnamemodify(root, ':t'), '\ ') . '\ console ' .
          \ command
    execute 'command! -bar -bang -buffer -nargs=* Console ' .
          \ (exists(':Start') < 2 ?
          \ 'ProjectDo ' . (offset == 1 ? '' : offset.' ') . '!' . command :
          \ 'Start<bang> ' . b:start) . ' <args>'
    break
  endfor

  for [root, command] in projectionist#query_exec('start')
    let offset = index(s:roots(), root) + 1
    let b:start = '-dir=' . fnameescape(root) . ' ' . command
    break
  endfor

  for [root, command] in s:query_exec_with_alternate('dispatch')
    let b:dispatch = '-dir=' . fnameescape(root) . ' ' . command
    break
  endfor

  for dir in projectionist#query_file('path')
    let dir = substitute(dir, '^\a\a\+:', '+&', '')
    if stridx(','.&l:path.',', ','.escape(dir, ', ').',') < 0
      let &l:path = escape(dir, ', ') . ',' . &path
    endif
  endfor

  for root in s:roots()
    let tags = s:real(root . projectionist#slash() . 'tags')
    if len(tags) && stridx(','.&l:tags.',', ','.escape(tags, ', ').',') < 0
      let &l:tags = &tags . ',' . escape(tags, ', ')
    endif
  endfor

  if exists('#User#ProjectionistActivate')
    doautocmd User ProjectionistActivate
  endif
endfunction

" Section: Completion

function! projectionist#completion_filter(results, query, sep, ...) abort
  if a:query =~# '\*'
    let regex = s:gsub(a:query, '\*', '.*')
    return filter(copy(a:results),'v:val =~# "^".regex')
  endif

  let C = get(g:, 'projectionist_completion_filter', get(g:, 'completion_filter'))
  if type(C) == type({}) && has_key(C, 'Apply')
    let results = call(C.Apply, [a:results, a:query, a:sep, a:0 ? a:1 : {}], C)
  elseif type(C) == type('') && exists('*'.C)
    let results = call(C, [a:results, a:query, a:sep, a:0 ? a:1 : {}])
  endif
  if get(l:, 'results') isnot# 0
    return results
  endif
  unlet! results

  let results = s:uniq(sort(copy(a:results)))
  call filter(results,'v:val !~# "\\~$" && !empty(v:val)')
  let filtered = filter(copy(results),'v:val[0:strlen(a:query)-1] ==# a:query')
  if !empty(filtered) | return filtered | endif
  if !empty(a:sep)
    let regex = s:gsub(a:query,'[^'.a:sep.']','[&].*')
    let filtered = filter(copy(results),'v:val =~# "^".regex')
    if !empty(filtered) | return filtered | endif
    let filtered = filter(copy(results),'a:sep.v:val =~# ''['.a:sep.']''.regex')
    if !empty(filtered) | return filtered | endif
  endif
  let regex = s:gsub(a:query,'.','[&].*')
  let filtered = filter(copy(results),'v:val =~# regex')
  return filtered
endfunction

function! s:dir_complete(lead, cmdline, _) abort
  let base = substitute(a:lead, '^[\/]', '', '')
  let slash = projectionist#slash()
  let c = matchstr(a:cmdline, '^\d\+')
  let matches = projectionist#glob(s:real(projectionist#path(substitute(base, '[\/]', '*&',  'g') . '*' . slash, c ? c : 1)))
  call map(matches,'fnameescape(matchstr(a:lead, "^[\\/]") . v:val[ strlen(s:real(projectionist#path()))+1 : -1 ])')
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

function! s:open_projection(mods, edit, variants, ...) abort
  let formats = []
  for variant in a:variants
    call add(formats, variant[0] . projectionist#slash() . (variant[1] =~# '\*\*'
          \ ? variant[1] : substitute(variant[1], '\*', '**/*', '')))
  endfor
  let cmd = s:parse(a:mods, a:000)
  if get(cmd.args, -1, '') ==# '`=`'
    let s:last_formats = formats
    return ''
  endif
  if len(cmd.args)
    call filter(formats, 'v:val =~# "\\*"')
    let name = join(cmd.args, ' ')
    let dir = matchstr(name, '.*\ze/')
    let base = matchstr(name, '[^\/]*$')
    call map(formats, 'substitute(substitute(v:val, "\\*\\*\\([\\/]\\=\\)", empty(dir) ? "" : dir . "\\1", ""), "\\*", base, "")')
  else
    call filter(formats, 'v:val !~# "\\*"')
  endif
  if empty(formats)
    return 'echoerr "Invalid number of arguments"'
  endif
  let target = formats[0]
  for format in formats
    if s:fcall('filereadable', format)
      let target = format
      break
    endif
  endfor
  call s:mkdir_p(fnamemodify(target, ':h'))
  return cmd.mods . a:edit . cmd.pre . ' ' .
        \ fnameescape(fnamemodify(target, ':~:.'))
endfunction

function! s:projection_complete(lead, cmdline, _) abort
  execute matchstr(a:cmdline, '\a\@<![' . join(keys(s:prefixes), '') . ']\w\+') . ' `=`'
  let results = []
  for format in s:last_formats
    if format !~# '\*'
      continue
    endif
    let glob = substitute(format, '[^\/]*\ze\*\*[\/]\*', '', 'g')
    let results += map(projectionist#glob(glob), 's:match(v:val, format)')
  endfor
  call s:uniq(results)
  return projectionist#completion_filter(results, a:lead, '/')
endfunction

" Section: :A

function! s:jumpopt(file) abort
  let pattern = '!$\|[:+@#]\d\+$\|[+@#].*$'
  let file = substitute(a:file, pattern, '', '')
  let jump = matchstr(a:file, pattern)
  if jump =~# '^[:+@#]\d\+$'
    return [file, '+'.jump[1:-1].' ']
  elseif jump ==# '!'
    return [file, '+AD ']
  elseif !empty(jump)
    return [file, '+A'.escape(jump, ' ').' ']
  else
    return [file, '']
  endif
endfunction

function! s:edit_command(mods, edit, count, ...) abort
  let cmd = s:parse(a:mods, a:000)
  let file = join(cmd.args, ' ')
  if len(file)
    if file =~# '^[@#+]'
      return 'echoerr ":A: @/#/+ not supported"'
    endif
    let open = s:jumpopt(projectionist#path(file, a:count < 1 ? 1 : a:count))
    if empty(open[0])
      return 'echoerr "Invalid count"'
    endif
  elseif a:edit =~# 'read'
    call projectionist#apply_template()
    return ''
  else
    let expansions = {}
    if a:count > 0
      let expansions.lnum = a:count
    endif
    let alternates = projectionist#query_file('alternate', expansions)
    let warning = get(filter(copy(alternates), 'v:val =~# "replace %.*}"'), 0, '')
    if !empty(warning)
      return 'echoerr '.string(matchstr(warning, 'replace %.*}').' in alternate projection')
    endif
    call map(alternates, 's:jumpopt(v:val)')
    let open = get(filter(copy(alternates), 's:fcall("getftime", v:val[0]) >= 0'), 0, [])
    if empty(alternates)
      return 'echoerr "No alternate file"'
    elseif empty(open)
      let choices = ['Create alternate file?']
      let i = 0
      for [alt, _] in alternates
        let i += 1
        call add(choices, i.' '.alt)
      endfor
      let i = inputlist(choices)
      if i > 0
        let open = get(alternates, i-1, [])
      endif
      if empty(open)
        return ''
      endif
    endif
  endif
  let [file, jump] = open
  call s:mkdir_p(fnamemodify(file, ':h'))
  return cmd.mods . a:edit . cmd.pre . ' ' .
        \ jump . fnameescape(fnamemodify(file, ':~:.'))
endfunction

function! s:edit_complete(lead, cmdline, _) abort
  let base = substitute(a:lead, '^[\/]', '', '')
  let c = matchstr(a:cmdline, '^\d\+')
  let matches = projectionist#glob(projectionist#path(substitute(base, '[\/]', '*&',  'g') . '*', c ? c : 1))
  call map(matches, 'fnameescape(matchstr(a:lead, "^[\\/]") . v:val[ strlen(projectionist#path())+1 : -1 ] . (s:fcall("isdirectory", v:val) ? projectionist#slash() : ""))')
  return matches
endfunction

" Section: :ProjectDo

function! s:do(bang, count, cmd) abort
  let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
  let cwd = getcwd()
  let cmd = substitute(a:cmd, '^\d\+ ', '', '')
  let offset = cmd ==# a:cmd ? 1 : matchstr(a:cmd, '^\d\+')
  try
    execute cd fnameescape(s:real(projectionist#path('', offset)))
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
  let dir = substitute(matchstr(','.&l:errorformat, ',\%(%\\&\)\=chdir[ =]\zs\%(\\.\|[^,]\)*'), '\\,' ,',', 'g')
  let cwd = getcwd()
  if !empty(dir) && dir !=# cwd
    let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd' : 'cd'
    execute cd fnameescape(dir)
    let s:qf_post = cd . ' ' . fnameescape(cwd)
  endif
endfunction

augroup projectionist_make
  autocmd!
  autocmd QuickFixCmdPre  *make* call s:qf_pre()
  autocmd QuickFixCmdPost *make*
        \ if exists('s:qf_post') | execute remove(s:, 'qf_post') | endif
augroup END

" Section: Templates

function! projectionist#apply_template() abort
  let template = get(projectionist#query('template'), 0, ['', ''])[1]
  if type(template) == type([]) && type(get(template, 0)) == type([])
    let template = template[0]
  endif
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
     if exists('#User#ProjectionistApplyTemplate')
       doautocmd User ProjectionistApplyTemplate
     endif
    doautocmd BufReadPost
  endif
  return ''
endfunction
