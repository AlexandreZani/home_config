python << EOF
import vim

def iter_back(buffer, start):
  for ln_no in xrange(start, -1, -1):
    yield buffer[ln_no]

def get_indent(line):
  indent = 0
  while line[indent].isspace():
    indent += 1
  return indent

def find_python_ctx(buffer, start):
  func = None
  cls = None
  cur_indent = 10000000
  for line in iter_back(buffer, start):
    if line.strip() == "":
      continue
    line_indent = get_indent(line)
    if line_indent < cur_indent:
      cur_indent = line_indent
      tokens = line.split()
      try:
        if tokens[0] in set(['class', 'def']):
          name = tokens[1].split("(")[0]
          if tokens[0] == 'def':
            func = name
            if line_indent == 0:
              return (cls, func)
          elif tokens[0] == 'class':
            cls = name
            if cls[-1] == ':':
              cls = cls[:-1]
            return (cls, func)
      except Exception:
        continue
  return (cls, func)

def get_cur_python_ctx():
  row, col = vim.current.window.cursor
  return find_python_ctx(vim.current.buffer, row-1)

def print_cur_python_ctx():
  cls, func = get_cur_python_ctx()
  if func and cls is None:
    print "def %s" % func
  elif cls and func is None:
    print "class %s" % cls
  elif cls and func:
    print "%s.%s" % (cls, func)
EOF

augroup python_context
  autocmd!
  autocmd CursorMoved *.py :python print_cur_python_ctx()
augroup END
