import re
import subprocess

class Output(object):
  def __init__(self, name, connected):
    self.name = name
    self.connected = connected

  def __repr__(self):
    return str(vars(self))


def parse_output(lines):
  first = lines[0].split()
  if first[0] == 'Screen':
    return None

  name = first[0]
  connected = (first[1] == 'connected')

  output = Output(name, connected)

  for line in lines[1:]:
    m = re.match(r'.*Brightness: (.*)', line)
    if m:
      output.brightness = float(m.group(1))

  return output


def get_outputs(connected=True):
  cmd = ['xrandr', '--verbose']
  completed = subprocess.run(cmd, capture_output=True)
  lines = completed.stdout.decode().split('\n')

  out = []
  for line in lines:
    if line and not line[0].isspace():
      out.append([line])
      continue

    out[-1].append(line)

  def filter_f(i):
    if not i:
      return False

    if connected and not i.connected:
      return False

    return True

  return [i for i in filter(filter_f, map(parse_output, out))]


def get_default_output():
  outputs = get_outputs(connected=True)
  if outputs:
    return outputs[0]
  return None


def new_brightness(cur, arg):
  v = float(arg[1:])
  c = arg[0]
  if c == '=':
    return v
  elif c == '-':
    return (cur - v)
  elif c == '+':
    return (cur + v)
  raise Exception('Arg is not good.')

def set_brightness(output, v):
  cmd = ['xrandr', '--output', output.name, '--brightness', str(v)]
  subprocess.run(cmd, capture_output=True)
