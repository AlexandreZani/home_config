import re
import subprocess
from collections import namedtuple

class Mode(namedtuple('Mode', ['size', 'current', 'preferred'])):

  def ratio(self):
    return self.size[0] / self.size[1]

  def dpmm(self, screen_size):
    return max((self.size[0] / screen_size[0], self.size[1] / screen_size[1]))

  def name(self):
    return '{}x{}'.format(*self.size)

class Output(object):
  ALLOWED = {'name', 'connected', 'brightness', 'size', 'modes'}
  def __init__(self, name, connected, brightness=None, size=None):
    self.name = name
    self.connected = connected
    self.modes = []

  def __setattr__(self, name, value):
    if name not in self.ALLOWED:
      raise AttributeError('\'{}\' object as no attribute \'{}\''.format(
        self.__class__.__name__, name))
    return object.__setattr__(self, name, value)

  def __getattribute__(self, name):
    try:
      return object.__getattribute__(self, name)
    except AttributeError as e:
      if name in self.ALLOWED:
        return None
      raise

  def __repr__(self):
    return str({k: v for k, v in vars(self).items() if v})

def parse_output(lines):
  first = lines[0].split()
  if first[0] == 'Screen':
    return None

  name = first[0]
  connected = (first[1] == 'connected')
  output = Output(name, connected)

  size_matches = re.findall(r'(\d+)mm', lines[0])
  if len(size_matches) == 2:
    output.size = (int(size_matches[0]), int(size_matches[1]))

  for line in lines[1:]:
    m = re.match(r'.*Brightness: (.*)', line)
    if m:
      output.brightness = float(m.group(1))

    m = re.match(r'\s*(\d+)x(\d+).*', line)
    if m:
      current = line.count('current') > 0 
      preferred = line.count('preferred') > 0 
      mode = Mode((int(m.group(1)), int(m.group(2))), current, preferred)
      output.modes.append(mode)

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


def set_mode(output, mode):
  cmd = ['xrandr', '--output', output.name, '--mode', mode.name()]
  subprocess.run(cmd, capture_output=True)
