import os
import os.path

BACKLIGHT_DIR = os.path.join('/', 'sys', 'class', 'backlight')

class Backlight(object):
  def __init__(self, name):
    self._name = name
    self._max = None

  def get_path(self, fn):
    return os.path.join(BACKLIGHT_DIR, self._name, fn)

  def get_max(self):
    if self._max is not None:
      return self._max

    with open(self.get_path('max_brightness')) as fp:
      self._max = int(fp.read().strip())
      return self._max

  def get_cur(self):
    with open(self.get_path('brightness')) as fp:
      return int(fp.read().strip())

  def set_cur(self, b):
    with open(self.get_path('brightness'), 'w') as fp:
      fp.writelines([str(b), ''])

  def get_cur_fraction(self):
    return self.get_cur() / self.get_max()

  def set_cur_fraction(self, f):
    b = f * self.get_max()
    self.set_cur(int(b))


def get_backlights():
  return [Backlight(d) for d in os.listdir(BACKLIGHT_DIR)]
