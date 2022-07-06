#!/usr/bin/env python3

import xrandr

def rank_modes(out):
  def score_mode(mode):
    ideal_dpmm = 6.5
    screen_ratio = out.size[0] / out.size[1]
    return (mode.dpmm(out.size) - ideal_dpmm) ** 2 + (mode.ratio() - screen_ratio) ** 2
  
  return sorted(out.modes, key=score_mode)


def main():
  out = xrandr.get_default_output()
  modes = rank_modes(out)
  if not modes[0].current:
    xrandr.set_mode(out, modes[0])


if __name__ == '__main__':
  main()
