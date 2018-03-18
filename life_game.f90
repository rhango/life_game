! -*- coding: utf-8-unix -*-

program main
  use life_game_window, only: window_type => window

  implicit none

  type(window_type) :: window

  call window%make()
end program main
