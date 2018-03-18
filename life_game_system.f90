! -*- coding: utf-8-unix -*-

module life_game_system
  use iso_c_binding    , only: c_int
  use life_game_field  , only: field_type   => field
  use life_game_graphic, only: graphic_type => graphic
  use life_game_menu   , only: menu_type    => menu

  implicit none
  private

  type, public :: system
     private
     type(field_type)           :: field
     type(graphic_type), public :: graphic
     type(menu_type)   , public :: menu
     integer(c_int)             :: time_id
     integer(c_int)             :: delay
     logical(1)                 :: delay_is_changed = .false.
     logical(1)                 :: game_is_playing  = .false.
   contains
     private
     procedure, public :: make
     procedure, nopass :: next_frame
     procedure, nopass :: make_field_random
     procedure, nopass :: make_field_file
     procedure         :: change_delay
     procedure, nopass :: start_game_func
     procedure, nopass :: stop_game_func
     procedure         :: start_game
     procedure         :: stop_game
  end type system

contains

  subroutine make(this, alignment, row, col, cell_size, delay)
    use iso_c_binding, only: c_int, c_double, c_ptr, c_loc

    class(system)  , intent(out), target :: this
    type(c_ptr)    , intent(in )         :: alignment
    integer        , intent(in )         :: row, col
    real(c_double) , intent(in )         :: cell_size
    integer(c_int) , intent(in )         :: delay

    call this%field%make_zero(row, col)
    call this%graphic%make(alignment, this%field, cell_size)

    select type (this)
    type is (system)
       call this%menu%make(c_loc(this), &
            make_field_random, make_field_file, &
            start_game_func, stop_game_func)
    end select

    this%delay =  delay
  end subroutine make

  function next_frame(c_this) result(ret) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    use gtk          , only: TRUE, FALSE

    integer(c_int)        :: ret
    type(c_ptr) , value   :: c_this
    type(system), pointer :: this

    call c_f_pointer(c_this, this)

    call this%field%next()
    call this%graphic%redraw()
    if (this%delay_is_changed) then
       call this%stop_game()
       call this%start_game()
    end if

    ret = TRUE
  end function next_frame

  function make_field_random(button, c_this) result(ret) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    use gtk          , only: TRUE, FALSE

    integer(c_int)        :: ret
    type(c_ptr) , value   :: button
    type(c_ptr) , value   :: c_this
    type(system), pointer :: this
    integer               :: row, col

    call c_f_pointer(c_this, this)

    if (.not. this%game_is_playing) then
       call this%menu%get_row_col_value(row, col)
       call this%field%make_random(row, col)
       call this%graphic%remake(this%field)
    end if

    ret = TRUE
  end function make_field_random

  function make_field_file(button, c_this) result(ret) bind(c)
    use iso_c_binding, only: c_int, c_char, c_ptr, c_f_pointer
    use gtk          , only: gtk_entry_get_text, TRUE, FALSE
    use gtk_hl       , only: convert_c_string

    integer(c_int)                  :: ret
    type(c_ptr)   , value           :: button
    type(c_ptr)   , value           :: c_this
    type(system)  , pointer         :: this
    character(kind=c_char, len=128) :: file

    call c_f_pointer(c_this, this)

    if (.not. this%game_is_playing) then
       call this%menu%get_file_path(file)
       call this%field%make_file(trim(file))
       call this%graphic%remake(this%field)
    end if

    ret = TRUE
  end function make_field_file

  subroutine change_delay(this, delay)
    use iso_c_binding, only: c_int

    class(system) , intent(inout) :: this
    integer(c_int), intent(in   ) :: delay

    this%delay            = delay
    this%delay_is_changed = .true.
  end subroutine change_delay

  function start_game_func(button, c_this) result(ret) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    use gtk          , only: TRUE, FALSE

    integer(c_int)        :: ret
    type(c_ptr) , value   :: button
    type(c_ptr) , value   :: c_this
    type(system), pointer :: this

    call c_f_pointer(c_this, this)
    call this%start_game()
    ret = TRUE
  end function start_game_func

  subroutine start_game(this)
    use iso_c_binding, only: c_loc, c_funloc
    use g            , only: g_timeout_add

    class(system), intent(inout) :: this

    if (.not. this%game_is_playing) then
       this%game_is_playing  = .true.
       this%delay_is_changed = .false.
       select type (this)
       type is (system)
          this%time_id = g_timeout_add( &
               this%delay, c_funloc(next_frame), c_loc(this))
       end select
    end if
  end subroutine start_game

  function stop_game_func(button, c_this) result(ret) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    use gtk          , only: TRUE, FALSE

    integer(c_int)        :: ret
    type(c_ptr) , value   :: button
    type(c_ptr) , value   :: c_this
    type(system), pointer :: this

    call c_f_pointer(c_this, this)
    call this%stop_game()
    ret = TRUE
  end function stop_game_func

  subroutine stop_game(this)
    use iso_c_binding, only: c_int
    use g            , only: g_source_remove

    class(system), intent(inout) :: this
    integer(c_int)               :: err

    if (this%game_is_playing) then
       this%game_is_playing = .false.
       err = g_source_remove(this%time_id)
    end if
  end subroutine stop_game

end module life_game_system
