! -*- coding: utf-8-unix -*-

module life_game_menu
  use iso_c_binding, only: c_ptr

  implicit none
  private

  type, public :: menu
     private
     type(c_ptr), public :: box
     type(c_ptr)         :: random_button, row_spin, col_spin
     type(c_ptr)         :: file_button, file_entry
     type(c_ptr)         :: start_button, stop_button
   contains
     private
     procedure, public :: make
     procedure         :: add_zero_random_box
     procedure         :: add_file_box
     procedure         :: add_start_stop_box
     procedure         :: set_event_callback
     procedure, public :: get_row_col_value
     procedure, public :: get_file_path
  end type menu

contains

  subroutine make(this, c_system, random, file, start, stop)
    use iso_c_binding   , only: c_int, c_loc, c_funloc
    use gtk, only: &
         gtk_vbox_new, &
         gtk_widget_set_size_request, &
         TRUE, FALSE

    class(menu), intent(out)  :: this
    type(c_ptr), intent(in )  :: c_system
    procedure(integer(c_int)) :: random, file, start, stop

    this%box = gtk_vbox_new(FALSE, 0)
    call gtk_widget_set_size_request(this%box, 100, -1)

    call this%add_zero_random_box()
    call this%add_file_box()
    call this%add_start_stop_box()
    call this%set_event_callback(c_system, random, file, start, stop)
  end subroutine make

  subroutine add_zero_random_box(this)
    use iso_c_binding, only: c_double, c_ptr
    use gtk          , only: &
         gtk_vbox_new, &
         gtk_button_new_with_label, &
         gtk_label_new, &
         gtk_spin_button_new_with_range, &
         gtk_box_pack_start, &
         c_null_char, &
         TRUE, FALSE

    class(menu), intent(inout) :: this
    type(c_ptr)                :: box
    type(c_ptr)                :: vbox, label

    box = gtk_vbox_new(FALSE, 0)
    call gtk_box_pack_start(this%box, box, FALSE, FALSE, 10)

    this%random_button = gtk_button_new_with_label("RANDOM"//c_null_char)
    call gtk_box_pack_start(box, this%random_button, FALSE, FALSE, 5)

    vbox = gtk_vbox_new(FALSE, 0)
    call gtk_box_pack_start(box, vbox, FALSE, FALSE, 5)

    label = gtk_label_new("ROW"//c_null_char)
    call gtk_box_pack_start(vbox, label, FALSE, FALSE, 0)

    this%row_spin = gtk_spin_button_new_with_range( &
         0.0_c_double, 1000.0_c_double, 1.0_c_double)
    call gtk_box_pack_start(vbox, this%row_spin, FALSE, FALSE, 0)

    vbox = gtk_vbox_new(FALSE, 0)
    call gtk_box_pack_start(box, vbox, FALSE, FALSE, 5)

    label = gtk_label_new("COLUMN"//c_null_char)
    call gtk_box_pack_start(vbox, label, FALSE, FALSE, 0)

    this%col_spin = gtk_spin_button_new_with_range( &
         0.0_c_double, 1000.0_c_double, 1.0_c_double)
    call gtk_box_pack_start(vbox, this%col_spin, FALSE, FALSE, 0)
  end subroutine add_zero_random_box

  subroutine add_file_box(this)
    use iso_c_binding, only: c_ptr
    use gtk          , only: &
         gtk_vbox_new, &
         gtk_button_new_with_label, &
         gtk_entry_new, &
         gtk_box_pack_start, &
         c_null_char, &
         TRUE, FALSE

    class(menu), intent(inout) :: this
    type(c_ptr)                :: box

    box = gtk_vbox_new(FALSE, 0)
    call gtk_box_pack_start(this%box, box, FALSE, FALSE, 10)

    this%file_button = gtk_button_new_with_label("FILE"//c_null_char)
    call gtk_box_pack_start(box, this%file_button, FALSE, FALSE, 5)

    this%file_entry = gtk_entry_new()
    call gtk_box_pack_start(box, this%file_entry, FALSE, FALSE, 5)
  end subroutine add_file_box

  subroutine add_start_stop_box(this)
    use iso_c_binding, only: c_ptr
    use gtk          , only: &
         gtk_vbox_new, &
         gtk_button_new_with_label, &
         gtk_box_pack_start, &
         c_null_char, &
         TRUE, FALSE

    class(menu), intent(inout) :: this
    type(c_ptr)                :: box

    box = gtk_vbox_new(FALSE, 0)
    call gtk_box_pack_start(this%box, box, FALSE, FALSE, 10)

    this%start_button = gtk_button_new_with_label("START"//c_null_char)
    call gtk_box_pack_start(box, this%start_button, FALSE, FALSE, 5)

    this%stop_button = gtk_button_new_with_label("STOP"//c_null_char)
    call gtk_box_pack_start(box, this%stop_button, FALSE, FALSE, 5)
  end subroutine add_start_stop_box

  subroutine set_event_callback(this, c_system, random, file, start, stop)
    use iso_c_binding   , only: c_int, c_loc, c_funloc
    use gtk             , only: g_signal_connect, c_null_char

    class(menu), intent(inout) :: this
    type(c_ptr), intent(in   ) :: c_system
    procedure(integer(c_int))  :: random, file, start, stop

    call g_signal_connect(this%random_button, &
         "clicked"//c_null_char, c_funloc(random), c_system)
    call g_signal_connect(this%file_button, &
         "clicked"//c_null_char, c_funloc(file  ), c_system)
    call g_signal_connect(this%start_button, &
         "clicked"//c_null_char, c_funloc(start ), c_system)
    call g_signal_connect(this%stop_button, &
         "clicked"//c_null_char, c_funloc(stop  ), c_system)
  end subroutine set_event_callback

  subroutine get_row_col_value(this, row, col)
    use gtk, only: gtk_spin_button_get_value

    class(menu), intent(in ) :: this
    integer    , intent(out) :: row, col

    row = int(gtk_spin_button_get_value(this%row_spin))
    col = int(gtk_spin_button_get_value(this%col_spin))
  end subroutine get_row_col_value

  subroutine get_file_path(this, file)
    use iso_c_binding, only: c_char
    use gtk          , only: gtk_entry_get_text
    use gtk_hl       , only: convert_c_string

    class(menu)                  , intent(in ) :: this
    character(kind=c_char, len=*), intent(out) :: file

    call convert_c_string(gtk_entry_get_text(this%file_entry), file)
  end subroutine get_file_path

end module life_game_menu
