! -*- coding: utf-8-unix -*-

module life_game_window
  use life_game_system, only: system_type => system
  use life_game_menu  , only: menu_type   => menu

  implicit none
  private

  type, public :: window
     private
     type(system_type) :: system
   contains
     private
     procedure, public :: make
     procedure, nopass :: delete_event
     procedure, nopass :: destroy
  end type window

contains

  subroutine make(this)
    use iso_c_binding, only: c_float, c_double, c_ptr, c_funloc
    use gtk, only: &
         gtk_init, &
         gtk_main, &
         gtk_widget_show_all, &
         gtk_window_new, &
         gtk_hbox_new, &
         gtk_scrolled_window_new, &
         gtk_alignment_new, &
         gtk_container_add, &
         gtk_box_pack_start, gtk_box_pack_end, &
         gtk_scrolled_window_add_with_viewport, &
         GTK_WINDOW_TOPLEVEL, &
         gtk_window_set_title, &
         gtk_scrolled_window_set_policy, &
         gtk_scrolled_window_set_shadow_type, &
         GTK_POLICY_AUTOMATIC, GTK_SHADOW_NONE, &
         gtk_window_set_default_size, &
         gtk_widget_set_size_request, &
         g_signal_connect, &
         c_null_char, c_null_ptr, &
         TRUE, FALSE

    class(window), intent(out) :: this
    type(c_ptr) :: window, box, scroll_window, alignment

     call gtk_init()

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL)
    call gtk_window_set_default_size(window, 500, 500)
    call gtk_window_set_title(window, "Life Game"//c_null_char)
    call g_signal_connect( &
         window, "delete-event"//c_null_char, c_funloc(delete_event))
    call g_signal_connect( &
         window, "destroy"//c_null_char, c_funloc(destroy))

    box = gtk_hbox_new(FALSE, 0)
    call gtk_container_add(window, box)

    scroll_window = gtk_scrolled_window_new(c_null_ptr, c_null_ptr)
    call gtk_box_pack_start(box, scroll_window, TRUE, TRUE, 0)
    call gtk_scrolled_window_set_policy( &
         scroll_window, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC)
    call gtk_scrolled_window_set_shadow_type( &
         scroll_window, GTK_SHADOW_NONE)

    alignment = gtk_alignment_new( &
         0.5_c_float, 0.5_c_float, 0.0_c_float, 0.0_c_float)
    call gtk_scrolled_window_add_with_viewport(scroll_window, alignment)

    call this%system%make(alignment, 50, 100, 10.0_c_double, 200)
    call gtk_box_pack_end(box, this%system%menu%box, FALSE, FALSE, 10)

    ! call gtk_spin_button_set_value(row_spin, real(row, c_double))
    ! call gtk_spin_button_set_value(col_spin, real(col, c_double))
    call gtk_widget_show_all(window)

    call gtk_main()
  end subroutine make

  function delete_event() result(ret) bind(c)
    use iso_c_binding, only: c_int
    use gtk          , only: TRUE, FALSE

    integer(c_int) :: ret

    ret = FALSE
  end function delete_event

  function destroy() result(ret) bind(c)
    use iso_c_binding, only: c_int
    use gtk          , only: gtk_main_quit, TRUE, FALSE

    integer(c_int) :: ret

    call gtk_main_quit()
    ret = TRUE
  end function destroy

end module life_game_window
