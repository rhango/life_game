! -*- coding: utf-8-unix -*-

module life_game_graphic
  use iso_c_binding  , only: c_double, c_ptr
  use life_game_field, only: field_type => field

   implicit none
   private

   type, public :: graphic
      private
      type(c_ptr)                :: drawing_area
      type(c_ptr)                :: alignment
      class(field_type), pointer :: field => null()
      real(c_double)             :: cell_size
    contains
      private
      procedure, public :: make
      procedure, public :: remake
      procedure, nopass :: draw
      procedure, public :: redraw
      procedure         :: draw_background
      procedure         :: draw_grid
      procedure         :: draw_cells
   end type graphic

contains

  subroutine make(this, alignment, field, cell_size)
    use iso_c_binding  , only: c_double, c_ptr, c_loc, c_funloc
    use gtk            , only: &
         gtk_drawing_area_new, &
         gtk_container_add, &
         gtk_widget_set_size_request, &
         g_signal_connect, &
         c_null_char
    use life_game_field, only: field_type => field

    class(graphic)  , intent(out), target :: this
    type(c_ptr)     , intent(in )         :: alignment
    type(field_type), intent(in ), target :: field
    real(c_double)  , intent(in )         :: cell_size
    type(c_ptr)                           :: drawing_area

    this%alignment =  alignment
    this%field     => field
    this%cell_size =  cell_size

    drawing_area = gtk_drawing_area_new()
    this%drawing_area = drawing_area
    call gtk_container_add(alignment, drawing_area)

    call gtk_widget_set_size_request(drawing_area, &
         nint(cell_size * field%col), nint(cell_size * field%row))

    select type (this)
    type is (graphic)
       call g_signal_connect(drawing_area, &
            "expose-event"//c_null_char, c_funloc(draw), c_loc(this))
    end select

    ! call gtk_spin_button_set_value(graphics%row_spin, real(row, c_double))
    ! call gtk_spin_button_set_value(graphics%col_spin, real(col, c_double))
  end subroutine make

  subroutine remake(this, field)
    use iso_c_binding  , only: c_double, c_ptr
    use gtk            , only: gtk_container_remove, gtk_widget_show
    use life_game_field, only: field_type => field

    class(graphic)  , intent(inout) :: this
    type(field_type), intent(in   ) :: field
    type(c_ptr)                     :: alignment
    real(c_double)                  :: cell_size

    alignment = this%alignment
    cell_size = this%cell_size

    call gtk_container_remove(alignment, this%drawing_area)
    call this%make(alignment, field, cell_size)
    call gtk_widget_show(this%drawing_area)
  end subroutine remake

  function draw(drawing_area, expose_event, c_this) result(ret) bind(c)
    use iso_c_binding, only: c_int, c_ptr, c_f_pointer
    use gtk          , only: TRUE, FALSE

    integer(c_int)         :: ret
    type(c_ptr)  , value   :: drawing_area, expose_event
    type(c_ptr)  , value   :: c_this
    type(graphic), pointer :: this => null()

    call c_f_pointer(c_this, this)

    call this%draw_background()
    call this%draw_grid()
    call this%draw_cells()

    ret = FALSE
  end function draw

  subroutine redraw(this)
    use gtk, only: gtk_widget_queue_draw

    class(graphic), intent(in) :: this

    call gtk_widget_queue_draw(this%drawing_area)
  end subroutine redraw

  subroutine draw_background(this)
    use iso_c_binding, only: c_double, c_ptr
    use gtk          , only: gtk_widget_get_window
    use gdk          , only: gdk_cairo_create
    use cairo        , only: &
         cairo_set_source_rgb, &
         cairo_paint, &
         cairo_destroy

    use iso_c_binding, only: c_loc

    class(graphic), intent(in) :: this
    type(c_ptr)                :: bg

    bg = gdk_cairo_create(gtk_widget_get_window(this%drawing_area))

    call cairo_set_source_rgb(bg, &
         0.0_c_double, 0.0_c_double, 0.0_c_double)
    call cairo_paint(bg)
    call cairo_destroy(bg)
  end subroutine draw_background

  subroutine draw_grid(this)
    use iso_c_binding, only: c_double, c_ptr
    use gtk          , only: gtk_widget_get_window
    use gdk          , only: gdk_cairo_create
    use cairo        , only: &
         cairo_set_source_rgb, &
         cairo_set_line_width, &
         cairo_move_to, cairo_line_to, &
         cairo_stroke, &
         cairo_destroy

    class(graphic), intent(in) :: this
    type(c_ptr)                :: grid
    integer                    :: row, col
    real(c_double)             :: cell_size
    integer                    :: i

    row       = this%field%row
    col       = this%field%col
    cell_size = this%cell_size

    grid = gdk_cairo_create(gtk_widget_get_window(this%drawing_area))

    call cairo_set_source_rgb(grid, &
         0.0_c_double, 1.0_c_double, 0.0_c_double)
    call cairo_set_line_width(grid, 0.1_c_double)

    do i = 0, col
       call cairo_move_to(grid, real(i, c_double)*cell_size, 0.0_c_double)
       call cairo_line_to(grid, real(i, c_double)*cell_size, &
            real(row, c_double)*cell_size)
       call cairo_stroke(grid)
    end do
    do i = 0, row
       call cairo_move_to(grid, 0.0_c_double, real(i, c_double)*cell_size)
       call cairo_line_to(grid, real(col, c_double)*cell_size, &
            real(i, c_double)*cell_size)
       call cairo_stroke(grid)
    end do
  end subroutine draw_grid

  subroutine draw_cells(this)
    use iso_c_binding, only: c_double, c_ptr
    use gtk          , only: gtk_widget_get_window
    use gdk          , only: gdk_cairo_create
    use cairo        , only: &
         cairo_set_source_rgb, &
         cairo_rectangle, &
         cairo_fill, &
         cairo_destroy

    class(graphic), intent(in) :: this
    type(c_ptr)                :: cell
    integer(1)    , pointer    :: cells(:,:) => null()
    integer                    :: row, col
    real(c_double)             :: cell_size
    integer                    :: i, j

    row       = this%field%row
    col       = this%field%col
    cell_size = this%cell_size
    cells(0:row+1,0:col+1) => this%field%cells

    cell = gdk_cairo_create(gtk_widget_get_window(this%drawing_area))

    call cairo_set_source_rgb(cell, &
         0.0_c_double, 1.0_c_double, 0.0_c_double)

    do j = 1, col
       do i = 1, row
          if (cells(i,j) == 1) then
             call cairo_rectangle(cell, &
                  real(j-1, c_double)*cell_size, &
                  real(i-1, c_double)*cell_size, &
                  cell_size, cell_size)
             call cairo_fill(cell)
          end if
       end do
    end do

    call cairo_destroy(cell)
  end subroutine draw_cells

end module life_game_graphic
