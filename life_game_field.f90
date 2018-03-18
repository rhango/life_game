! -*- coding: utf-8-unix -*-

module life_game_field
  implicit none
  private

  type, public :: field
     private
     integer   , public              :: row
     integer   , public              :: col
     integer(1), public, allocatable :: cells(:,:)
   contains
     private
     final             :: field_dtor
     procedure, public :: make_zero
     procedure, public :: make_random
     procedure, public :: make_file
     procedure, public :: next
  end type field

contains
  subroutine field_dtor(this)
    type(field), intent(inout) :: this

    if (allocated(this%cells)) deallocate(this%cells)
  end subroutine field_dtor

  subroutine make_zero(this, row, col)
    class(field), intent(out) :: this
    integer     , intent(in ) :: row, col

    this%row = row
    this%col = col
    if (allocated(this%cells)) deallocate(this%cells)
    allocate( this%cells(0:row+1,0:col+1) )
    this%cells(:,:) = 0
  end subroutine make_zero

  subroutine make_random(this, row, col)
    class(field), intent(out) :: this
    integer     , intent(in ) :: row, col
    real    :: rnd(1:row,1:col)
    integer :: count, seed_size
    integer :: i

    call system_clock(count = count)
    call random_seed(size = seed_size)
    call random_seed(put = [ (count, i = 1, seed_size) ])
    call random_number(rnd)
    call this%make_zero(row, col)
    this%cells(1:row,1:col) = nint(rnd(1:row,1:col), 1)
  end subroutine make_random

  subroutine make_file(this, file)
    use iso_c_binding   , only: c_int, c_char, c_ptr, c_f_pointer
    use gtk             , only: c_null_char, c_null_ptr
    use gtk_os_dependent, only: gdk_pixbuf_new_from_file
    use gdk_pixbuf      , only: &
         gdk_pixbuf_get_width, gdk_pixbuf_get_height, &
         gdk_pixbuf_get_n_channels, &
         gdk_pixbuf_get_rowstride, &
         gdk_pixbuf_get_pixels

    class(field)     , intent(out) :: this
    character(len=* ), intent(in ) :: file
    character(c_char), pointer     :: pixel(:)
    type(c_ptr) :: pixbuf
    integer     :: row, col, nch, rowstride
    integer     :: w, h
    integer     :: i, j

    pixbuf    = gdk_pixbuf_new_from_file(file//c_null_char, c_null_ptr)
    row       = int(gdk_pixbuf_get_height(pixbuf))
    col       = int(gdk_pixbuf_get_width(pixbuf))
    nch       = int(gdk_pixbuf_get_n_channels(pixbuf))
    rowstride = int(gdk_pixbuf_get_rowstride(pixbuf))
    call c_f_pointer(gdk_pixbuf_get_pixels(pixbuf), pixel, [0])

    this%row = row
    this%col = col
    call this%make_zero(row, col)

    do j = 1, row
       h = (j-1) * rowstride
       do i = 1, col
          w = (i-1) * nch
          if (sum(ichar(pixel(h+w+1:h+w+3))) /= 0) then
             this%cells(j,i) = 0
          else
             this%cells(j,i) = 1
          end if
       end do
    end do
  end subroutine make_file

  subroutine next(this)
    class(field), intent(inout) :: this
    integer :: row, col
    integer :: i, j

    row = this%row
    col = this%col
    this%cells(1:row,1:col) = reshape( &
         [ ((dead_or_alive(this%cells(i-1:i+1,j-1:j+1)), &
         i=1, row), j=1, col) ], &
         [row, col] )
  end subroutine next

  function dead_or_alive(cells) result(doa)
    integer(1)             :: doa
    integer(1), intent(in) :: cells(-1:1,-1:1)

    if (cells(0,0) == 0) then
       if (sum(cells) == 3) doa = 1
    else
       if ((sum(cells)-1 == 2) .or. (sum(cells)-1 == 3)) then
          doa = 1
       else
          doa = 0
       end if
    end if
  end function dead_or_alive
end module life_game_field
