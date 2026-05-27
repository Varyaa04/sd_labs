module List_IO
   use Environment

   implicit none

   !рекурсивный производный тип
   type, public :: node
      character(:), allocatable :: value
      type(node), pointer       :: next => Null()
   end type node

   !инкапсулирующий тип для списка
   type, public :: StringList
   private
      type(node), pointer :: head => Null()
   contains
      procedure :: read_from_file
      procedure :: output
      procedure :: destroy
      procedure :: get_head
      procedure :: set_head
      final :: finalize_list
   end type StringList

contains

   !чтение списка из файла
   subroutine read_from_file(this, input_file)
      class(StringList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      open (file=input_file, newunit=In)
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         call add_to_end(this%head, buffer)
      end do
      close (In)
   end subroutine read_from_file

   !хвостовая рекурсия для добавления в конец
   recursive subroutine add_to_end(head, val)
      type(node), pointer, intent(inout) :: head
      character(*), intent(in) :: val

      if (.not. associated(head)) then
         allocate(head)
         head%value = val
         head%next => Null()
      else
         call add_to_end(head%next, val)
      end if
   end subroutine add_to_end

   !вывод списка
   subroutine output(this, output_file, header, position)
      class(StringList), intent(in) :: this
      character(*), intent(in) :: output_file, position, header
      integer :: Out
      logical :: file_exists

      inquire(file=output_file, exist=file_exists)
      open (file=output_file, position=position, newunit=Out)
      
      if (position == "append" .and. file_exists) write (Out, '(a)') ""
      write (Out, '(a)') header
      
      if (associated(this%head)) then
         call write_values(Out, this%head)
      else
         write (Out, '(a)') "  (список пуст)"
      end if
      close (Out)
   end subroutine output

   recursive subroutine write_values(out, head)
      integer, intent(in) :: out
      type(node), pointer, intent(in) :: head

      if (.not. associated(head)) return
      write (out, '(5x, a)') head%value
      call write_values(out, head%next)
   end subroutine write_values

   function get_head(this) result(head)
      class(StringList), intent(in) :: this
      type(node), pointer :: head
      head => this%head
   end function get_head

   subroutine set_head(this, head)
      class(StringList), intent(inout) :: this
      type(node), pointer, intent(in) :: head
      this%head => head
   end subroutine set_head

   !уничтожение списка
   recursive subroutine destroy(this)
      class(StringList), intent(inout) :: this
      call destroy_recursive(this%head)
      this%head => Null()
   end subroutine destroy

   recursive subroutine destroy_recursive(head)
      type(node), pointer, intent(inout) :: head
      type(node), pointer :: tmp

      if (.not. associated(head)) return
      tmp => head
      head => head%next
      deallocate(tmp)
      call destroy_recursive(head)
   end subroutine destroy_recursive

   !завершающая функция
   subroutine finalize_list(this)
      type(StringList), intent(inout) :: this
      call this%destroy()
   end subroutine finalize_list

   !чтение последней строки из файла
   subroutine read_last_line_from_file(input_file, last_line)
      character(*), intent(in) :: input_file
      character(*), intent(out) :: last_line
      integer :: In, IO
      character(100) :: buffer

      open (file=input_file, newunit=In)
      last_line = ""
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         last_line = buffer
      end do
      close (In)
   end subroutine read_last_line_from_file

end module List_IO