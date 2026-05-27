module List_Process
   use Environment

   implicit none

   ! рекурсивный производный тип
   type, private  :: node
      character(:), allocatable :: value
      type(node), pointer       :: next => Null()
   end type node

   ! инкапсулирующий тип для списка
   type, public :: StringList
   private
      type(node), pointer :: head => Null()
   contains
      procedure :: read_from_file
      procedure :: output
      procedure :: delete_all
   end type StringList

contains

   ! чтение списка из файла
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

   ! хвостовая рекурсия для добавления в конец
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

   ! вывод списка
   subroutine output(this, output_file, header, position)
      class(StringList), intent(in) :: this
      character(*), intent(in) :: output_file, header, position
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

   ! удаление всех вхождений
   recursive subroutine delete_all(this, name)
      class(StringList), intent(inout) :: this
      character(*), intent(in) :: name
      type(node), pointer :: tmp

      if (.not. associated(this%head)) return

      if (this%head%value == name) then
         tmp => this%head
         this%head => this%head%next
         deallocate(tmp)
         call delete_all(this, name)
      else
         call delete_all_node(this%head%next, name)
      end if
   end subroutine delete_all

   ! рекурсивная процедура удаления для узлов
   recursive subroutine delete_all_node(head, name)
      type(node), pointer, intent(inout) :: head
      character(*), intent(in) :: name
      type(node), pointer :: tmp

      if (.not. associated(head)) return

      if (head%value == name) then
         tmp => head
         head => head%next
         deallocate(tmp)
         call delete_all_node(head, name)
      else
         call delete_all_node(head%next, name)
      end if
   end subroutine delete_all_node

   ! чтение последней строки из файла
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

end module List_Process
