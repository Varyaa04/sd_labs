module List_Process
   use Environment

   implicit none

   ! базовый абстрактный тип для узла (полиморфизм)
   type, public, abstract :: base_node
   contains
      procedure(print_interface), deferred, pass :: print
      procedure(equals_interface), deferred, pass :: equals
   end type base_node

   abstract interface
      subroutine print_interface(this, unit)
         import base_node
         class(base_node), intent(in) :: this
         integer, intent(in) :: unit
      end subroutine print_interface
      
      logical function equals_interface(this, value)
         import base_node
         class(base_node), intent(in) :: this
         character(*), intent(in) :: value
      end function equals_interface
   end interface

   ! рекурсивный производный тип узла (наследование)
   type, extends(base_node), public :: node
      character(:), allocatable :: value
      type(node), pointer       :: next => Null()
   contains
      procedure, pass :: print => print_node
      procedure, pass :: equals => node_equals
   end type node

   ! инкапсулирующий тип для списка
   type, public :: StringList
   private
      type(node), pointer :: head => Null()
      character(:), allocatable :: last_line_to_delete
      integer :: list_size = 0
   contains
      procedure, public :: read_from_file
      procedure, public :: output
      procedure, public :: delete_last_line_from_file
      procedure, public :: delete_all
      procedure, private :: add_to_end
      procedure, private :: clear_list
      procedure, private :: write_values_polymorphic
      procedure, private :: delete_all_recursive
      final :: stringlist_destructor
   end type StringList

contains

   ! завершаемая функция - автоматическое удаление списка
   subroutine stringlist_destructor(this)
      type(StringList), intent(inout) :: this
      type(node), pointer :: current, next_node
      
      current => this%head
      do while (associated(current))
         next_node => current%next
         deallocate(current)
         current => next_node
      end do
      this%head => Null()
      this%list_size = 0
   end subroutine stringlist_destructor

   ! реализация полиморфного метода print
   subroutine print_node(this, unit)
      class(node), intent(in) :: this
      integer, intent(in) :: unit
      write(unit, '(5x, a)') trim(this%value)
   end subroutine print_node

   ! реализация полиморфного метода equals
   logical function node_equals(this, value)
      class(node), intent(in) :: this
      character(*), intent(in) :: value
      node_equals = (this%value == value)
   end function node_equals

   ! чтение списка из файла
   subroutine read_from_file(this, input_file)
      class(StringList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      if (associated(this%head)) then
         call this%clear_list(this%head)
         this%head => Null()
         this%list_size = 0
      end if

      open (file=input_file, newunit=In, status='old', action='read')
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         if (len_trim(buffer) > 0) then
            call this%add_to_end(this%head, trim(buffer))
            this%list_size = this%list_size + 1
         end if
      end do
      close (In)
   end subroutine read_from_file

   ! добавление в конец (рекурсивная)
   recursive subroutine add_to_end(this, head, val)
      class(StringList), intent(inout) :: this
      type(node), pointer, intent(inout) :: head
      character(*), intent(in) :: val

      if (.not. associated(head)) then
         allocate(head)
         head%value = val
         head%next => Null()
      else
         call this%add_to_end(head%next, val)
      end if
   end subroutine add_to_end

   ! очистка списка (рекурсивная)
   recursive subroutine clear_list(this, head)
      class(StringList), intent(inout) :: this
      type(node), pointer, intent(inout) :: head
      if (associated(head)) then
         if (associated(head%next)) then
            call this%clear_list(head%next)
         end if
         deallocate(head)
         head => Null()
      end if
   end subroutine clear_list

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
         call this%write_values_polymorphic(Out, this%head)
      else
         write (Out, '(a)') "  (список пуст)"
      end if
      close (Out)
   end subroutine output

   ! полиморфная рекурсивная запись значений
   recursive subroutine write_values_polymorphic(this, out, head)
      class(StringList), intent(in) :: this
      integer, intent(in) :: out
      class(base_node), pointer, intent(in) :: head
      type(node), pointer :: node_ptr

      if (.not. associated(head)) return
      
      select type(head)
      type is (node)
         node_ptr => head
         call node_ptr%print(out)
         call this%write_values_polymorphic(out, node_ptr%next)
      end select
   end subroutine write_values_polymorphic

   ! чтение последней строки из файла и сохранение её для удаления
   subroutine delete_last_line_from_file(this, input_file)
      class(StringList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      open (file=input_file, newunit=In, status='old', action='read')
      this%last_line_to_delete = ""
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         if (len_trim(buffer) > 0) this%last_line_to_delete = trim(buffer)
      end do
      close (In)
   end subroutine delete_last_line_from_file

   ! удаление всех вхождений
   recursive subroutine delete_all(this)
      class(StringList), intent(inout) :: this
      integer :: deleted_count

      if (.not. associated(this%head)) return
      if (.not. allocated(this%last_line_to_delete)) return

      deleted_count = 0
      call this%delete_all_recursive(this%head, deleted_count)
      this%list_size = this%list_size - deleted_count
   end subroutine delete_all

   ! рекурсивная процедура удаления для узлов
   recursive subroutine delete_all_recursive(this, head, deleted_count)
      class(StringList), intent(inout) :: this
      type(node), pointer, intent(inout) :: head
      integer, intent(inout) :: deleted_count
      type(node), pointer :: tmp

      if (.not. associated(head)) return

      if (head%equals(this%last_line_to_delete)) then
         tmp => head
         head => head%next
         deallocate(tmp)
         deleted_count = deleted_count + 1
         call this%delete_all_recursive(head, deleted_count)
      else
         call this%delete_all_recursive(head%next, deleted_count)
      end if
   end subroutine delete_all_recursive

end module List_Process