module List_Process
   use Environment

   implicit none

   !тип узла
   type, public :: node
      character(:), allocatable :: value
      type(node), allocatable    :: next
   end type node

   !инкапсулирующий тип для списка
   type, public :: StringList
   private
      type(node), allocatable :: head
      character(:), allocatable :: last_line_to_delete
      integer :: list_size = 0
   contains
      procedure, public :: read_from_file
      procedure, public :: output
      procedure, public :: last_line_from_file
      procedure, public :: delete_all
      procedure, private :: add_to_end
      procedure, private :: clear_list
      procedure, private :: delete_all_recursive
      procedure, private :: output_recursive
   end type StringList

contains

   !чтение списка из файла
   subroutine read_from_file(this, input_file)
      class(StringList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      !очистка существующего списка
      if (allocated(this%head)) then
         call this%clear_list(this%head)
         this%list_size = 0
      end if

      open (file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO /= 0) return

      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         if (len_trim(buffer) > 0) then
            call this%add_to_end(this%head, buffer)
            this%list_size = this%list_size + 1
         end if
      end do
      close (In)
   end subroutine read_from_file

   !добавление в конец 
   recursive subroutine add_to_end(this, head, val)
      class(StringList), intent(inout) :: this
      type(node), allocatable, intent(inout) :: head
      character(*), intent(in) :: val

      if (.not. allocated(head)) then
         allocate(head)
         head%value = val
      else
         call this%add_to_end(head%next, val)
      end if
   end subroutine add_to_end

   !очистка списка 
   recursive subroutine clear_list(this, head)
      class(StringList), intent(inout) :: this
      type(node), allocatable, intent(inout) :: head

      if (allocated(head)) then
         if (allocated(head%next)) then
            call this%clear_list(head%next)
         end if
         deallocate(head)
      end if
   end subroutine clear_list

   !вывод списка в файл 
   subroutine output(this, output_file, header, position)
      class(StringList), intent(in) :: this
      character(*), intent(in) :: output_file, header, position
      integer :: Out

      open (newunit=Out, file=output_file, status='unknown', &
            position=merge('append', 'rewind', position == 'append'), action='write')

      if (position == 'append') write (Out, '(a)') ""
      write (Out, '(a)') header

      if (allocated(this%head)) then
         call this%output_recursive(Out, this%head)
      else
         write (Out, '(a)') "  (список пуст)"
      end if

      close (Out)
   end subroutine output


   !рекурсивный вывод 
   recursive subroutine output_recursive(this, unit, current)
      class(StringList), intent(in) :: this
      integer, intent(in) :: unit
      type(node), intent(in) :: current

      ! прямой вывод без вызова отдельной процедуры
      write(unit, '(5x, a)') trim(current%value)
      
      if (allocated(current%next)) then
         call this%output_recursive(unit, current%next)
      end if
   end subroutine output_recursive

   !чтение последней строки из файла (для удаления)
   subroutine last_line_from_file(this, input_file)
      class(StringList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      open (file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO /= 0) return

      if (allocated(this%last_line_to_delete)) deallocate(this%last_line_to_delete)
      
      this%last_line_to_delete = ""
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         buffer = trim(buffer)
         if (len_trim(buffer) > 0) this%last_line_to_delete = buffer
      end do
      close (In)
   end subroutine last_line_from_file

   !удаление всех вхождений last_line_to_delete
   subroutine delete_all(this)
      class(StringList), intent(inout) :: this
      integer :: deleted_count

      if (.not. allocated(this%head)) return
      if (.not. allocated(this%last_line_to_delete)) return
      if (len_trim(this%last_line_to_delete) == 0) return

      deleted_count = 0
      call this%delete_all_recursive(this%head, deleted_count)
      this%list_size = this%list_size - deleted_count
   end subroutine delete_all

   !рекурсивное удаление узлов 
   recursive subroutine delete_all_recursive(this, head, deleted_count)
      class(StringList), intent(inout) :: this
      type(node), allocatable, intent(inout) :: head
      integer, intent(inout) :: deleted_count
      type(node), allocatable :: tmp

      if (.not. allocated(head)) return

      ! прямое сравнение строк
      if (head%value == this%last_line_to_delete) then
         call move_alloc(head, tmp)
         
         if (allocated(tmp%next)) then
            call move_alloc(tmp%next, head)
         end if
         
         deleted_count = deleted_count + 1
         
         if (allocated(head)) then
            call this%delete_all_recursive(head, deleted_count)
         end if
      else
         call this%delete_all_recursive(head%next, deleted_count)
      end if
   end subroutine delete_all_recursive

end module List_Process