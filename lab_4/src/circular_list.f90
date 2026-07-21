module CircularList
   use Environment
   implicit none

   private
   public :: CircularList

   ! Базовый абстрактный узел
   type, abstract, public :: base_node
   contains
      procedure(print_interface), deferred, pass :: print
      procedure(equals_interface), deferred, pass :: equals
   end type base_node

   abstract interface
      subroutine print_interface(this, unit)
         import :: base_node
         class(base_node), intent(in) :: this
         integer, intent(in) :: unit
      end subroutine
      logical function equals_interface(this, name)
         import :: base_node
         class(base_node), intent(in) :: this
         character(*), intent(in) :: name
      end function
   end interface

   ! Конкретный узел
   type, extends(base_node), public :: node
      character(:), allocatable :: name
      type(node), pointer :: next => null()
   contains
      procedure, pass :: print => print_node
      procedure, pass :: equals => node_equals
   end type node

   ! Основной тип
   type :: CircularList
      private
      type(node), pointer :: head => null()
      type(node), pointer :: current => null()
      integer :: size = 0
   contains
      procedure :: read_names
      procedure :: play_game
      procedure :: output_result
      procedure, private :: add_to_circular
      procedure, private :: find_starting_node
      procedure, private :: print_remaining
      procedure, private :: clear_list
      procedure, private :: remove_current
      final :: circularlist_destructor
   end type CircularList

contains

   subroutine circularlist_destructor(this)
      type(CircularList), intent(inout) :: this
      call this%clear_list()
   end subroutine

   subroutine print_node(this, unit)
      class(node), intent(in) :: this
      integer, intent(in) :: unit
      write(unit, '(a)', advance='no') trim(this%name)
   end subroutine

   logical function node_equals(this, name)
      class(node), intent(in) :: this
      character(*), intent(in) :: name
      node_equals = (this%name == name)
   end function

   ! ==================== РЕКУРСИВНЫЕ ОПЕРАЦИИ ====================

   ! Хвостовая рекурсия: добавление в конец (поддержание кольца)
   recursive subroutine add_to_circular(this, name, curr)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: name
      type(node), pointer, optional :: curr
      type(node), pointer :: new_node

      if (.not. associated(this%head)) then
         allocate(this%head)
         this%head%name = name
         this%head%next => this%head
         this%current => this%head
         this%size = 1
         return
      end if

      if (present(curr)) then
         if (associated(curr%next, this%head)) then  ! нашли конец
            allocate(new_node)
            new_node%name = name
            new_node%next => this%head
            curr%next => new_node
            this%size = this%size + 1
            return
         end if
         call add_to_circular(this, name, curr%next)  ! хвостовая рекурсия
      else
         call add_to_circular(this, name, this%head)
      end if
   end subroutine add_to_circular

   subroutine read_names(this, input_file)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      call this%clear_list()

      open(file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO /= 0) return

      do
         read(In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         buffer = trim(adjustl(buffer))
         if (len_trim(buffer) > 0) then
            call this%add_to_circular(buffer)
         end if
      end do
      close(In)
   end subroutine

   ! Поиск стартового узла (хвостовая рекурсия)
   recursive subroutine find_starting_node(this, start_name, curr)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      type(node), pointer, optional :: curr

      if (.not. associated(this%head)) return

      if (present(curr)) then
         if (curr%equals(start_name)) then
            this%current => curr
            return
         end if
         if (associated(curr%next, this%head)) then  ! обошли круг
            this%current => this%head
            write(*, '(a,a,a)') "Имя '", trim(start_name), "' не найдено. Начинаем с первого."
            return
         end if
         call find_starting_node(this, start_name, curr%next)
      else
         call find_starting_node(this, start_name, this%head)
      end if
   end subroutine find_starting_node

   ! Удаление текущего узла (рекурсивный вариант с prev)
   recursive subroutine remove_current(this, prev, remaining)
      class(CircularList), intent(inout) :: this
      type(node), pointer :: prev
      integer, intent(inout) :: remaining
      type(node), pointer :: to_remove

      write(*, '(a, a)') "Выбывает: ", trim(this%current%name)

      to_remove => this%current

      prev%next => to_remove%next

      if (associated(to_remove, this%head)) then
         this%head => to_remove%next
      end if

      this%current => to_remove%next

      deallocate(to_remove)
      remaining = remaining - 1
   end subroutine remove_current

   ! Печать оставшихся (хвостовая рекурсия)
   recursive subroutine print_remaining(this, count, curr, printed)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: count
      type(node), pointer, intent(in) :: curr
      integer, intent(inout) :: printed

      if (printed >= count) then
         write(*, *)
         return
      end if

      if (printed == 0) then
         write(*, '(a, i0, a)') "Оставшиеся участники (", count, "):"
         write(*, '(a)', advance='no') "  "
      end if

      call curr%print(output_unit)
      printed = printed + 1

      if (printed < count) write(*, '(a)', advance='no') " -> "

      call print_remaining(this, count, curr%next, printed)
   end subroutine print_remaining

   ! Основная игра
   subroutine play_game(this, start_name, m)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer, intent(in) :: m
      integer :: remaining, i, printed
      type(node), pointer :: prev, to_count

      if (this%size == 0) then
         write(*, '(a)') "Нет участников!"
         return
      end if
      if (m < 1) then
         write(*, '(a)') "Ошибка: m >= 1!"
         return
      end if

      call this%find_starting_node(start_name)

      remaining = this%size
      write(*, '(a,a)') "Начало игры с: ", trim(start_name)
      write(*, '(a,i0)') "Шаг счета: ", m
      write(*, *)

      do while (remaining > 1)
         prev => this%current
         to_count => this%current

         ! Отсчитываем m-1 шагов (можно сделать рекурсивно, но для ясности цикл)
         do i = 1, m-1
            prev => to_count
            to_count => to_count%next
         end do

         this%current => to_count
         call this%remove_current(prev, remaining)

         if (remaining > 0) then
            printed = 0
            call this%print_remaining(remaining, this%current, printed)
            write(*, *)
         end if
      end do

      write(*, '(a)') "Последний оставшийся участник:"
      write(*, '(2x,a)') trim(this%current%name)
      write(*, *)
   end subroutine play_game

   subroutine output_result(this, output_file)
      class(CircularList), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out

      open(file=output_file, newunit=Out, action='write', iostat=Out)
      if (Out /= 0) return

      write(Out, '(a)') "Результат игры в считалку:"
      write(Out, '(a)') ""
      if (associated(this%current)) then
         write(Out, '(a)') "Победитель:"
         write(Out, '(2x,a)') trim(this%current%name)
         write(Out, '(a,i0)') "Всего участников было: ", this%size
      else
         write(Out, '(a)') "Нет участников"
      end if
      close(Out)
   end subroutine

   subroutine clear_list(this)
      class(CircularList), intent(inout) :: this
      type(node), pointer :: curr, nextn

      if (.not. associated(this%head)) return

      ! Размыкаем кольцо
      curr => this%head%next
      this%head%next => null()

      do while (associated(curr))
         nextn => curr%next
         deallocate(curr)
         curr => nextn
      end do

      deallocate(this%head)
      this%head => null()
      this%current => null()
      this%size = 0
   end subroutine

end module CircularList