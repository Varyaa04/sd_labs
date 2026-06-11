module CircularList
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
      
      logical function equals_interface(this, name)
         import base_node
         class(base_node), intent(in) :: this
         character(*), intent(in) :: name
      end function equals_interface
   end interface

   ! рекурсивный производный тип узла (наследование)
   type, extends(base_node), public :: node
      character(:), allocatable :: name
      type(node), pointer       :: next => null()
   contains
      procedure, pass :: print => print_node
      procedure, pass :: equals => node_equals
   end type node

   ! инкапсулирующий тип для кольцевого списка
   type, public :: CircularList
   private
      type(node), pointer :: head => null()
      type(node), pointer :: current => null()
      integer :: size = 0
   contains
      procedure, public :: read_names
      procedure, public :: play_game
      procedure, public :: output_result
      procedure, private :: add_to_circular
      procedure, private :: find_starting_node
      procedure, private :: counting_game_recursive
      procedure, private :: print_remaining
      procedure, private :: clear_list
      final :: circularlist_destructor
   end type CircularList

contains

   ! завершаемая функция - автоматическое удаление списка
   subroutine circularlist_destructor(this)
      type(CircularList), intent(inout) :: this
      type(node), pointer :: current_node, next_node
      
      if (.not. associated(this%head)) return
      
      current_node => this%head
      do
         next_node => current_node%next
         deallocate(current_node)
         current_node => next_node
         if (associated(current_node, this%head)) exit
      end do
      
      this%head => null()
      this%current => null()
      this%size = 0
   end subroutine circularlist_destructor

   ! реализация полиморфного метода print
   subroutine print_node(this, unit)
      class(node), intent(in) :: this
      integer, intent(in) :: unit
      write(unit, '(2x, a)', advance='no') trim(this%name)
   end subroutine print_node

   ! реализация полиморфного метода equals
   logical function node_equals(this, name)
      class(node), intent(in) :: this
      character(*), intent(in) :: name
      node_equals = (this%name == name)
   end function node_equals

   ! чтение имен из файла и формирование кольцевого списка
   subroutine read_names(this, input_file)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      call this%clear_list()
      this%size = 0

      open (file=input_file, newunit=In, status='old', action='read')
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         if (len_trim(buffer) > 0) then
            call this%add_to_circular(trim(buffer))
            this%size = this%size + 1
         end if
      end do
      close (In)

      ! замыкаем кольцо
      if (associated(this%head) .and. this%size > 0) then
         call close_circular(this%head)
      end if
   end subroutine read_names

   ! рекурсивное добавление в конец с замыканием в кольцо
   recursive subroutine add_to_circular(this, name)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: name
      
      if (.not. associated(this%head)) then
         allocate(this%head)
         this%head%name = name
         this%head%next => this%head
         this%current => this%head
      else
         call add_recursive(this%head, name)
      end if
   end subroutine add_to_circular

   ! рекурсивная вспомогательная процедура
   recursive subroutine add_recursive(head, name)
      type(node), pointer, intent(in) :: head
      character(*), intent(in) :: name
      type(node), pointer :: current
      
      current => head
      if (.not. associated(current%next, head)) then
         call add_recursive(current%next, name)
      else
         allocate(current%next)
         current%next%name = name
         current%next%next => head
      end if
   end subroutine add_recursive

   ! замыкание кольца (рекурсивное)
   recursive subroutine close_circular(head)
      type(node), pointer, intent(in) :: head
      
      if (.not. associated(head%next)) then
         head%next => head
      else if (.not. associated(head%next, head)) then
         call close_circular(head%next)
      end if
   end subroutine close_circular

   ! основная игра
   subroutine play_game(this, start_name, m)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer, intent(in) :: m
      
      if (this%size == 0) return
      
      call this%find_starting_node(start_name)
      
      write(*, '(a, a)') "Начало игры с: ", trim(start_name)
      write(*, '(a, i0)') "Шаг счета: ", m
      write(*, *)
      write(*, '(a)') "Ход игры:"
      write(*, *)
      
      call this%counting_game_recursive(this%current, m, this%size)
   end subroutine play_game

   ! рекурсивная процедура "считалки"
   recursive subroutine counting_game_recursive(this, start_node, m, remaining)
      class(CircularList), intent(inout) :: this
      type(node), pointer, intent(in) :: start_node
      integer, intent(in) :: m, remaining
      type(node), pointer :: current, prev, to_remove
      integer :: i
      
      if (remaining == 1) then
         write(*, '(a)') "Последний оставшийся участник:"
         this%current => start_node
         return
      end if
      
      ! находим m-го человека
      current => start_node
      do i = 2, m
         prev => current
         current => current%next
      end do
      
      ! выводим удаляемого
      write(*, '(a, a)') "Выбывает: ", trim(current%name)
      
      ! удаляем текущий узел
      to_remove => current
      
      ! находим предыдущий узел
      prev => start_node
      do while (.not. associated(prev%next, to_remove))
         prev => prev%next
      end do
      
      ! переподвязываем указатели
      prev%next => to_remove%next
      
      ! следующий счет начнется со следующего за удаленным
      this%current => to_remove%next
      
      ! освобождаем память
      deallocate(to_remove)
      
      ! выводим оставшихся участников
      call this%print_remaining(this%current, remaining - 1)
      write(*, *)
      
      ! рекурсивный вызов с оставшимися участниками
      call this%counting_game_recursive(this%current, m, remaining - 1)
      
   end subroutine counting_game_recursive

   ! поиск начального узла по имени
   subroutine find_starting_node(this, start_name)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      type(node), pointer :: current
      
      if (.not. associated(this%head)) return
      
      current => this%head
      do
         if (current%equals(start_name)) then
            this%current => current
            return
         end if
         current => current%next
         if (associated(current, this%head)) exit
      end do
      
      ! если имя не найдено, начинаем с первого
      this%current => this%head
   end subroutine find_starting_node

   ! вывод оставшихся участников
   subroutine print_remaining(this, start_node, count)
      class(CircularList), intent(in) :: this
      type(node), pointer, intent(in) :: start_node
      integer, intent(in) :: count
      type(node), pointer :: current
      integer :: i
      
      write(*, '(a, a, a)') "Оставшиеся участники (начинаем с: ", trim(start_node%name), "):"
      write(*, '(a)', advance='no') "  "
      
      current => start_node
      do i = 1, count
         call current%print(6)
         if (i < count) then
            write(*, '(a)', advance='no') " -> "
         end if
         current => current%next
      end do
      write(*, *)
   end subroutine print_remaining

   ! вывод результата в файл
   subroutine output_result(this, output_file)
      class(CircularList), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out
      
      open (file=output_file, newunit=Out, action='write')
      write(Out, '(a)') "Результат игры в считалку:"
      write(Out, '(a)') ""
      
      if (associated(this%current)) then
         write(Out, '(a)') "Победитель:"
         write(Out, '(2x, a)') trim(this%current%name)
      else
         write(Out, '(a)') "Нет участников"
      end if
      
      close(Out)
   end subroutine output_result

   ! очистка списка
   recursive subroutine clear_list(this)
      class(CircularList), intent(inout) :: this
      
      if (associated(this%head)) then
         call clear_recursive(this%head)
         this%head => null()
         this%current => null()
      end if
   end subroutine clear_list

   ! рекурсивная очистка
   recursive subroutine clear_recursive(node_ptr)
      type(node), pointer, intent(inout) :: node_ptr
      type(node), pointer :: next_node
      
      if (associated(node_ptr)) then
         if (.not. associated(node_ptr%next, node_ptr)) then
            next_node => node_ptr%next
            deallocate(node_ptr)
            call clear_recursive(next_node)
         else
            deallocate(node_ptr)
         end if
      end if
   end subroutine clear_recursive

end module CircularList