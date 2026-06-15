module CircularList
   use Environment

   implicit none

   !базовый абстрактный тип для узла 
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

   !рекурсивный производный тип узла 
   type, extends(base_node), public :: node
      character(:), allocatable :: name
      type(node), pointer       :: next => null()
   contains
      procedure, pass :: print => print_node
      procedure, pass :: equals => node_equals
   end type node

   !инкапсулирующий тип для кольцевого списка
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
      procedure, private :: print_remaining
      procedure, private :: clear_list
      final :: circularlist_destructor
   end type CircularList

contains

   !деструктор
   subroutine circularlist_destructor(this)
      type(CircularList), intent(inout) :: this
      call this%clear_list()
   end subroutine circularlist_destructor

   !реализация полиморфного метода print
   subroutine print_node(this, unit)
      class(node), intent(in) :: this
      integer, intent(in) :: unit
      write(unit, '(a)', advance='no') trim(this%name)
   end subroutine print_node

   !реализация полиморфного метода equals
   logical function node_equals(this, name)
      class(node), intent(in) :: this
      character(*), intent(in) :: name
      node_equals = (this%name == name)
   end function node_equals

   !чтение имен из файла и формирование кольцевого списка
   subroutine read_names(this, input_file)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      call this%clear_list()

      open (file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO /= 0) return
      
      do
         read (In, '(a)', iostat=IO) buffer
         if (IO /= 0) exit
         buffer = trim(buffer)
         if (len(buffer) > 0) then
            call this%add_to_circular(buffer)
         end if
      end do
      close (In)
   end subroutine read_names

   !добавление в кольцевой список
   subroutine add_to_circular(this, name)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: name
      type(node), pointer :: new_node, last
      
      allocate(new_node)
      new_node%name = name
      
      if (.not. associated(this%head)) then
         !первый узел - указывает сам на себя
         this%head => new_node
         this%head%next => this%head
      else
         !находим последний узел 
         last => this%head
         do while (.not. associated(last%next, this%head))
            last => last%next
         end do
         
         last%next => new_node
         new_node%next => this%head
      end if
      
      this%size = this%size + 1
   end subroutine add_to_circular

   !основная игра
   subroutine play_game(this, start_name, m)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer, intent(in) :: m
      integer :: remaining, i
      type(node), pointer :: prev
      
      !проверка корректности входных данных
      if (this%size == 0) then
         write(*, '(a)') "Нет участников!"
         return
      end if
      
      if (m < 1) then
         write(*, '(a)') "Ошибка: шаг счета (m) должен быть >= 1!"
         return
      end if
      
      !находим начальный узел
      call this%find_starting_node(start_name)
      
      if (.not. associated(this%current)) then
         write(*, '(a)') "Ошибка: не найден начальный узел!"
         return
      end if
      
      remaining = this%size
      
      write(*, '(a, a)') "Начало игры с: ", trim(start_name)
      write(*, '(a, i0)') "Шаг счета: ", m
      write(*, *)
      write(*, '(a)') "Ход игры:"
      write(*, *)
      
      !частный случай: m = 1 - удаляем каждого подряд
      if (m == 1) then
         call play_game_m1(this, remaining)
         return
      end if
      
      !основной цикл игры
      do while (remaining > 1)
         !делаем m-1 шагов для нахождения удаляемого узла
         prev => this%current
         do i = 1, m-1
            prev => this%current
            this%current => this%current%next
         end do
         
         !удаляем текущий узел
         call remove_current_node(this, prev, remaining)
         
         !выводим оставшихся участников
         if (remaining > 0) then
            call this%print_remaining(remaining)
            write(*, *)
         end if
      end do
      
      !выводим победителя
      write(*, '(a)') "Последний оставшийся участник:"
      write(*, '(2x, a)') trim(this%current%name)
      write(*, *)
      
   end subroutine play_game

   !частный случай m = 1 - удаление каждого подряд 
   subroutine play_game_m1(this, remaining)
      class(CircularList), intent(inout) :: this
      integer, intent(inout) :: remaining
      type(node), pointer :: to_remove
      
      do while (remaining > 1)
         write(*, '(a, a)') "Выбывает: ", trim(this%current%name)
         
         to_remove => this%current
         this%current => this%current%next
         
         if (associated(to_remove, this%head)) then
            this%head => this%current
         end if
         
         deallocate(to_remove)
         remaining = remaining - 1
         
         call this%print_remaining(remaining)
         write(*, *)
      end do
   end subroutine play_game_m1

   !удаление текущего узла из кольцевого списка
   subroutine remove_current_node(this, prev, remaining)
      class(CircularList), intent(inout) :: this
      type(node), pointer, intent(in) :: prev
      integer, intent(inout) :: remaining
      type(node), pointer :: to_remove
      type(node), pointer :: last
      
      write(*, '(a, a)') "Выбывает: ", trim(this%current%name)
      
      to_remove => this%current
      
      !переподвязываем указатели
      prev%next => to_remove%next
      
      if (associated(to_remove, this%head)) then
         this%head => to_remove%next
      end if
      
      !переходим к следующему узлу
      this%current => to_remove%next
      
      deallocate(to_remove)
      remaining = remaining - 1
   end subroutine remove_current_node

   !поиск начального узла по имени
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
      
      !если имя не найдено, начинаем с первого
      write(*, '(a, a, a)') "Имя '", trim(start_name), "' не найдено. Начинаем с первого участника."
      this%current => this%head
   end subroutine find_starting_node

   !вывод оставшихся участников
   subroutine print_remaining(this, count)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: count
      type(node), pointer :: current
      integer :: i
      
      write(*, '(a, i0, a)') "Оставшиеся участники (", count, "):"
      write(*, '(a)', advance='no') "  "
      
      current => this%current
      do i = 1, count
         if (.not. associated(current)) exit  
         call current%print(output_unit)    
         if (i < count) then
            write(*, '(a)', advance='no') " -> "
         end if
         current => current%next
      end do
      write(*, *)
   end subroutine print_remaining

   !вывод результата в файл
   subroutine output_result(this, output_file)
      class(CircularList), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out
      
      open (file=output_file, newunit=Out, action='write', iostat=Out)
      if (Out /= 0) return 
      
      write(Out, '(a)') "Результат игры в считалку:"
      write(Out, '(a)') ""
      
      if (associated(this%current)) then
         write(Out, '(a)') "Победитель:"
         write(Out, '(2x, a)') trim(this%current%name)
         write(Out, '(a)') ""
         write(Out, '(a, i0)') "Всего участников было: ", this%size
      else
         write(Out, '(a)') "Нет участников или игра не проводилась"
      end if
      
      close(Out)
   end subroutine output_result

   !очистка списка 
   subroutine clear_list(this)
      class(CircularList), intent(inout) :: this
      type(node), pointer :: current_node, next_node
      
      if (.not. associated(this%head)) return
      
      !размыкаем кольцо и удаляем все узлы
      current_node => this%head
      next_node => current_node%next
      
      !временно размыкаем кольцо, сделав его линейным
      this%head%next => null()
      
      !удаляем все узлы итеративно
      do while (associated(current_node))
         next_node => current_node%next
         deallocate(current_node)
         current_node => next_node
      end do
      
      this%head => null()
      this%current => null()
      this%size = 0
   end subroutine clear_list

end module CircularList