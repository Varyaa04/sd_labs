module CircularList
   use Environment
   implicit none

   public :: CircularList

   ! Узел списка
   type, public :: node
      character(:), allocatable :: name
      logical :: active = .true.
   end type node

   ! Основной тип
   type :: CircularList
      private
      type(node), allocatable :: nodes(:)
      integer :: size = 0      ! сколько узлов реально добавлено 
      integer :: current = 0   ! индекс текущего узла в nodes
   contains
      procedure :: read_names
      procedure :: play_game
      procedure :: output_result
      procedure, private :: add_to_circular
      procedure, private :: find_starting_node
      procedure, private :: print_remaining
      procedure, private :: print_full_list
      procedure, private :: remove_current
      procedure, private :: next_index
      procedure, private :: ensure_capacity
   end type CircularList

contains

   ! Увеличивает ёмкость массива nodes при необходимости
   subroutine ensure_capacity(this)
      class(CircularList), intent(inout) :: this
      type(node), allocatable :: tmp(:)
      integer :: new_cap

      if (.not. allocated(this%nodes)) then
         allocate(this%nodes(8))
      else if (this%size < size(this%nodes)) then
         ! ничего не делаем
      else
         new_cap = size(this%nodes) * 2
         allocate(tmp(new_cap))
         tmp(1:this%size) = this%nodes(1:this%size)
         call move_alloc(tmp, this%nodes)
      end if
   end subroutine ensure_capacity

   ! Добавление в конец 
   subroutine add_to_circular(this, name)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: name

      call this%ensure_capacity()
      this%size = this%size + 1
      this%nodes(this%size)%name = name
      this%nodes(this%size)%active = .true.

      if (this%size == 1) this%current = 1
   end subroutine add_to_circular

   ! Индекс следующего активного узла после idx, по кругу
   function next_index(this, idx) result(res)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: idx
      integer :: res, j

      j = idx
      do
         j = j + 1
         if (j > this%size) j = 1
         if (this%nodes(j)%active) then
            res = j
            exit
         end if
         if (j == idx) then   ! обошли круг, активных не осталось
            res = idx
            exit
         end if
      end do
   end function next_index

   subroutine read_names(this, input_file)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer

      if (allocated(this%nodes)) deallocate(this%nodes)
      this%size = 0
      this%current = 0

      open(file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO == 0) then
         do
            read(In, '(a)', iostat=IO) buffer
            if (IO /= 0) exit
            buffer = adjustl(buffer)
            if (len_trim(buffer) > 0) then
               call this%add_to_circular(buffer)
            end if
         end do
         close(In)
      end if
   end subroutine

   ! Поиск стартового узла (хвостовая рекурсия)
   recursive subroutine find_starting_node(this, start_name, idx)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer, intent(in), optional :: idx
      integer :: j

      if (this%size > 0) then
         j = 1
         if (present(idx)) j = idx

         if (this%nodes(j)%active .and. this%nodes(j)%name == start_name) then
            this%current = j
         else if (j >= this%size) then  ! обошли весь массив, не нашли
            this%current = 1
            write(*, '(a,a,a)') "Имя '", start_name, "' не найдено. Начинаем с первого."
         else
            call this%find_starting_node(start_name, j + 1)
         end if
      end if
   end subroutine find_starting_node

   ! Удаление текущего узла
   subroutine remove_current(this, remaining)
      class(CircularList), intent(inout) :: this
      integer, intent(inout) :: remaining
      integer :: nxt

      write(*, '(a, a)') "Выбывает: ", this%nodes(this%current)%name

      nxt = this%next_index(this%current)
      this%nodes(this%current)%active = .false.
      remaining = remaining - 1

      this%current = nxt
   end subroutine remove_current

   ! Печать оставшихся (хвостовая рекурсия)
   recursive subroutine print_remaining(this, count, idx, printed)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: count
      integer, intent(in) :: idx
      integer, intent(inout) :: printed

      if (printed < count) then
         if (printed == 0) then
            write(*, '(a, i0, a)') "Оставшиеся участники (", count, "):"
            write(*, '(a)', advance='no') "  "
         end if

         write(*, '(a)', advance='no') this%nodes(idx)%name
         printed = printed + 1

         if (printed < count) then
            write(*, '(a)', advance='no') " -> "
            call this%print_remaining(count, this%next_index(idx), printed)
         else
            write(*, *)
         end if
      end if
   end subroutine print_remaining

   ! Печать полного списка участников
   recursive subroutine print_full_list(this, idx, printed)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: idx
      integer, intent(inout) :: printed

      if (printed < this%size) then
         if (printed == 0) then
            write(*, '(a, i0, a)') "Все участники (", this%size, "):"
            write(*, '(a)', advance='no') "  "
         end if

         write(*, '(a)', advance='no') this%nodes(idx)%name
         printed = printed + 1

         if (printed < this%size) then
            write(*, '(a)', advance='no') " -> "
            call this%print_full_list(idx + 1, printed)
         else
            write(*, *)
            write(*, *)
         end if
      end if
   end subroutine print_full_list

   ! Основная игра
   subroutine play_game(this, start_name, m)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer, intent(in) :: m
      integer :: remaining, i, printed, idx

      if (this%size == 0) then
         write(*, '(a)') "Нет участников!"
      else if (m < 1) then
         write(*, '(a)') "Ошибка: m >= 1!"
      else
         ! Вывод полного списка
         printed = 0
         call this%print_full_list(1, printed)

         call this%find_starting_node(start_name)

         remaining = this%size
         write(*, '(a,a)') "Начало игры с: ", start_name
         write(*, '(a,i0)') "Шаг счета: ", m
         write(*, *)

         do while (remaining > 1)
            idx = this%current

            ! Отсчитываем m-1 шагов
            do i = 1, m - 1
               idx = this%next_index(idx)
            end do

            this%current = idx
            call this%remove_current(remaining)

            if (remaining > 0) then
               printed = 0
               call this%print_remaining(remaining, this%current, printed)
               write(*, *)
            end if
         end do

         write(*, '(a)') "Последний оставшийся участник:"
         write(*, '(2x,a)') this%nodes(this%current)%name
         write(*, *)
      end if
   end subroutine play_game

   subroutine output_result(this, output_file)
      class(CircularList), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out

      open(file=output_file, newunit=Out, action='write', iostat=Out)
      if (Out == 0) then
         write(Out, '(a)') "Результат игры в считалку:"
         write(Out, '(a)') ""
         if (this%current > 0) then
            write(Out, '(a)') "Победитель:"
            write(Out, '(2x,a)') this%nodes(this%current)%name
            write(Out, '(a,i0)') "Всего участников было: ", this%size
         else
            write(Out, '(a)') "Нет участников"
         end if
         close(Out)
      end if
   end subroutine

end module CircularList