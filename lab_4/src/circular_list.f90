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

      if (this%size == size(this%nodes)) then
         new_cap = max(8, size(this%nodes) * 2)
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

   ! Индекс следующего активного узла по кругу
   function next_index(this, idx) result(res)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: idx
      integer :: res, j
      logical :: found

      if (this%size == 0) then
         res = 0
      else
         j = idx
         found = .false.
         
         ! Проверяем, есть ли активные узлы
         do while (.not. found)
            j = j + 1
            if (j > this%size) j = 1
            if (this%nodes(j)%active) then
               res = j
               found = .true.
            end if
            if (j == idx) then   ! обошли круг, активных не осталось
               res = idx
               found = .true.
            end if
         end do
      end if
   end function next_index

   subroutine read_names(this, input_file)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(100) :: buffer
      logical :: end_of_file

      this%size = 0
      this%current = 0

      open(file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO == 0) then
         end_of_file = .false.
         do while (.not. end_of_file)
            read(In, '(a)', iostat=IO) buffer
            if (IO /= 0) then
               end_of_file = .true.
            else
               buffer = adjustl(buffer)
               if (len_trim(buffer) > 0) call this%add_to_circular(buffer)
            end if
         end do
         close(In)
      end if
   end subroutine

   ! Поиск стартового узла 
   subroutine find_starting_node(this, start_name)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer :: j
      logical :: found

      if (this%size == 0) then
         this%current = 0
      else
         found = .false.
         j = 1
         do while (j <= this%size .and. .not. found)
            if (this%nodes(j)%active .and. this%nodes(j)%name == start_name) then
               this%current = j
               found = .true.
            end if
            j = j + 1
         end do

         if (.not. found) then
            ! Не нашли - начинаем с первого
            this%current = 1
            write(*, '(a,a,a)') "Имя '", start_name, "' не найдено. Начинаем с первого."
         end if
      end if
   end subroutine find_starting_node

   ! Удаление текущего узла
   subroutine remove_current(this, remaining)
      class(CircularList), intent(inout) :: this
      integer, intent(inout) :: remaining
      integer :: nxt

      if (this%current /= 0) then
         write(*, '(a, a)') "Выбывает: ", this%nodes(this%current)%name

         nxt = this%next_index(this%current)
         this%nodes(this%current)%active = .false.
         remaining = remaining - 1

         this%current = nxt
      end if
   end subroutine remove_current

   ! Печать оставшихся 
   subroutine print_remaining(this, count)
      class(CircularList), intent(in) :: this
      integer, intent(in) :: count
      integer :: idx, printed

      if (count == 0 .or. this%current == 0) then
         write(*, '(a)') "Нет оставшихся участников"
      else
         write(*, '(a, i0, a)') "Оставшиеся участники (", count, "):"
         write(*, '(a)', advance='no') "  "

         idx = this%current
         printed = 0
         
         do while (printed < count)
            if (this%nodes(idx)%active) then
               write(*, '(a)', advance='no') trim(this%nodes(idx)%name)
               printed = printed + 1
               if (printed < count) then
                  write(*, '(a)', advance='no') " -> "
               end if
            end if
            idx = this%next_index(idx)
         end do
         
         write(*, *)
      end if
   end subroutine print_remaining

   ! Печать полного списка участников
   subroutine print_full_list(this)
      class(CircularList), intent(in) :: this
      integer :: idx

      if (this%size == 0) then
         write(*, '(a)') "Список пуст"
      else
         write(*, '(a, i0, a)') "Все участники (", this%size, "):"
         write(*, '(a)', advance='no') "  "

         do idx = 1, this%size
            write(*, '(a)', advance='no') trim(this%nodes(idx)%name)
            if (idx < this%size) then
               write(*, '(a)', advance='no') " -> "
            else
               write(*, *)
               write(*, *)
            end if
         end do
      end if
   end subroutine print_full_list

   ! Основная игра
   subroutine play_game(this, start_name, m)
      class(CircularList), intent(inout) :: this
      character(*), intent(in) :: start_name
      integer, intent(in) :: m
      integer :: remaining, i, idx
      logical :: game_over

      if (this%size == 0) then
         write(*, '(a)') "Нет участников!"
      else if (m < 1) then
         write(*, '(a)') "Ошибка: m >= 1!"
      else
         ! Вывод полного списка
         call this%print_full_list()

         call this%find_starting_node(start_name)

         if (this%current /= 0) then
            remaining = this%size
            write(*, '(a,a)') "Начало игры с: ", start_name
            write(*, '(a,i0)') "Шаг счета: ", m
            write(*, *)

            game_over = .false.
            do while (.not. game_over)
               if (remaining <= 1) then
                  game_over = .true.
               else
                  idx = this%current

                  ! Отсчитываем m-1 шагов
                  i = 1
                  do while (i <= m - 1)
                     idx = this%next_index(idx)
                     i = i + 1
                  end do

                  this%current = idx
                  call this%remove_current(remaining)

                  call this%print_remaining(remaining)
                  write(*, *)
               end if
            end do

            if (this%current > 0) then
               write(*, '(a)') "Последний оставшийся участник:"
               write(*, '(2x,a)') this%nodes(this%current)%name
               write(*, *)
            end if
         else
            write(*, '(a)') "Нет активных узлов!"
         end if
      end if
   end subroutine play_game

   subroutine output_result(this, output_file)
      class(CircularList), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out, IO

      open(file=output_file, newunit=Out, action='write', iostat=IO)
      if (IO == 0) then
         write(Out, '(a)') "Результат игры в считалку:"
         write(Out, '(a)') ""
         if (this%current > 0 .and. allocated(this%nodes)) then
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