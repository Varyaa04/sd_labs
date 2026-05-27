module text_processing
   use environment

   implicit none
   private

   type, public :: text_node
      character(:, CH_), allocatable :: line
      type(text_node),   allocatable :: next
   end type text_node

   type, public :: dir_node
      character(1, CH_)            :: dir
      type(dir_node), allocatable  :: next
   end type dir_node

   character(1, CH_), parameter :: CHAR_F = 'f'
   character(1, CH_), parameter :: CHAR_F_BIG = 'F'
   character(1, CH_), parameter :: CHAR_B = 'b'
   character(1, CH_), parameter :: CHAR_B_BIG = 'B'

   public :: text_size, get_line, paginate

contains
  !рекурсивно вычисляет количество элементов в связанном списке текста
   pure recursive integer(I_) function text_size(head) result(n)
      type(text_node), intent(in) :: head

      if (allocated(head%next)) then
         n = 1 + text_size(head%next)
      else
         n = 1
      end if
   end function text_size

   !возвращает строку по номеру
   pure recursive function get_line(head, k) result(res)
      type(text_node),  intent(in) :: head
      integer(I_),      intent(in) :: k
      character(:, CH_), allocatable :: res

      if (k == 1) then
         if (allocated(head%line)) then
            res = head%line
         else
            allocate(character(0, CH_) :: res)
         end if
      else if (allocated(head%next)) then
         res = get_line(head%next, k - 1)
      else
         allocate(character(0, CH_) :: res)
      end if
   end function get_line

   !вычисляет следующую позицию по команде
   pure integer(I_) function next_pos(pos, dir, win_size, total)
      integer(I_),      intent(in) :: pos, win_size, total
      character(1, CH_), intent(in) :: dir

      if (dir == CHAR_F .or. dir == CHAR_F_BIG) then
         if (pos + win_size <= total) then
            next_pos = pos + win_size
         else
            next_pos = total - win_size + 1
            if (next_pos < 1) next_pos = 1
         end if
      else if (dir == CHAR_B .or. dir == CHAR_B_BIG) then
         next_pos = max(pos - win_size, 1)
      else
         next_pos = pos
      end if
   end function next_pos

   !рекурсивная функция пролистывания
   recursive function paginate(text, dirs, win_size, pos, total) result(actions)
      type(text_node), allocatable, intent(in) :: text
      type(dir_node),  allocatable, intent(in) :: dirs
      integer(I_),                   intent(in) :: win_size, pos, total, i
      character(:, CH_), allocatable :: actions(:)
      character(:, CH_), allocatable :: tail_actions(:), window_lines(:)
      integer(I_) :: new_pos, end_pos, n_lines, n

      !вычисляем последнюю строку текущего окна
      end_pos = min(pos + win_size - 1, total)
      n_lines = end_pos - pos + 1

      if (.not. allocated(dirs)) then
         allocate(character(100, CH_) :: actions(n_lines))
         actions = [(get_line(text, pos + i - 1), i = 1, n_lines)]
      else
         new_pos = next_pos(pos, dirs%dir, win_size, total)
         
         tail_actions = paginate(text, dirs%next, win_size, new_pos, total)
         
         allocate(character(100, CH_) :: window_lines(n_lines))
         window_lines = [(get_line(text, pos + i - 1), i = 1, n_lines)]
         
         n = n_lines + 1 + size(tail_actions)
         allocate(character(100, CH_) :: actions(n))
         
         actions(1:n_lines) = window_lines
         actions(n_lines + 1) = dirs%dir
         actions(n_lines + 2:) = tail_actions
      end if
   end function paginate

end module text_processing
