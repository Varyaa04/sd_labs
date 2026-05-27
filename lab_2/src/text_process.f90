module text_processing
   use environment

   implicit none
   private

   !хранит строки текста
   type, public :: text_node
      character(:, CH_), allocatable :: line
      type(text_node),   allocatable :: next
   end type text_node

   !хранит команды
   type, public :: dir_node
      character(1, CH_)            :: dir
      type(dir_node), allocatable  :: next
   end type dir_node

   character(1, CH_), parameter :: CHAR_F = 'f'
   character(1, CH_), parameter :: CHAR_F_BIG = 'F'
   character(1, CH_), parameter :: CHAR_B = 'b'
   character(1, CH_), parameter :: CHAR_B_BIG = 'B'

   public :: read_all_data, write_full_output
   public :: text_size, get_line, paginate

contains

   
   !ВВОД/ВЫВОД

   !читаем все данные 
   subroutine read_all_data(file1, file2, text_list, dir_list, win_size)
      character(*),                    intent(in)  :: file1, file2
      type(text_node), allocatable,    intent(out) :: text_list
      type(dir_node),  allocatable,    intent(out) :: dir_list
      integer(I_),                     intent(out) :: win_size
      integer :: in1, in2, ios

      open (newunit=in1, file=file1, encoding=E_, iostat=ios, action='read')
      if (ios /= 0) stop "Error opening text.txt"
      
      open (newunit=in2, file=file2, encoding=E_, iostat=ios, action='read')
      if (ios /= 0) stop "Error opening direction.txt"

      call read_win_size(in2, win_size)
      call read_text_list(in1, text_list)
      call read_dir_list(in2, dir_list)

      close (in1)
      close (in2)
   end subroutine read_all_data

   !считываем размер окна 
   subroutine read_win_size(in_unit, win_size)
      integer,      intent(in)  :: in_unit
      integer(I_),  intent(out) :: win_size
      integer :: ios

      read(in_unit, *, iostat=ios) win_size
      call Handle_IO_status(ios, "reading window size")
   end subroutine read_win_size

   !рекурсивно считываем строки текста из файла в связанный список
   recursive subroutine read_text_list(in_unit, head)
      integer,                  intent(in)  :: in_unit
      type(text_node), allocatable          :: head
      character(1024, CH_) :: buffer
      integer :: ios

      read(in_unit, '(a)', iostat=ios) buffer
      if (ios == 0) then
         allocate(head)
         head%line = buffer
         call read_text_list(in_unit, head%next)
      end if
   end subroutine read_text_list

   !рекурсивно считываем команды направления из файла в связанный список
   recursive subroutine read_dir_list(in_unit, head)
      integer,                 intent(in)  :: in_unit
      type(dir_node), allocatable          :: head
      character(1, CH_) :: cmd
      integer :: ios

      read(in_unit, '(a)', iostat=ios) cmd
      if (ios == 0) then
         allocate(head)
         head%dir = cmd
         call read_dir_list(in_unit, head%next)
      end if
   end subroutine read_dir_list

   !вывод всего
   subroutine write_full_output(fileout, text_list, dir_list, win_size, actions)
      character(*),                      intent(in) :: fileout
      type(text_node), allocatable,      intent(in) :: text_list
      type(dir_node),  allocatable,      intent(in) :: dir_list
      integer(I_),                       intent(in) :: win_size
      character(:, CH_), allocatable,    intent(in) :: actions(:)
      integer :: out_unit, ios, i

      open (newunit=out_unit, file=fileout, iostat=ios, action='write')
      if (ios /= 0) stop "Error opening result.txt"

      write(out_unit, '(a)') 'Исходный файл:'
      if (allocated(text_list)) then
         call write_text_list(out_unit, text_list)
      else
         write(out_unit, '(a)') '(пусто)'
      end if

      write(out_unit, '(a)') 'Размер листа:'
      write(out_unit, '(i0)') win_size

      write(out_unit, '(a)') 'Команды:'
      if (allocated(dir_list)) then
         call write_dir_list(out_unit, dir_list)
      else
         write(out_unit, '(a)') '(нет команд)'
      end if

      write(out_unit, '(a)') 'Результат пролистывания:'
      if (allocated(actions)) then
         do i = 1, size(actions)
            write(out_unit, '(a)') trim(actions(i))
         end do
      else
         write(out_unit, '(a)') '(нет данных)'
      end if

      close (out_unit)
   end subroutine write_full_output
 
   !рекурсивно записываем команды направления в файл
   recursive subroutine write_dir_list(out_unit, head)
      integer,                     intent(in) :: out_unit
      type(dir_node), allocatable, intent(in) :: head

      if (allocated(head)) then
         write(out_unit, '(a)') head%dir
         call write_dir_list(out_unit, head%next)
      end if
   end subroutine write_dir_list

   !рекурсивно записываем строки текста в файл
   recursive subroutine write_text_list(out_unit, head)
      integer,                       intent(in) :: out_unit
      type(text_node), allocatable, intent(in) :: head

      if (allocated(head)) then
         write(out_unit, '(a)') trim(head%line)
         call write_text_list(out_unit, head%next)
      end if
   end subroutine write_text_list


   !===============ОБРАБОТКА===============

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
      integer(I_),       intent(in) :: pos, win_size, total
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
      integer(I_),                   intent(in) :: win_size, pos, total
      character(:, CH_), allocatable :: actions(:)
      character(:, CH_), allocatable :: tail_actions(:), window_lines(:)
      integer(I_) :: new_pos, end_pos, n_lines, n, i

      !вычисляем последнюю строку текущего окна
      end_pos = min(pos + win_size - 1, total)
      n_lines = end_pos - pos + 1

      if (.not. allocated(dirs)) then
         allocate(character(100, CH_) :: actions(n_lines))
         do i = 1, n_lines
            actions(i) = get_line(text, pos + i - 1)
         end do
      else
         new_pos = next_pos(pos, dirs%dir, win_size, total)
         
         tail_actions = paginate(text, dirs%next, win_size, new_pos, total)
         
         allocate(character(100, CH_) :: window_lines(n_lines))
         do i = 1, n_lines
            window_lines(i) = get_line(text, pos + i - 1)
         end do
         
         n = n_lines + 1 + size(tail_actions)
         allocate(character(100, CH_) :: actions(n))
         
         actions(1:n_lines) = window_lines
         actions(n_lines + 1) = dirs%dir
         actions(n_lines + 2:) = tail_actions
      end if
   end function paginate

end module text_processing
