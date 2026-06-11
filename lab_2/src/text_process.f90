module text_processing
   use environment
   implicit none
   private

   !узел списка текста (двусвязный для движения назад)
   type, public :: text_node
      character(:, CH_), allocatable :: line
      type(text_node),   pointer     :: next => null()
      type(text_node),   pointer     :: prev => null()
   end type text_node

   !узел списка команд
   type, public :: dir_node
      character(1, CH_)            :: dir
      type(dir_node), pointer      :: next => null()
   end type dir_node

   !пагинатор с состоянием
   type, public :: paginator
      type(text_node), pointer :: current_pos => null()
      integer(I_) :: win_size = 0
      integer(I_) :: total_len = 0
   contains
      procedure :: init => paginator_init
      procedure :: move_forward => paginator_move_forward
      procedure :: move_back => paginator_move_back
      procedure :: get_window_copy => paginator_get_window_copy
      procedure :: set_position => paginator_set_position
   end type paginator

   character(1, CH_), parameter :: CHAR_F = 'f'
   character(1, CH_), parameter :: CHAR_F_BIG = 'F'
   character(1, CH_), parameter :: CHAR_B = 'b'
   character(1, CH_), parameter :: CHAR_B_BIG = 'B'

   !главный тип с инкапсуляцией
   type, public :: text_processor
      type(text_node), pointer :: text_list => null()
      type(dir_node),  pointer :: dir_list => null()
      character(:, CH_), allocatable :: actions(:)
      integer(I_) :: win_size = 0
      integer(I_) :: total_len = 0
   contains
      procedure :: init   => processor_init
      procedure :: process => processor_process
      procedure :: write  => processor_write
   end type text_processor

contains

   !===============ВВОД/ВЫВОД===============

   subroutine read_all_data(file1, file2, text_list, dir_list, win_size)
      character(*),                    intent(in)  :: file1, file2
      type(text_node), pointer,        intent(out) :: text_list
      type(dir_node),  pointer,        intent(out) :: dir_list
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

   subroutine processor_init(this, file1, file2)
      class(text_processor), intent(inout) :: this
      character(*), intent(in) :: file1, file2
      
      call read_all_data(file1, file2, this%text_list, this%dir_list, this%win_size)
   end subroutine processor_init

   subroutine processor_process(this)
      class(text_processor), intent(inout) :: this
      
      if (associated(this%text_list) .and. associated(this%dir_list)) then
         this%total_len = text_size(this%text_list)
         this%actions = paginate(this%text_list, this%dir_list, &
                                 this%win_size, 1_I_, this%total_len)
      end if
   end subroutine processor_process

   subroutine processor_write(this, fileout)
      class(text_processor), intent(in) :: this
      character(*), intent(in) :: fileout
      
      call write_full_output(fileout, this%text_list, this%dir_list, &
                             this%win_size, this%actions)
   end subroutine processor_write

   subroutine read_win_size(in_unit, win_size)
      integer,      intent(in)  :: in_unit
      integer(I_),  intent(out) :: win_size
      integer :: ios
      read(in_unit, *, iostat=ios) win_size
      call Handle_IO_status(ios, "reading window size")
   end subroutine read_win_size

   recursive subroutine read_text_list(in_unit, head)
      integer,                  intent(in)  :: in_unit
      type(text_node), pointer, intent(out) :: head
      character(1024, CH_) :: buffer
      integer :: ios
      type(text_node), pointer :: new_node

      read(in_unit, '(a)', iostat=ios) buffer
      if (ios == 0) then
         allocate(new_node)
         new_node%line = trim(buffer)
         new_node%next => null()
         new_node%prev => null()
         head => new_node
         call read_text_list(in_unit, head%next)
      else
         head => null()
      end if
   end subroutine read_text_list

   recursive subroutine read_dir_list(in_unit, head)
      integer,                 intent(in)  :: in_unit
      type(dir_node), pointer, intent(out) :: head
      character(1, CH_) :: cmd
      integer :: ios
      type(dir_node), pointer :: new_node

      read(in_unit, '(a)', iostat=ios) cmd
      if (ios == 0) then
         allocate(new_node)
         new_node%dir = cmd
         new_node%next => null()
         head => new_node
         call read_dir_list(in_unit, head%next)
      else
         head => null()
      end if
   end subroutine read_dir_list

   subroutine write_full_output(fileout, text_list, dir_list, win_size, actions)
      character(*),                      intent(in) :: fileout
      type(text_node), pointer,          intent(in) :: text_list
      type(dir_node),  pointer,          intent(in) :: dir_list
      integer(I_),                       intent(in) :: win_size
      character(:, CH_), allocatable,    intent(in) :: actions(:)
      integer :: out_unit, ios, i

      open (newunit=out_unit, file=fileout, iostat=ios, action='write')
      if (ios /= 0) stop "Error opening result.txt"

      write(out_unit, '(a)') 'Исходный файл:'
      if (associated(text_list)) then
         call write_text_list(out_unit, text_list)
      else
         write(out_unit, '(a)') '(пусто)'
      end if

      write(out_unit, '(a)') 'Размер окна:'
      write(out_unit, '(i0)') win_size

      write(out_unit, '(a)') 'Команды:'
      if (associated(dir_list)) then
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
 
   recursive subroutine write_dir_list(out_unit, head)
      integer,                    intent(in) :: out_unit
      type(dir_node), pointer,    intent(in) :: head
      if (associated(head)) then
         write(out_unit, '(a)') head%dir
         call write_dir_list(out_unit, head%next)
      end if
   end subroutine write_dir_list

   recursive subroutine write_text_list(out_unit, head)
      integer,                      intent(in) :: out_unit
      type(text_node), pointer,     intent(in) :: head
      if (associated(head)) then
         write(out_unit, '(a)') trim(head%line)
         call write_text_list(out_unit, head%next)
      end if
   end subroutine write_text_list

   !===============ПАГИНАТОР С СОСТОЯНИЕМ===============

   subroutine paginator_init(this, head, win_size, total_len)
      class(paginator), intent(inout) :: this
      type(text_node), target, intent(in) :: head
      integer(I_), intent(in) :: win_size, total_len
      
      this%current_pos => head
      this%win_size = win_size
      this%total_len = total_len
   end subroutine paginator_init

   subroutine paginator_set_position(this, pos)
      class(paginator), intent(inout) :: this
      integer(I_), intent(in) :: pos
      integer(I_) :: i
      
      ! Находим начало списка
      do while (associated(this%current_pos%prev))
         this%current_pos => this%current_pos%prev
      end do
      
      ! Перемещаемся на позицию pos
      do i = 2, pos
         if (associated(this%current_pos%next)) then
            this%current_pos => this%current_pos%next
         end if
      end do
   end subroutine paginator_set_position

   subroutine paginator_move_forward(this)
      class(paginator), intent(inout) :: this
      integer(I_) :: i
      
      do i = 1, this%win_size
         if (associated(this%current_pos%next)) then
            this%current_pos => this%current_pos%next
         else
            exit
         end if
      end do
   end subroutine paginator_move_forward

   subroutine paginator_move_back(this)
      class(paginator), intent(inout) :: this
      integer(I_) :: i
      
      do i = 1, this%win_size
         if (associated(this%current_pos%prev)) then
            this%current_pos => this%current_pos%prev
         else
            exit
         end if
      end do
   end subroutine paginator_move_back

   function paginator_get_window_copy(this) result(window_copy)
      class(paginator), intent(in) :: this
      type(text_node), pointer :: window_copy
      
      window_copy => null()
      call copy_window_recursive(this%current_pos, this%win_size, 1, window_copy)
      
   end function paginator_get_window_copy

   recursive subroutine copy_window_recursive(src, win_size, depth, dest)
      type(text_node), pointer, intent(in) :: src
      integer(I_), intent(in) :: win_size, depth
      type(text_node), pointer, intent(out) :: dest
      
      if (.not. associated(src)) return
      if (depth > win_size) return
      
      allocate(dest)
      dest%line = src%line
      dest%prev => null()
      dest%next => null()
      
      if (depth < win_size .and. associated(src%next)) then
         call copy_window_recursive(src%next, win_size, depth + 1, dest%next)
      end if
      
   end subroutine copy_window_recursive

   function window_copy_to_array(window) result(lines)
      type(text_node), pointer, intent(in) :: window
      character(:, CH_), allocatable :: lines(:)
      integer(I_) :: count
      
      if (.not. associated(window)) then
         allocate(character(0, CH_) :: lines(0))
         return
      end if
      
      count = count_nodes(window)
      allocate(character(100, CH_) :: lines(count))
      
      call copy_to_array_recursive(window, lines, 1)
      
   end function window_copy_to_array
   
   recursive integer(I_) function count_nodes(head) result(n)
      type(text_node), pointer, intent(in) :: head
      if (associated(head)) then
         n = 1 + count_nodes(head%next)
      else
         n = 0
      end if
   end function count_nodes
   
   recursive subroutine copy_to_array_recursive(node, arr, idx)
      type(text_node), pointer, intent(in) :: node
      character(:, CH_), allocatable, intent(inout) :: arr(:)
      integer(I_), intent(in) :: idx
      
      if (associated(node)) then
         arr(idx) = node%line
         if (associated(node%next)) then
            call copy_to_array_recursive(node%next, arr, idx + 1)
         end if
      end if
   end subroutine copy_to_array_recursive

   !===============ОСНОВНАЯ ФУНКЦИЯ ПАГИНАЦИИ===============

   recursive function paginate(text, dirs, win_size, start_pos, total) result(actions)
      type(text_node), pointer, intent(in) :: text
      type(dir_node),  pointer, intent(in) :: dirs
      integer(I_),                   intent(in) :: win_size, start_pos, total
      character(:, CH_), allocatable :: actions(:)
      
      type(paginator) :: pager
      type(text_node), pointer :: window_copy
      character(:, CH_), allocatable :: current_window(:), tail_actions(:)
      integer(I_) :: n_current, n_total, i, new_pos
      
      if (.not. associated(text) .or. .not. associated(dirs)) then
         call pager%init(text, win_size, total)
         call pager%set_position(start_pos)
         window_copy => pager%get_window_copy()
         actions = window_copy_to_array(window_copy)
         return
      end if
      
      call pager%init(text, win_size, total)
      call pager%set_position(start_pos)
      
      window_copy => pager%get_window_copy()
      current_window = window_copy_to_array(window_copy)
      n_current = size(current_window)
      
      if (dirs%dir == CHAR_F .or. dirs%dir == CHAR_F_BIG) then
         call pager%move_forward()
      else if (dirs%dir == CHAR_B .or. dirs%dir == CHAR_B_BIG) then
         call pager%move_back()
      end if
      
      new_pos = get_current_position(pager)
      tail_actions = paginate(text, dirs%next, win_size, new_pos, total)
      
      n_total = n_current + 1 + size(tail_actions)
      allocate(character(100, CH_) :: actions(n_total))
      
      do i = 1, n_current
         actions(i) = current_window(i)
      end do
      
      actions(n_current + 1) = dirs%dir
      
      do i = 1, size(tail_actions)
         actions(n_current + 1 + i) = tail_actions(i)
      end do
      
   end function paginate
   
   function get_current_position(pager) result(pos)
      class(paginator), intent(in) :: pager
      integer(I_) :: pos
      type(text_node), pointer :: temp
      
      temp => pager%current_pos
      
      ! Находим начало списка
      do while (associated(temp%prev))
         temp => temp%prev
      end do
      
      pos = 1
      do while (associated(temp) .and. .not. associated(temp, pager%current_pos))
         temp => temp%next
         pos = pos + 1
      end do
      
   end function get_current_position

   recursive integer(I_) function text_size(head) result(n)
      type(text_node), pointer, intent(in) :: head
      if (associated(head)) then
         n = 1 + text_size(head%next)
      else
         n = 0
      end if
   end function text_size

end module text_processing