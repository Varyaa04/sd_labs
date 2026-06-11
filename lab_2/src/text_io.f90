! module text_io
!    use environment
!    use text_processing

!    implicit none
!    private

!    public :: read_all_data, write_full_output

! contains
! !читаем все входные данные из двух файлов и возвращает списки и размер окна
!    subroutine read_all_data(file1, file2, text_list, dir_list, win_size)
!       character(*),                    intent(in)  :: file1, file2
!       type(text_node), allocatable,    intent(out) :: text_list
!       type(dir_node),  allocatable,    intent(out) :: dir_list
!       integer(I_),                     intent(out) :: win_size
!       integer :: in1, in2, ios

!       open (newunit=in1, file=file1, encoding=E_, iostat=ios, action='read')
!       if (ios /= 0) stop "Error opening text.txt"
      
!       open (newunit=in2, file=file2, encoding=E_, iostat=ios, action='read')
!       if (ios /= 0) stop "Error opening direction.txt"

!       call read_win_size(in2, win_size)
!       call read_text_list(in1, text_list)
!       call read_dir_list(in2, dir_list)

!       close (in1)
!       close (in2)
!    end subroutine read_all_data

!    !считываем размер окна из открытого файла
!    subroutine read_win_size(in_unit, win_size)
!       integer,      intent(in)  :: in_unit
!       integer(I_),  intent(out) :: win_size
!       integer :: ios

!       read(in_unit, *, iostat=ios) win_size
!       call Handle_IO_status(ios, "reading window size")
!    end subroutine read_win_size

!    !рекурсивно считываем строки текста из файла в связанный список
!    recursive subroutine read_text_list(in_unit, head)
!       integer,                  intent(in)  :: in_unit
!       type(text_node), allocatable          :: head
!       character(1024, CH_) :: buffer
!       integer :: ios

!       read(in_unit, '(a)', iostat=ios) buffer
!       if (ios == 0) then
!          allocate(head)
!          head%line = buffer
!          call read_text_list(in_unit, head%next)
!       end if
!    end subroutine read_text_list

!    !рекурсивно считываем команды направления из файла в связанный список
!    recursive subroutine read_dir_list(in_unit, head)
!       integer,                 intent(in)  :: in_unit
!       type(dir_node), allocatable          :: head
!       character(1, CH_) :: cmd
!       integer :: ios

!       read(in_unit, '(a)', iostat=ios) cmd
!       if (ios == 0) then
!          allocate(head)
!          head%dir = cmd
!          call read_dir_list(in_unit, head%next)
!       end if
!    end subroutine read_dir_list

!    !вывод всего
!    subroutine write_full_output(fileout, text_list, dir_list, win_size, actions)
!       character(*),                      intent(in) :: fileout
!       type(text_node), allocatable,      intent(in) :: text_list
!       type(dir_node),  allocatable,      intent(in) :: dir_list
!       integer(I_),                       intent(in) :: win_size
!       character(:, CH_), allocatable,    intent(in) :: actions(:)
!       integer :: out_unit, ios, i

!       open (newunit=out_unit, file=fileout, iostat=ios, action='write')
!       if (ios /= 0) stop "Error opening result.txt"

!       write(out_unit, '(a)') 'Исходный файл:'
!       if (allocated(text_list)) then
!          call write_text_list(out_unit, text_list)
!       else
!          write(out_unit, '(a)') '(пусто)'
!       end if


!       write(out_unit, '(a)') 'Размер листа:'
!       write(out_unit, '(i0)') win_size

!       write(out_unit, '(a)') 'Команды:'
!       if (allocated(dir_list)) then
!          call write_dir_list(out_unit, dir_list)
!       else
!          write(out_unit, '(a)') '(нет команд)'
!       end if

!       write(out_unit, '(a)') 'Результат пролистывания:'
!       if (allocated(actions)) then
!          write(out_unit, '(a)') (actions(i), i = 1, size(actions))
!       else
!          write(out_unit, '(a)') '(нет данных)'
!       end if

!       close (out_unit)
!    end subroutine write_full_output
 
!    !рекурсивно записываем команды направления в файл
!    recursive subroutine write_dir_list(out_unit, head)
!       integer,                     intent(in) :: out_unit
!       type(dir_node), allocatable, intent(in) :: head

!       if (allocated(head)) then
!          write(out_unit, '(a)') head%dir
!          call write_dir_list(out_unit, head%next)
!       end if
!    end subroutine write_dir_list

!    !рекурсивно записываем строки текста в файл
!    recursive subroutine write_text_list(out_unit, head)
!       integer,                       intent(in) :: out_unit
!       type(text_node), allocatable, intent(in) :: head

!       if (allocated(head)) then
!          write(out_unit, '(a)') head%line
!          call write_text_list(out_unit, head%next)
!       end if
!    end subroutine write_text_list

! end module text_io