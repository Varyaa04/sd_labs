program main
   use environment
   use text_processing

   implicit none
!инкапс
   character(*), parameter :: File1   = "../data/text.txt"
   character(*), parameter :: File2   = "../data/direction.txt"
   character(*), parameter :: FileOut = "output.txt"  

   type(text_node), allocatable :: text_list
   type(dir_node),  allocatable :: dir_list
   character(:, CH_), allocatable :: actions(:)
   integer(I_) :: win_size, total_len

   call read_all_data(File1, File2, text_list, dir_list, win_size)
!хранить текущую позицию и выводит лист, делать копию текущего листа
   if (allocated(text_list) .and. allocated(dir_list)) then
      total_len = text_size(text_list)
      actions = paginate(text_list, dir_list, win_size, 1, total_len)
   end if

   call write_full_output(FileOut, text_list, dir_list, win_size, actions)

end program main
