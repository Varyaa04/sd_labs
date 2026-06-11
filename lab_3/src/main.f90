program reference_lab_list
   use Environment
   use List_Process

   implicit none
   type(StringList) :: S

   call S%read_from_file("../data/input.txt")
   call S%output("output.txt", "Исходный список:", "rewind")
   call S%delete_last_line_from_file("../data/input.txt")
   call S%delete_all()
   call S%output("output.txt", "Список после удаления:", "append")

end program reference_lab_list