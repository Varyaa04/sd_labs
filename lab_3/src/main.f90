program reference_lab_list
   use Environment
   use List_IO
   use List_Process

   implicit none
   character(:), allocatable :: input_file, output_file
   type(StringList) :: S
   character(100) :: last_line

   input_file  = "../data/input.txt"
   output_file = "output.txt"
   
   print *, "Обработка списка фамилий"
   print *

   !формирование списка из файла
   call S%read_from_file(input_file)

   call S%output(output_file, "Исходный список:", "rewind")

   !чтение последней строки файла
   call read_last_line_from_file(input_file, last_line)
   print *, "Удаляемая фамилия: '", last_line, "'"
   print *

   !удаление всех элементов, совпадающих с last_line
   call delete_all(S, last_line)

   call S%output(output_file, "Список после удаления:", "append")

   call S%destroy()

end program reference_lab_list