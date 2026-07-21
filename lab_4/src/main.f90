program main
   use Environment
   use CircularList

   implicit none
   type(CircularList) :: game

   call game%read_names("../data/names.txt")
   call game%play_game("Анна", 4)
   call game%output_result("output.txt")

end program main