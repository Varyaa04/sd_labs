program counting_game
   use Environment
   use CircularList

   implicit none
   type(CircularList) :: game
   
   call game%read_names("../data/names.txt")
   call game%play_game("Анна", 3)
   call game%output_result("output.txt")

end program counting_game