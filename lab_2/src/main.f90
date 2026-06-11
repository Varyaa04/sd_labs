program main
   use environment
   use text_processing

   implicit none

   character(*), parameter :: File1   = "../data/text.txt"
   character(*), parameter :: File2   = "../data/direction.txt"
   character(*), parameter :: FileOut = "output.txt"  

   type(text_processor) :: processor

   call processor%init(File1, File2)
   call processor%process()
   call processor%write(FileOut)

end program main