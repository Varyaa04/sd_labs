program prefix_to_postfix
   use Environment
   use ExpressionConverter

   implicit none
   type(ExpressionConverter) :: converter
   
   call converter%read_expression("../data/expression.txt")
   call converter%validate_and_convert()
   call converter%output_result("output.txt")

end program prefix_to_postfix