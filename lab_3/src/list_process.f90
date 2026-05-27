module List_Process
   use Environment
   use List_IO

   implicit none

contains

   !хвостовая рекурсия для удаления всех вхождений
   recursive subroutine delete_all(list, name)
      type(StringList), intent(inout) :: list
      character(*), intent(in) :: name
      type(node), pointer :: head

      head => list%get_head()
      
      call delete_all_recursive(head, name)
      
      call list%set_head(head)
   end subroutine delete_all

   !рекурсивная процедура удаления
   recursive subroutine delete_all_recursive(head, name)
      type(node), pointer, intent(inout) :: head
      character(*), intent(in) :: name
      type(node), pointer :: tmp

      if (.not. associated(head)) return

      if (head%value == name) then
         tmp => head
         head => head%next
         deallocate(tmp)
         !хвостовая рекурсия 
         call delete_all_recursive(head, name)
      else
         call delete_all_recursive(head%next, name)
      end if
   end subroutine delete_all_recursive

end module List_Process