module Text_Process
   use Environment
   use Text_IO

   implicit none

contains

   ! Функция для вычисления длины списка.
   recursive function Get_List_Length(head) result(len)
      type(TextLine), pointer, intent(in) :: head
      integer                             :: len

      if (.not. associated(head)) then
         len = 0
      else
         len = 1 + Get_List_Length(head%next)
      end if
   end function Get_List_Length

   ! Функция получения n-го узла (1-based).
   recursive function Get_Nth_Node(head, n) result(node)
      type(TextLine), pointer, intent(in) :: head
      integer, intent(in)                 :: n
      type(TextLine), pointer             :: node

      if (n <= 0 .or. .not. associated(head)) then
         node => null()
      else if (n == 1) then
         node => head
      else
         node => Get_Nth_Node(head%next, n-1)
      end if
   end function Get_Nth_Node

   ! Функция для создания среза списка (новый список!).
   recursive function Make_Slice(head, count) result(slice)
      type(TextLine), pointer, intent(in) :: head
      integer, intent(in)                 :: count
      type(TextLine), pointer             :: slice

      if (count <= 0 .or. .not. associated(head)) then
         slice => null()
      else
         allocate(slice)
         slice%String = head%String
         slice%Next => Make_Slice(head%next, count-1)
      end if
   end function Make_Slice

   ! Преобразование списка строк в одну строку с пробелами.
   recursive function List_To_String(head) result(res_str)
      type(TextLine), pointer, intent(in) :: head
      character(:, CH_), allocatable      :: res_str
      character(:, CH_), allocatable      :: tail_str

      if (.not. associated(head)) then
         res_str = ""
      else
         tail_str = List_To_String(head%next)
         if (len(tail_str) > 0) then
            res_str = Trim(head%String) // CH__" " // tail_str
         else
            res_str = Trim(head%String)
         end if
      end if
   end function List_To_String

   ! Добавление в конец списка.
   recursive function Append_Result(accumulator, new_string) result(res)
      type(TextLine), pointer, intent(in) :: accumulator
      character(*, CH_), intent(in)       :: new_string   ! Используем ассумированную длину
      type(TextLine), pointer             :: res

      if (.not. associated(accumulator)) then
         allocate(res)
         res%String = new_string
         res%Next => null()
      else
         allocate(res)
         res%String = accumulator%String
         res%Next => Append_Result(accumulator%Next, new_string)
      end if
   end function Append_Result

   ! Разворот списка (для правильного порядка результатов).
   recursive function Reverse_List(head) result(reversed)
      type(TextLine), pointer, intent(in) :: head
      type(TextLine), pointer             :: reversed

      if (.not. associated(head)) then
         reversed => null()
      else
         reversed => Append_Result(Reverse_List(head%Next), head%String)
      end if
   end function Reverse_List

   ! Основная рекурсия для пролистывания.
   recursive function Paginate(head, commands, n, current_pos, accumulator) result(res)
      type(TextLine), pointer, intent(in)     :: head
      type(Command), pointer, intent(in)      :: commands
      integer, intent(in)                     :: n
      integer, intent(in)                     :: current_pos
      type(TextLine), pointer, intent(in)     :: accumulator
      type(TextLine), pointer                 :: res

      type(TextLine), pointer :: frame, new_accumulator
      integer                 :: new_pos, total_len
      character(:, CH_), allocatable :: frame_text
      character(1)            :: cmd_char

      if (.not. associated(commands)) then
         ! Базовый случай: разворачиваем аккумулятор для правильного порядка.
         res => Reverse_List(accumulator)
      else
         ! Вычисляем новую позицию на основе команды.
         cmd_char = commands%Cmd
         if (cmd_char == 'F' .or. cmd_char == 'f') then
            new_pos = current_pos + n
         else if (cmd_char == 'B' .or. cmd_char == 'b') then
            new_pos = max(1, current_pos - n)
         else
            new_pos = current_pos
         end if

         ! Получаем кадр, начиная с новой позиции.
         total_len = Get_List_Length(head)
         
         if (new_pos > total_len) then
            frame => null()
         else
            frame => Make_Slice(Get_Nth_Node(head, new_pos), n)
         end if

         ! Преобразуем кадр в строку для вывода.
         frame_text = List_To_String(frame)
         
         ! Добавляем в аккумулятор (накапливаем в обратном порядке).
         if (len(frame_text) > 0) then
            allocate(new_accumulator)
            new_accumulator%String = frame_text
            new_accumulator%Next => accumulator
         else
            new_accumulator => accumulator
         end if

         ! Рекурсивный вызов с новыми параметрами.
         res => Paginate(head, commands%Next, n, new_pos, new_accumulator)
      end if
   end function Paginate

end module Text_Process