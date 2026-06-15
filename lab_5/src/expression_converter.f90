module ExpressionConverter
   use Environment

   implicit none

   ! базовый абстрактный тип для узла
   type, public, abstract :: base_node
   contains
      procedure(print_interface), deferred, pass :: print
   end type base_node

   abstract interface
      subroutine print_interface(this, unit)
         import base_node
         class(base_node), intent(in) :: this
         integer, intent(in) :: unit
      end subroutine print_interface
   end interface

   ! рекурсивный производный тип узла выражения
   type, extends(base_node), public :: expr_node
      character(1) :: value
      type(expr_node), pointer :: left => null()
      type(expr_node), pointer :: right => null()
   contains
      procedure, pass :: print => print_node
   end type expr_node

   ! инкапсулирующий тип для конвертера выражений
   type, public :: ExpressionConverter
   private
      character(:), allocatable :: prefix_expr
      character(:), allocatable :: postfix_expr
      integer :: pos
      logical :: is_valid
      character(100) :: error_msg
      type(expr_node), pointer :: root => null()  ! сохраняем корень для деструктора
   contains
      procedure, public :: read_expression
      procedure, public :: validate_and_convert
      procedure, public :: output_result
      procedure, private :: parse_expression
      procedure, private :: to_postfix
      procedure, private :: check_operand
      procedure, private :: check_operator
      procedure, private :: skip_spaces
      procedure, private :: clear_tree
      final :: converter_destructor
   end type ExpressionConverter

   ! символы операций
   character(1), parameter :: OPERATORS(4) = ['+', '-', '*', '/']
   character(1), parameter :: OPEN_PAREN = '('
   character(1), parameter :: CLOSE_PAREN = ')'

contains

   ! деструктор - освобождает память дерева
   subroutine converter_destructor(this)
      type(ExpressionConverter), intent(inout) :: this
      if (associated(this%root)) then
         call this%clear_tree(this%root)
      end if
   end subroutine converter_destructor

   ! реализация полиморфного метода print
   subroutine print_node(this, unit)
      class(expr_node), intent(in) :: this
      integer, intent(in) :: unit
      write(unit, '(a)', advance='no') this%value
   end subroutine print_node

   ! пропуск пробелов
   subroutine skip_spaces(this)
      class(ExpressionConverter), intent(inout) :: this
      do while (this%pos <= len(this%prefix_expr) .and. &
                this%prefix_expr(this%pos:this%pos) == ' ')
         this%pos = this%pos + 1
      end do
   end subroutine skip_spaces

   ! чтение выражения из файла
   subroutine read_expression(this, input_file)
      class(ExpressionConverter), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(256) :: buffer

      ! очищаем предыдущее состояние
      if (associated(this%root)) then
         call this%clear_tree(this%root)
         this%root => null()
      end if
      this%is_valid = .true.
      this%error_msg = ""
      this%postfix_expr = ""

      open (file=input_file, newunit=In, status='old', action='read', iostat=IO)
      if (IO /= 0) then
         this%is_valid = .false.
         this%error_msg = "Ошибка открытия файла"
         return
      end if
      
      read (In, '(a)', iostat=IO) buffer
      close (In)

      if (IO == 0) then
         this%prefix_expr = trim(buffer)
         if (len_trim(this%prefix_expr) == 0) then
            this%is_valid = .false.
            this%error_msg = "Пустое выражение"
         end if
         this%pos = 1
      else
         this%is_valid = .false.
         this%error_msg = "Ошибка чтения файла"
      end if
   end subroutine read_expression

   ! проверка корректности и преобразование
   subroutine validate_and_convert(this)
      class(ExpressionConverter), intent(inout) :: this
      
      if (.not. this%is_valid) return
      
      this%pos = 1
      this%postfix_expr = ""
      
      ! очищаем предыдущее дерево
      if (associated(this%root)) then
         call this%clear_tree(this%root)
         this%root => null()
      end if
      
      ! парсим выражение
      call this%parse_expression(this%root)
      
      if (this%is_valid) then
         ! проверяем, что вся строка разобрана
         call this%skip_spaces()
         if (this%pos <= len(this%prefix_expr)) then
            this%is_valid = .false.
            this%error_msg = "Лишние символы после выражения"
            return
         end if
         
         ! преобразуем в постфиксную форму
         call this%to_postfix(this%root)
      end if
      
   end subroutine validate_and_convert

   ! рекурсивный парсинг выражения
   recursive subroutine parse_expression(this, node)
      class(ExpressionConverter), intent(inout) :: this
      type(expr_node), pointer, intent(out) :: node
      character(1) :: ch
      
      node => null()
      
      if (.not. this%is_valid) return
      
      call this%skip_spaces()
      if (this%pos > len(this%prefix_expr)) return
      
      ch = this%prefix_expr(this%pos:this%pos)
      
      ! если это операнд
      if (this%check_operand(ch)) then
         allocate(node)
         node%value = ch
         node%left => null()
         node%right => null()
         this%pos = this%pos + 1
         
      ! если это оператор
      else if (this%check_operator(ch)) then
         allocate(node)
         node%value = ch
         this%pos = this%pos + 1
         
         ! парсим левый операнд
         call this%parse_expression(node%left)
         if (.not. this%is_valid) return
         
         ! парсим правый операнд
         call this%parse_expression(node%right)
         if (.not. this%is_valid) return
         
         ! проверяем, что оба операнда существуют
         if (.not. associated(node%left) .or. .not. associated(node%right)) then
            this%is_valid = .false.
            this%error_msg = "Недостаточно операндов для оператора '" // ch // "'"
            return
         end if
         
      else
         this%is_valid = .false.
         this%error_msg = "Некорректный символ: '" // ch // "'"
         return
      end if
      
   end subroutine parse_expression

   ! проверка, является ли символ операндом 
   function check_operand(this, ch) result(res)
      class(ExpressionConverter), intent(in) :: this
      character(1), intent(in) :: ch
      logical :: res
      
      res = (ch >= 'A' .and. ch <= 'Z') .or. (ch >= 'a' .and. ch <= 'z')
   end function check_operand

   ! проверка, является ли символ оператором
   function check_operator(this, ch) result(res)
      class(ExpressionConverter), intent(in) :: this
      character(1), intent(in) :: ch
      logical :: res
      integer :: i
      
      res = .false.
      do i = 1, size(OPERATORS)
         if (ch == OPERATORS(i)) then
            res = .true.
            exit
         end if
      end do
   end function check_operator

   ! рекурсивное преобразование в постфиксную форму
   recursive subroutine to_postfix(this, node)
      class(ExpressionConverter), intent(inout) :: this
      type(expr_node), pointer, intent(in) :: node
      
      if (.not. associated(node)) return
      
      ! постфиксная форма: левое поддерево, правое поддерево, корень
      call this%to_postfix(node%left)
      call this%to_postfix(node%right)
      
      ! добавляем текущий узел
      if (this%postfix_expr == "") then
         this%postfix_expr = node%value
      else
         this%postfix_expr = this%postfix_expr // " " // node%value
      end if
      
   end subroutine to_postfix

   ! рекурсивная очистка дерева
   recursive subroutine clear_tree(this, node)
      class(ExpressionConverter), intent(inout) :: this
      type(expr_node), pointer, intent(inout) :: node
      
      if (associated(node)) then
         call this%clear_tree(node%left)
         call this%clear_tree(node%right)
         deallocate(node)
         node => null()
      end if
   end subroutine clear_tree

   ! вывод результата
   subroutine output_result(this, output_file)
      class(ExpressionConverter), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out
      
      open (file=output_file, newunit=Out, action='write', iostat=Out)
      if (Out /= 0) return
      
      write(Out, '(a)') "Преобразование префиксной формы в постфиксную"
      write(Out, '(a)') ""
      write(Out, '(a)') "Исходное выражение (префиксная форма):"
      write(Out, '(2x, a)') trim(this%prefix_expr)
      write(Out, '(a)') ""
      
      if (this%is_valid) then
         write(Out, '(a)') "Результат (постфиксная форма):"
         write(Out, '(2x, a)') trim(this%postfix_expr)
         write(Out, '(a)') ""
         write(Out, '(a)') "Проверка: выражение корректно"
      else
         write(Out, '(a)') "ОШИБКА:"
         write(Out, '(2x, a)') trim(this%error_msg)
      end if
      
      close(Out)
   end subroutine output_result

end module ExpressionConverter
