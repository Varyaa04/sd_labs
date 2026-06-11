module ExpressionConverter
   use Environment

   implicit none

   ! базовый абстрактный тип для узла (полиморфизм)
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
   contains
      procedure, public :: read_expression
      procedure, public :: validate_and_convert
      procedure, public :: output_result
      procedure, private :: parse_expression
      procedure, private :: to_postfix
      procedure, private :: check_operand
      procedure, private :: check_operator
      procedure, private :: clear_tree
      final :: converter_destructor
   end type ExpressionConverter

   ! символы операций
   character(1), parameter :: OPERATORS(4) = ['+', '-', '*', '/']
   character(1), parameter :: OPEN_PAREN = '('
   character(1), parameter :: CLOSE_PAREN = ')'

contains

   ! завершаемая функция - автоматическое удаление дерева
   subroutine converter_destructor(this)
      type(ExpressionConverter), intent(inout) :: this
      ! дерево будет очищено при необходимости
   end subroutine converter_destructor

   ! реализация полиморфного метода print
   subroutine print_node(this, unit)
      class(expr_node), intent(in) :: this
      integer, intent(in) :: unit
      write(unit, '(a)', advance='no') this%value
   end subroutine print_node

   ! чтение выражения из файла
   subroutine read_expression(this, input_file)
      class(ExpressionConverter), intent(inout) :: this
      character(*), intent(in) :: input_file
      integer :: In, IO
      character(256) :: buffer

      open (file=input_file, newunit=In, status='old', action='read')
      read (In, '(a)', iostat=IO) buffer
      close (In)

      if (IO == 0) then
         this%prefix_expr = trim(buffer)
         this%pos = 1
         this%is_valid = .true.
         this%error_msg = ""
      else
         this%is_valid = .false.
         this%error_msg = "Ошибка чтения файла"
      end if
   end subroutine read_expression

   ! проверка корректности и преобразование
   subroutine validate_and_convert(this)
      class(ExpressionConverter), intent(inout) :: this
      type(expr_node), pointer :: root
      
      if (.not. this%is_valid) return
      
      this%pos = 1
      this%postfix_expr = ""
      
      ! парсим выражение
      call this%parse_expression(root)
      
      if (this%is_valid) then
         ! преобразуем в постфиксную форму
         call this%to_postfix(root)
         call this%clear_tree(root)
      end if
      
   end subroutine validate_and_convert

   ! рекурсивный парсинг выражения
   recursive subroutine parse_expression(this, node)
      class(ExpressionConverter), intent(inout) :: this
      type(expr_node), pointer, intent(out) :: node
      character(1) :: ch
      
      node => null()
      
      if (.not. this%is_valid) return
      if (this%pos > len(this%prefix_expr)) return
      
      ch = this%prefix_expr(this%pos:this%pos)
      
      ! проверка на пропуск пробелов
      if (ch == ' ') then
         this%pos = this%pos + 1
         call this%parse_expression(node)
         return
      end if
      
      ! если это операнд (буква)
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
            this%error_msg = "Недостаточно операндов для оператора"
            return
         end if
         
      else
         this%is_valid = .false.
         this%error_msg = "Некорректный символ: " // ch
         return
      end if
      
   end subroutine parse_expression

   ! проверка, является ли символ операндом (латинская буква)
   function check_operand(this, ch) result(res)
      class(ExpressionConverter), intent(in) :: this
      character(1), intent(in) :: ch
      logical :: res
      
      res = (ch >= 'A' .and. ch <= 'Z')
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
            return
         end if
      end do
   end function check_operator

   ! рекурсивное преобразование в постфиксную форму
   recursive subroutine to_postfix(this, node)
      class(ExpressionConverter), intent(inout) :: this
      type(expr_node), pointer, intent(in) :: node
      
      if (.not. associated(node)) return
      
      ! постфиксная форма: левое поддерево, правое поддерево, корень
      if (associated(node%left)) then
         call this%to_postfix(node%left)
      end if
      
      if (associated(node%right)) then
         call this%to_postfix(node%right)
      end if
      
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
         if (associated(node%left)) then
            call this%clear_tree(node%left)
         end if
         if (associated(node%right)) then
            call this%clear_tree(node%right)
         end if
         deallocate(node)
         node => null()
      end if
   end subroutine clear_tree

   ! вывод результата
   subroutine output_result(this, output_file)
      class(ExpressionConverter), intent(in) :: this
      character(*), intent(in) :: output_file
      integer :: Out
      
      open (file=output_file, newunit=Out, action='write')
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