program Lab_2
   use Environment
   use Text_Process
   use Text_IO

   implicit none
   character(:), allocatable :: TextFile, CommandFile, OutputFile

   type(TextLine), pointer   :: SourceText => Null()   ! Исходный текст.
   type(Command), pointer    :: Commands   => Null()   ! Список команд.
   type(TextLine), pointer   :: Result     => Null()   ! Результат пролистывания.
   integer                   :: N                       ! Размер листа.

   ! Имена файлов.
   TextFile    = "../data/text.txt"
   CommandFile = "../data/direction.txt"
   OutputFile  = "result.txt"

   ! Чтение исходного текста.
   SourceText => Read_Text(TextFile)
   if (.not. associated(SourceText)) then
      write(ERROR_UNIT, '(a)') "Error: Could not read source text file."
      stop
   end if

   ! Чтение размера листа N.
   N = Read_N(CommandFile)

   ! Чтение команд.
   Commands => Read_Commands(CommandFile)
   if (.not. associated(Commands)) then
      write(ERROR_UNIT, '(a)') "Error: Could not read commands file."
      stop
   end if

   ! Обработка: пролистывание текста.
   ! Начинаем с позиции 1.
   Result => Paginate(SourceText, Commands, N, 1, null())

   ! Вывод результата.
   if (associated(Result)) then
      call Output_Text(OutputFile, Result)
   else
      write(ERROR_UNIT, '(a)') "Warning: No result to output."
   end if

end program Lab_2