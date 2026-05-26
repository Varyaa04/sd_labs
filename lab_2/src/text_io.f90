module Text_IO
   use Environment
   implicit none
   
   ! Структура данных для хранения строки текста.
   type TextLine
      character(:, CH_), allocatable   :: String
      type(TextLine), pointer          :: Next => Null()
   end type TextLine

   ! Структура данных для хранения команд (F/B).
   type Command
      character(1)                     :: Cmd   ! Используем default KIND
      type(Command), pointer           :: Next => Null()
   end type Command

contains
   ! Чтение текста из файла.
   function Read_Text(InputFile) result(Text)
      type(TextLine), pointer  :: Text
      character(*), intent(in) :: InputFile
      integer                  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         Text => Read_Text_Line(In)
      close (In)
   end function Read_Text

   ! Чтение строки текста (рекурсивно).
   recursive function Read_Text_Line(In) result(Text)
      type(TextLine), pointer  :: Text
      integer, intent(in)      :: In
      integer, parameter       :: max_len = 1024
      character(max_len, CH_)  :: string
      integer                  :: IO

      read (In, "(a)", iostat=IO) string
      call Handle_IO_Status(IO, "reading line from text file")
      if (IO == 0) then
         allocate (Text)
         Text%String = Trim(string)
         Text%Next => Read_Text_Line(In)
      else
         Text => Null()
      end if
   end function Read_Text_Line

   ! Чтение команд из файла.
   function Read_Commands(InputFile) result(Commands)
      type(Command), pointer   :: Commands
      character(*), intent(in) :: InputFile
      integer                  :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         Commands => Read_Command_Line(In)
      close (In)
   end function Read_Commands

   ! Чтение одной команды (рекурсивно).
   recursive function Read_Command_Line(In) result(Commands)
      type(Command), pointer  :: Commands
      integer, intent(in)     :: In
      character(1)            :: cmd_char
      integer                 :: IO

      read (In, *, iostat=IO) cmd_char
      call Handle_IO_Status(IO, "reading command from file")
      if (IO == 0) then
         allocate (Commands)
         Commands%Cmd = cmd_char
         Commands%Next => Read_Command_Line(In)
      else
         Commands => Null()
      end if
   end function Read_Command_Line

   ! Чтение размера листа N из файла команд (первая строка).
   function Read_N(InputFile) result(N)
      integer                  :: N
      character(*), intent(in) :: InputFile
      integer                  :: In
      integer                  :: IO

      open (file=InputFile, encoding=E_, newunit=In)
         read (In, *, iostat=IO) N
         call Handle_IO_Status(IO, "reading N from file")
      close (In)
   end function Read_N

   ! Вывод текста в файл.
   subroutine Output_Text(OutputFile, Text)
      character(*), intent(in)     :: OutputFile 
      type(TextLine), intent(in)   :: Text 
      integer                      :: Out
      
      open (file=OutputFile, encoding=E_, newunit=Out)
         call Output_Text_Line(Out, Text)
      close (Out)
   end subroutine Output_Text

   ! Вывод строки текста (рекурсивно).
   recursive subroutine Output_Text_Line(Out, Text)
      integer, intent(in)          :: Out
      type(TextLine), intent(in)   :: Text
      integer                      :: IO

      write (Out, "(a)", iostat=IO) Text%String
      call Handle_IO_Status(IO, "writing line to file")
      if (Associated(Text%Next)) &
         call Output_Text_Line(Out, Text%Next)
   end subroutine Output_Text_Line

end module Text_IO