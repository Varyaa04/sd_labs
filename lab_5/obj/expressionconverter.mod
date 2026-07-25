!mod$ v1 sum:c4bf9b12e728e03d
!need$ 5cbba2cdaa980ab0 n environment
module expressionconverter
use environment,only:event_type
use environment,only:notify_type
use environment,only:lock_type
use environment,only:team_type
use environment,only:atomic_int_kind
use environment,only:atomic_logical_kind
use environment,only:compiler_options
use environment,only:compiler_version
use environment,only:selectedint8
use environment,only:selectedint16
use environment,only:selectedint32
use environment,only:selectedint64
use environment,only:selectedint128
use environment,only:safeint8
use environment,only:safeint16
use environment,only:safeint32
use environment,only:safeint64
use environment,only:safeint128
use environment,only:int8
use environment,only:int16
use environment,only:int32
use environment,only:int64
use environment,only:int128
use environment,only:selecteduint8
use environment,only:selecteduint16
use environment,only:selecteduint32
use environment,only:selecteduint64
use environment,only:selecteduint128
use environment,only:safeuint8
use environment,only:safeuint16
use environment,only:safeuint32
use environment,only:safeuint64
use environment,only:safeuint128
use environment,only:uint8
use environment,only:uint16
use environment,only:uint32
use environment,only:uint64
use environment,only:uint128
use environment,only:logical8
use environment,only:logical16
use environment,only:logical32
use environment,only:logical64
use environment,only:selectedreal16
use environment,only:selectedbfloat16
use environment,only:selectedreal32
use environment,only:selectedreal64
use environment,only:selectedreal80
use environment,only:selectedreal64x2
use environment,only:selectedreal128
use environment,only:safereal16
use environment,only:safebfloat16
use environment,only:safereal32
use environment,only:safereal64
use environment,only:safereal80
use environment,only:safereal64x2
use environment,only:safereal128
use environment,only:real16
use environment,only:bfloat16
use environment,only:real32
use environment,only:real64
use environment,only:real80
use environment,only:real64x2
use environment,only:real128
use environment,only:integer_kinds
use environment,only:real_kinds
use environment,only:logical_kinds
use environment,only:character_kinds
use environment,only:current_team
use environment,only:initial_team
use environment,only:parent_team
use environment,only:character_storage_size
use environment,only:file_storage_size
use environment,only:numeric_storage_size
use environment,only:output_unit
use environment,only:input_unit
use environment,only:error_unit
use environment,only:iostat_end
use environment,only:iostat_eor
use environment,only:iostat_inquire_internal_unit
use environment,only:stat_failed_image
use environment,only:stat_locked
use environment,only:stat_locked_other_image
use environment,only:stat_stopped_image
use environment,only:stat_unlocked
use environment,only:stat_unlocked_failed_image
use environment,only:i_
use environment,only:r_
use environment,only:c_
use environment,only:ch_
use environment,only:selected_char_kind
use environment,only:e_
use environment,only:operator(//)
use environment,only:int_plus_string
use environment,only:string_plus_int
use environment,only:handle_io_status
type,abstract::base_node
contains
procedure(print_interface),deferred,pass::print
end type
abstract interface
subroutine print_interface(this,unit)
import::base_node
class(base_node),intent(in)::this
integer(4),intent(in)::unit
end
end interface
type,extends(base_node)::expr_node
character(1_4,1)::value
type(expr_node),allocatable::left
type(expr_node),allocatable::right
contains
procedure,pass::print=>print_node
end type
type::expressionconverter
character(:,1),allocatable,private::prefix_expr
character(:,1),allocatable,private::postfix_expr
integer(4),private::pos
logical(4),private::is_valid
character(100_4,1),private::error_msg
type(expr_node),allocatable,private::root
contains
procedure::read_expression
procedure::validate_and_convert
procedure::output_result
procedure,private::parse_expression
procedure,private::to_postfix
procedure,private::check_operand
procedure,private::check_operator
procedure,private::skip_spaces
procedure,private::clear_tree
final::converter_destructor
end type
character(1_4,1),parameter::operators(1_8:4_8)=[CHARACTER(KIND=1,LEN=1)::"+","-","*","/"]
character(1_4,1),parameter::open_paren="("
character(1_4,1),parameter::close_paren=")"
contains
subroutine converter_destructor(this)
type(expressionconverter),intent(inout)::this
end
subroutine print_node(this,unit)
class(expr_node),intent(in)::this
integer(4),intent(in)::unit
end
subroutine skip_spaces(this)
class(expressionconverter),intent(inout)::this
end
subroutine read_expression(this,input_file)
class(expressionconverter),intent(inout)::this
character(*,1),intent(in)::input_file
end
subroutine validate_and_convert(this)
class(expressionconverter),intent(inout)::this
end
recursive subroutine parse_expression(this,node)
class(expressionconverter),intent(inout)::this
type(expr_node),allocatable,intent(out)::node
end
function check_operand(this,ch) result(res)
class(expressionconverter),intent(in)::this
character(1_4,1),intent(in)::ch
logical(4)::res
end
function check_operator(this,ch) result(res)
class(expressionconverter),intent(in)::this
character(1_4,1),intent(in)::ch
logical(4)::res
end
recursive subroutine to_postfix(this,node)
class(expressionconverter),intent(inout)::this
type(expr_node),allocatable,intent(in)::node
end
recursive subroutine clear_tree(this,node)
class(expressionconverter),intent(inout)::this
type(expr_node),allocatable,intent(inout)::node
end
subroutine output_result(this,output_file)
class(expressionconverter),intent(in)::this
character(*,1),intent(in)::output_file
end
end
