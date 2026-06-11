!mod$ v1 sum:8393402358a6b0be
!need$ 5cbba2cdaa980ab0 n environment
module circularlist
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
procedure(equals_interface),deferred,pass::equals
end type
abstract interface
subroutine print_interface(this,unit)
import::base_node
class(base_node),intent(in)::this
integer(4),intent(in)::unit
end
end interface
abstract interface
function equals_interface(this,name)
import::base_node
class(base_node),intent(in)::this
character(*,1),intent(in)::name
logical(4)::equals_interface
end
end interface
type,extends(base_node)::node
character(:,1),allocatable::name
type(node),pointer::next=>NULL()
contains
procedure,pass::print=>print_node
procedure,pass::equals=>node_equals
end type
intrinsic::null
type::circularlist
type(node),pointer,private::head=>NULL()
type(node),pointer,private::current=>NULL()
integer(4),private::size=0_4
contains
procedure::read_names
procedure::play_game
procedure::output_result
procedure,private::add_to_circular
procedure,private::find_starting_node
procedure,private::counting_game_recursive
procedure,private::print_remaining
procedure,private::clear_list
final::circularlist_destructor
end type
contains
subroutine circularlist_destructor(this)
type(circularlist),intent(inout)::this
end
subroutine print_node(this,unit)
class(node),intent(in)::this
integer(4),intent(in)::unit
end
function node_equals(this,name)
class(node),intent(in)::this
character(*,1),intent(in)::name
logical(4)::node_equals
end
subroutine read_names(this,input_file)
class(circularlist),intent(inout)::this
character(*,1),intent(in)::input_file
end
recursive subroutine add_to_circular(this,name)
class(circularlist),intent(inout)::this
character(*,1),intent(in)::name
end
recursive subroutine add_recursive(head,name)
type(node),intent(in),pointer::head
character(*,1),intent(in)::name
end
recursive subroutine close_circular(head)
type(node),intent(in),pointer::head
end
subroutine play_game(this,start_name,m)
class(circularlist),intent(inout)::this
character(*,1),intent(in)::start_name
integer(4),intent(in)::m
end
recursive subroutine counting_game_recursive(this,start_node,m,remaining)
class(circularlist),intent(inout)::this
type(node),intent(in),pointer::start_node
integer(4),intent(in)::m
integer(4),intent(in)::remaining
end
subroutine find_starting_node(this,start_name)
class(circularlist),intent(inout)::this
character(*,1),intent(in)::start_name
end
subroutine print_remaining(this,start_node,count)
class(circularlist),intent(in)::this
type(node),intent(in),pointer::start_node
integer(4),intent(in)::count
end
subroutine output_result(this,output_file)
class(circularlist),intent(in)::this
character(*,1),intent(in)::output_file
end
recursive subroutine clear_list(this)
class(circularlist),intent(inout)::this
end
recursive subroutine clear_recursive(node_ptr)
type(node),intent(inout),pointer::node_ptr
end
end
