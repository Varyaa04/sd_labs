!mod$ v1 sum:0872014cdbc889ef
!need$ 5cbba2cdaa980ab0 n environment
module text_processing
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
private::event_type
private::notify_type
private::lock_type
private::team_type
private::atomic_int_kind
private::atomic_logical_kind
private::compiler_options
private::compiler_version
private::selectedint8
private::selectedint16
private::selectedint32
private::selectedint64
private::selectedint128
private::safeint8
private::safeint16
private::safeint32
private::safeint64
private::safeint128
private::int8
private::int16
private::int32
private::int64
private::int128
private::selecteduint8
private::selecteduint16
private::selecteduint32
private::selecteduint64
private::selecteduint128
private::safeuint8
private::safeuint16
private::safeuint32
private::safeuint64
private::safeuint128
private::uint8
private::uint16
private::uint32
private::uint64
private::uint128
private::logical8
private::logical16
private::logical32
private::logical64
private::selectedreal16
private::selectedbfloat16
private::selectedreal32
private::selectedreal64
private::selectedreal80
private::selectedreal64x2
private::selectedreal128
private::safereal16
private::safebfloat16
private::safereal32
private::safereal64
private::safereal80
private::safereal64x2
private::safereal128
private::real16
private::bfloat16
private::real32
private::real64
private::real80
private::real64x2
private::real128
private::integer_kinds
private::real_kinds
private::logical_kinds
private::character_kinds
private::current_team
private::initial_team
private::parent_team
private::character_storage_size
private::file_storage_size
private::numeric_storage_size
private::output_unit
private::input_unit
private::error_unit
private::iostat_end
private::iostat_eor
private::iostat_inquire_internal_unit
private::stat_failed_image
private::stat_locked
private::stat_locked_other_image
private::stat_stopped_image
private::stat_unlocked
private::stat_unlocked_failed_image
private::i_
private::r_
private::c_
private::ch_
private::selected_char_kind
private::e_
private::operator(//)
private::int_plus_string
private::string_plus_int
private::handle_io_status
type::text_node
character(:,4),allocatable::line
type(text_node),pointer::next=>NULL()
type(text_node),pointer::prev=>NULL()
end type
intrinsic::null
private::null
type::dir_node
character(1_4,4)::dir
type(dir_node),pointer::next=>NULL()
end type
type::paginator
type(text_node),pointer::current_pos=>NULL()
integer(4)::win_size=0_4
integer(4)::total_len=0_4
contains
procedure::init=>paginator_init
procedure::move_forward=>paginator_move_forward
procedure::move_back=>paginator_move_back
procedure::get_window_copy=>paginator_get_window_copy
procedure::set_position=>paginator_set_position
end type
character(1_4,4),parameter,private::char_f=4_"f"
character(1_4,4),parameter,private::char_f_big=4_"F"
character(1_4,4),parameter,private::char_b=4_"b"
character(1_4,4),parameter,private::char_b_big=4_"B"
type::text_processor
type(text_node),pointer::text_list=>NULL()
type(dir_node),pointer::dir_list=>NULL()
character(:,4),allocatable::actions(:)
integer(4)::win_size=0_4
integer(4)::total_len=0_4
contains
procedure::init=>processor_init
procedure::process=>processor_process
procedure::write=>processor_write
end type
private::read_all_data
private::processor_init
private::processor_process
private::processor_write
private::read_win_size
private::read_text_list
private::read_dir_list
private::write_full_output
private::write_dir_list
private::write_text_list
private::paginator_init
private::paginator_set_position
private::paginator_move_forward
private::paginator_move_back
private::paginator_get_window_copy
private::copy_window_recursive
private::window_copy_to_array
private::count_nodes
private::copy_to_array_recursive
private::paginate
private::get_current_position
private::text_size
contains
subroutine read_all_data(file1,file2,text_list,dir_list,win_size)
character(*,1),intent(in)::file1
character(*,1),intent(in)::file2
type(text_node),intent(out),pointer::text_list
type(dir_node),intent(out),pointer::dir_list
integer(4),intent(out)::win_size
end
subroutine processor_init(this,file1,file2)
class(text_processor),intent(inout)::this
character(*,1),intent(in)::file1
character(*,1),intent(in)::file2
end
subroutine processor_process(this)
class(text_processor),intent(inout)::this
end
subroutine processor_write(this,fileout)
class(text_processor),intent(in)::this
character(*,1),intent(in)::fileout
end
subroutine read_win_size(in_unit,win_size)
integer(4),intent(in)::in_unit
integer(4),intent(out)::win_size
end
recursive subroutine read_text_list(in_unit,head)
integer(4),intent(in)::in_unit
type(text_node),intent(out),pointer::head
end
recursive subroutine read_dir_list(in_unit,head)
integer(4),intent(in)::in_unit
type(dir_node),intent(out),pointer::head
end
subroutine write_full_output(fileout,text_list,dir_list,win_size,actions)
character(*,1),intent(in)::fileout
type(text_node),intent(in),pointer::text_list
type(dir_node),intent(in),pointer::dir_list
integer(4),intent(in)::win_size
character(:,4),allocatable,intent(in)::actions(:)
end
recursive subroutine write_dir_list(out_unit,head)
integer(4),intent(in)::out_unit
type(dir_node),intent(in),pointer::head
end
recursive subroutine write_text_list(out_unit,head)
integer(4),intent(in)::out_unit
type(text_node),intent(in),pointer::head
end
subroutine paginator_init(this,head,win_size,total_len)
class(paginator),intent(inout)::this
type(text_node),intent(in),target::head
integer(4),intent(in)::win_size
integer(4),intent(in)::total_len
end
subroutine paginator_set_position(this,pos)
class(paginator),intent(inout)::this
integer(4),intent(in)::pos
end
subroutine paginator_move_forward(this)
class(paginator),intent(inout)::this
end
subroutine paginator_move_back(this)
class(paginator),intent(inout)::this
end
function paginator_get_window_copy(this) result(window_copy)
class(paginator),intent(in)::this
type(text_node),pointer::window_copy
end
recursive subroutine copy_window_recursive(src,win_size,depth,dest)
type(text_node),intent(in),pointer::src
integer(4),intent(in)::win_size
integer(4),intent(in)::depth
type(text_node),intent(out),pointer::dest
end
function window_copy_to_array(window) result(lines)
type(text_node),intent(in),pointer::window
character(:,4),allocatable::lines(:)
end
recursive function count_nodes(head) result(n)
type(text_node),intent(in),pointer::head
integer(4)::n
end
recursive subroutine copy_to_array_recursive(node,arr,idx)
type(text_node),intent(in),pointer::node
character(:,4),allocatable,intent(inout)::arr(:)
integer(4),intent(in)::idx
end
recursive function paginate(text,dirs,win_size,start_pos,total) result(actions)
type(text_node),intent(in),pointer::text
type(dir_node),intent(in),pointer::dirs
integer(4),intent(in)::win_size
integer(4),intent(in)::start_pos
integer(4),intent(in)::total
character(:,4),allocatable::actions(:)
end
function get_current_position(pager) result(pos)
class(paginator),intent(in)::pager
integer(4)::pos
end
recursive function text_size(head) result(n)
type(text_node),intent(in),pointer::head
integer(4)::n
end
end
