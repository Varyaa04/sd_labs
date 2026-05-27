!mod$ v1 sum:4a9aa7df4d40f19a
!need$ ea6dd147e57435bd n environment
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
type(text_node),allocatable::next
end type
type::dir_node
character(1_4,4)::dir
type(dir_node),allocatable::next
end type
character(1_4,4),parameter,private::char_f=4_"f"
character(1_4,4),parameter,private::char_f_big=4_"F"
character(1_4,4),parameter,private::char_b=4_"b"
character(1_4,4),parameter,private::char_b_big=4_"B"
private::read_win_size
private::read_text_list
private::read_dir_list
private::write_dir_list
private::write_text_list
private::next_pos
contains
subroutine read_all_data(file1,file2,text_list,dir_list,win_size)
character(*,1),intent(in)::file1
character(*,1),intent(in)::file2
type(text_node),allocatable,intent(out)::text_list
type(dir_node),allocatable,intent(out)::dir_list
integer(4),intent(out)::win_size
end
subroutine read_win_size(in_unit,win_size)
integer(4),intent(in)::in_unit
integer(4),intent(out)::win_size
end
recursive subroutine read_text_list(in_unit,head)
integer(4),intent(in)::in_unit
type(text_node),allocatable::head
end
recursive subroutine read_dir_list(in_unit,head)
integer(4),intent(in)::in_unit
type(dir_node),allocatable::head
end
subroutine write_full_output(fileout,text_list,dir_list,win_size,actions)
character(*,1),intent(in)::fileout
type(text_node),allocatable,intent(in)::text_list
type(dir_node),allocatable,intent(in)::dir_list
integer(4),intent(in)::win_size
character(:,4),allocatable,intent(in)::actions(:)
end
recursive subroutine write_dir_list(out_unit,head)
integer(4),intent(in)::out_unit
type(dir_node),allocatable,intent(in)::head
end
recursive subroutine write_text_list(out_unit,head)
integer(4),intent(in)::out_unit
type(text_node),allocatable,intent(in)::head
end
pure recursive function text_size(head) result(n)
type(text_node),intent(in)::head
integer(4)::n
end
pure recursive function get_line(head,k) result(res)
type(text_node),intent(in)::head
integer(4),intent(in)::k
character(:,4),allocatable::res
end
pure function next_pos(pos,dir,win_size,total)
integer(4),intent(in)::pos
character(1_4,4),intent(in)::dir
integer(4),intent(in)::win_size
integer(4),intent(in)::total
integer(4)::next_pos
end
recursive function paginate(text,dirs,win_size,pos,total) result(actions)
type(text_node),allocatable,intent(in)::text
type(dir_node),allocatable,intent(in)::dirs
integer(4),intent(in)::win_size
integer(4),intent(in)::pos
integer(4),intent(in)::total
character(:,4),allocatable::actions(:)
end
end
