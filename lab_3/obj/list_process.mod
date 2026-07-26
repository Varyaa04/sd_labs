!mod$ v1 sum:79d8285db280a9fe
!need$ ea6dd147e57435bd n environment
module list_process
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
type::node
character(:,1),allocatable::value
type(node),allocatable::next
end type
type::stringlist
type(node),allocatable,private::head
character(:,1),allocatable,private::last_line_to_delete
integer(4),private::list_size=0_4
contains
procedure::read_from_file
procedure::output
procedure::last_line_from_file
procedure::delete_all
procedure,private::add_to_end
procedure,private::clear_list
procedure,private::delete_all_recursive
procedure,private::output_recursive
end type
contains
subroutine read_from_file(this,input_file)
class(stringlist),intent(inout)::this
character(*,1),intent(in)::input_file
end
recursive subroutine add_to_end(this,head,val)
class(stringlist),intent(inout)::this
type(node),allocatable,intent(inout)::head
character(*,1),intent(in)::val
end
recursive subroutine clear_list(this,head)
class(stringlist),intent(inout)::this
type(node),allocatable,intent(inout)::head
end
subroutine output(this,output_file,header,position)
class(stringlist),intent(in)::this
character(*,1),intent(in)::output_file
character(*,1),intent(in)::header
character(*,1),intent(in)::position
end
recursive subroutine output_recursive(this,unit,current)
class(stringlist),intent(in)::this
integer(4),intent(in)::unit
type(node),intent(in)::current
end
subroutine last_line_from_file(this,input_file)
class(stringlist),intent(inout)::this
character(*,1),intent(in)::input_file
end
subroutine delete_all(this)
class(stringlist),intent(inout)::this
end
recursive subroutine delete_all_recursive(this,head,deleted_count)
class(stringlist),intent(inout)::this
type(node),allocatable,intent(inout)::head
integer(4),intent(inout)::deleted_count
end
end
