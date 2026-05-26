!mod$ v1 sum:3ffe7858b6d3de20
!need$ c44d789aa54bc7e3 n text_io
!need$ 5cbba2cdaa980ab0 n environment
module text_process
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
use text_io,only:textline
use text_io,only:command
use text_io,only:read_text
use text_io,only:read_text_line
use text_io,only:read_commands
use text_io,only:read_command_line
use text_io,only:read_n
use text_io,only:output_text
use text_io,only:output_text_line
contains
recursive function get_list_length(head) result(len)
type(textline),intent(in),pointer::head
integer(4)::len
end
recursive function get_nth_node(head,n) result(node)
type(textline),intent(in),pointer::head
integer(4),intent(in)::n
type(textline),pointer::node
end
recursive function make_slice(head,count) result(slice)
type(textline),intent(in),pointer::head
integer(4),intent(in)::count
type(textline),pointer::slice
end
recursive function list_to_string(head) result(res_str)
type(textline),intent(in),pointer::head
character(:,4),allocatable::res_str
end
recursive function append_result(accumulator,new_string) result(res)
type(textline),intent(in),pointer::accumulator
character(*,4),intent(in)::new_string
type(textline),pointer::res
end
recursive function reverse_list(head) result(reversed)
type(textline),intent(in),pointer::head
type(textline),pointer::reversed
end
recursive function paginate(head,commands,n,current_pos,accumulator) result(res)
type(textline),intent(in),pointer::head
type(command),intent(in),pointer::commands
integer(4),intent(in)::n
integer(4),intent(in)::current_pos
type(textline),intent(in),pointer::accumulator
type(textline),pointer::res
end
end
