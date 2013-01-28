open Printf
open Lwt
open Net
(* open Cohttp_lwt_mirage *)

module OrdKey = struct
  type t = string
  let compare = Pervasives.compare
end
module KVMap = Map.Make(OrdKey)

let port = 19000

let ip =
  let open Net.Nettypes in
  ( ipv4_addr_of_tuple (10l,0l,0l,2l),
    ipv4_addr_of_tuple (255l,255l,255l,0l),
   [ipv4_addr_of_tuple (10l,0l,0l,1l)]
  )

(* type ic = Channel.t *)
(* type oc = Channel.t *)

let read_line_opt ic =
  match_lwt Channel.read_line ic with
  |[] -> return None
  |bufs -> return (Some (Cstruct.copyv bufs))

let kv = ref KVMap.empty
let set_num = ref 0

let conn_worker_loop ic oc =
	(* nData==0 indicates the line is a command, otherwise it is a value *)
	let data_left = ref 0 in
	let key_in_process = ref "" in
	(* let kv = ref KVMap.empty in *)
	let request_stream = Lwt_stream.from (fun () -> 
		read_line_opt ic) in 
	for_lwt line in request_stream do begin
		(* printf "read line: %s\n%!" line; *)
		if !data_left > 0 then begin
			kv := KVMap.add !key_in_process line !kv;
			set_num := !set_num + 1;
			data_left := !data_left - String.length line;
			if !data_left == 0 then begin
				Channel.write_line oc "STORED\r\n"; 
				Channel.flush oc;
				return ()
			end
			else 
				return ()
			;
			printf "set_num: %d, map cardinal: %d, data_left: %d\n%!"
	!set_num (KVMap.cardinal !kv) !data_left;
			return ()
		end
		else begin
			let str_sep = Re_str.regexp_string " " in
			let _ = match Re_str.split_delim str_sep line with
			|"TYPE" :: line -> () (* printf "TYPE command\n%!" *)
			|["set"; key; x; y; len] ->
				key_in_process := key;
				data_left := int_of_string len
			|unknown :: line -> printf "unknown command %s\n%!" unknown
			in 
			return ()	
		end	
	end	
	done   

let accept_num = ref 0

let listen_memd mgr src spec =
  (* TODO XXX the cancel-based timeout is almost certainly broken as the
   * thread wont issue a Response *)
	let cb = fun dst ch -> 
		accept_num := !accept_num + 1;
		printf "accept_num: %d\n%!" !accept_num;
		conn_worker_loop ch ch
	in
	Net.Channel.listen mgr (`TCPv4 (src, cb))

type config = {
    callback_extra: unit -> unit;
    conn_closed : int -> unit -> unit;
}

let main () =
	printf "*************listening to MEMD on port %d*************\n" port;
	let callback_extra () = 
		printf "this is mirage-memd callback\n%!" in
	let conn_closed conn_id () =
		printf "conn %s closed\n%!" (string_of_int conn_id) in
	let spec = { callback_extra; conn_closed } in
	Net.Manager.create (
	fun mgr interface id ->
		let src = None, port in
		Net.Manager.configure interface (`IPv4 ip) >>
		listen_memd mgr src spec
	)
