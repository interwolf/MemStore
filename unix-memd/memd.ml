(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(*open OUnit*)
open Printf
open Lwt

module OrdKey = struct
  type t = string
  let compare = Pervasives.compare
end
module KVMap = Map.Make(OrdKey)

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

let build_sockaddr addr port =
  try_lwt
    (* should this be lwt hent = Lwt_lib.gethostbyname addr ? *)
    let hent = Unix.gethostbyname addr in
    return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port))
  with _ -> 
    raise_lwt (Failure ("cant resolve hostname: " ^ addr))

let init_socket sockaddr =
  let suck = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
  Lwt_unix.bind suck sockaddr;
  Lwt_unix.listen suck 15;
  suck
	
let close (ic,oc) =
  try_lwt Lwt_io.close oc with _ -> return () >>
  try_lwt Lwt_io.close ic with _ -> return ()

let conn_worker_loop ic oc =
	(* nData==0 indicates the line is a command, otherwise it is a value *)
	let data_left = ref 0 in
	let key_in_process = ref "" in
	let kv = ref KVMap.empty in
	let request_stream = Lwt_stream.from (fun () -> 
		Lwt_io.read_line_opt ic) in 
	for_lwt line in request_stream do begin
		(* printf "read line: %s\n%!" line; *)
		if !data_left > 0 then begin
			kv := KVMap.add !key_in_process line !kv;
			data_left := !data_left - String.length line;
			if !data_left == 0 then
				Lwt_io.write oc "STORED\r\n" >> Lwt_io.flush oc
			else 
				return ()
			;
			(* printf "map cardinal: %d, data_left: %d\n%!" (KVMap.cardinal !kv) !data_left; *)
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

let process_accept ~sockaddr ?timeout (client,_) =
  let ic = Lwt_io.of_fd Lwt_io.input client in
  let oc = Lwt_io.of_fd Lwt_io.output client in
  let c = conn_worker_loop ic oc in
  let events = match timeout with
    |None -> [c]
    |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
  let _ = Lwt.pick events >> 
	close (ic,oc) in
  return ()

let init ~sockaddr =
	let s = init_socket sockaddr in
	while_lwt true do
        (*the value returned by 'accept' will be the last parameter	of process_accept*)
		Lwt_unix.accept s >>=
		process_accept ~sockaddr
	done

let server ~address ~port =
  lwt sockaddr = build_sockaddr address port in
  init ~sockaddr
  
let make_server () =
  server ~address:"0.0.0.0" ~port:19000
    
let _ = Lwt_unix.run (make_server ()) 
