open Util
open Amqp_client_lwt
open Infra.Log

let host = "sparrow-01.rmq.cloudamqp.com"

let credentials = ("pymgrgko", "1rBW0EAK9jf1IGMVKnKJcy5N_daFPPBZ")

let virtual_host = "pymgrgko"

let client : Rpc.Client.t option ref = ref None

let get_connection () =
  Connection.connect ~id:"auth-job" ~virtual_host:virtual_host ~credentials:credentials host

let get_channel connection =
  Connection.open_channel ~id:"auth-job"  Channel.with_confirm connection

  let get_queue channel =
    Queue.declare channel "auth-job"

let run () =
  let open LwtSyntax in
  let* connection = get_connection () in
  let* channel = get_channel connection in 
  let* queue = get_queue channel in
  Lwt.return_unit

let from_message (msg : Message.message option) =
  let open LwtSyntax in
  match msg with
  | None -> Lwt.return ""
  | Some (content, data) -> Lwt.return data


 let init_client () = let open LwtSyntax in let* connection = get_connection
 () in let* c = Rpc.Client.init ~id:"auth-job" connection in let _ = client
    := Some c in Lwt.return c