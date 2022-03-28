(* This Source Code Form is subject to the terms of the Mozilla Public License,
      v. 2.0. If a copy of the MPL was not distributed with this file, You can
      obtain one at https://mozilla.org/MPL/2.0/ *)
open Infra.Log
open Util
open Rabbit
open Amqp_client_lwt
      
module JobService = Service.Job (Repository.Job)
module JwtService = Service.Jwt
      
(** Heartbeat route *)
let hello_handler _request =
  let () = debug "Call hello_handler" in
  let open Yojson.Safe in
  Dream.json @@ to_string @@ `Assoc [("message", `String "hello world")]

    
(** create job route *)
let create_handler request =
  let () = debug "Call create_handler" in
  let open Yojson.Safe.Util in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    match !Rabbit.client with
    | None ->  Dream.json ~status:`Bad_Request "Connection error with authentication-service"
    | Some c -> 
      let* message = (Rpc.Client.call c Exchange.default ~ttl:500 ~routing_key:"authQueue" ~headers:[] ( token |> Message.make )) in
      match message with 
      | None -> Dream.json ~status:`Forbidden ""
      | Some (content, role) -> 
        if String.equal role "announcer"
          then 
            let* body = Dream.body request in
            let json_res = try Ok (Yojson.Safe.from_string body) with
            | Failure _ -> Error "Invalid JSON Body" in
            match json_res with
            | Error e -> Dream.json ~status:`Bad_Request e
            | Ok json ->
              let title = json |> member "title" |> to_string
              and company = json |> member "company" |> to_string 
              and city = json |> member "city" |> to_string 
              and job_description = json |> member "job_description" |> to_string 
              and company_description = json |> member "company_description" |> to_string
              and created_at = json |> member "created_at" |> to_string  
              and end_date = json |> member "end_date" |> to_string 
              and contact_email = json |> member "contact_email" |> to_string  
              and contract_type = json |> member "contract_type" |> to_string  
              and duration = json |> member "duration" |> to_string in
              let* update_result = 
                Dream.sql request @@ JobService.create ~title ~company ~city ~job_description ~company_description ~created_at ~end_date ~contact_email ~contract_type ~duration in
              match update_result with
              | Error e -> Dream.json ~status:`Forbidden e
              | Ok _ -> Dream.json ~status:`OK ""
            else 
              Dream.json ~status:`Forbidden ""
        
      
(** get job by id route *)
let get_by_id_handler request =
  let () = debug "Call get_by_id_handler" in
  let open Yojson.Safe in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    match !Rabbit.client with
    | None ->  Dream.json ~status:`Bad_Request "Connection error with authentication-service"
    | Some c -> 
      let* message = (Rpc.Client.call c Exchange.default ~ttl:500 ~routing_key:"authQueue" ~headers:[] ( token |> Message.make )) in
      match message with 
      | None -> Dream.json ~status:`Forbidden ""
      | Some (content, role) -> 
        if String.equal role "announcer" || String.equal role "member"
        then 
          let id = Dream.param request "id" in
          let* get_by_id_result = Dream.sql request @@ JobService.get_by_id ~id in
          match get_by_id_result with
          | Error e -> Dream.json ~status:`Forbidden e
          | Ok job -> Dream.json ~status:`OK job
        else
          Dream.json ~status:`Forbidden ""
 
      
(** get jobs by city route *)
let get_by_city_handler request =
  let () = debug "Call get_by_city_handler" in
  let open Yojson.Safe in
  let open LwtSyntax in
  let city = Dream.param request "city" in
  let* get_by_city_result = Dream.sql request @@ JobService.get_by_city ~city in
  match get_by_city_result with
  | Error e -> Dream.json ~status:`Forbidden e
  | Ok jobs -> Dream.json ~status:`OK (String.concat "" (List.map (fun o -> Domain.Job.show o) jobs ))
      
      
(** update job route *)
let update_handler request =
  let () = debug "Call update_handler" in
  let open Yojson.Safe.Util in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    match !Rabbit.client with
    | None ->  Dream.json ~status:`Bad_Request "Connection error with authentication-service"
    | Some c -> 
      let* message = 
        (Rpc.Client.call c Exchange.default ~ttl:500 ~routing_key:"authQueue" ~headers:[] ( token |> Message.make )) in
      match message with 
      | None -> Dream.json ~status:`Forbidden ""
      | Some (content, role) -> 
        if String.equal role "announcer"
        then 
          let id = Dream.param request "id" in
          let* body = Dream.body request in
          let json_res = try Ok (Yojson.Safe.from_string body) with
          | Failure _ -> Error "Invalid JSON Body" in
          match json_res with
          | Error e -> Dream.json ~status:`Bad_Request e
          | Ok json ->
            let title = json |> member "title" |> to_string
            and company = json |> member "company" |> to_string 
            and city = json |> member "city" |> to_string 
            and job_description = json |> member "job_description" |> to_string 
            and company_description = json |> member "company_description" |> to_string 
            and end_date = json |> member "end_date" |> to_string 
            and contact_email = json |> member "contact_email" |> to_string  
            and contract_type = json |> member "contract_type" |> to_string  
            and duration = json |> member "duration" |> to_string in
            let* update_result = 
              Dream.sql request @@ JobService.update ~id ~title ~company ~city ~job_description ~company_description ~end_date ~contact_email ~contract_type ~duration in
            match update_result with
            | Error e -> Dream.json ~status:`Forbidden e
            | Ok _ -> Dream.json ~status:`OK ""
        else
          Dream.json ~status:`Forbidden ""
      
      
(** delete job route *)
let delete_handler request =
  let () = debug "Call delete_handler" in
  let open Yojson.Safe in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    match !Rabbit.client with
    | None ->  Dream.json ~status:`Bad_Request "Connection error with authentication-service"
    | Some c -> 
      let* message = 
        (Rpc.Client.call c Exchange.default ~ttl:500 ~routing_key:"authQueue" ~headers:[] ( token |> Message.make )) in
      match message with 
      | None -> Dream.json ~status:`Forbidden ""
      | Some (content, role) -> 
        if String.equal role "announcer"
        then 
          let id = Dream.param request "id" in
          let* delete_result = Dream.sql request @@ JobService.delete ~id  in
          match delete_result with
          | Error e -> Dream.json ~status:`Forbidden e
          | Ok _ -> Dream.json ~status:`OK ""
        else
          Dream.json ~status:`Forbidden ""
      
      
let routes =
  [
    Dream.get "/" hello_handler;
    Dream.post "/job" create_handler;
    Dream.get "/job/:id" get_by_id_handler;
    Dream.put "/job/:id" update_handler;
    Dream.delete "/job/:id" delete_handler;
    Dream.get "jobs/:city" get_by_city_handler;
  ]      