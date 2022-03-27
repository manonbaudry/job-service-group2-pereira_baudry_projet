(* This Source Code Form is subject to the terms of the Mozilla Public License,
      v. 2.0. If a copy of the MPL was not distributed with this file, You can
      obtain one at https://mozilla.org/MPL/2.0/ *)
open Infra.Log
open Util
      
module JobServive = Service.Job (Repository.Job)
module JwtService = Service.Jwt
      
(** Heartbeat route *)
let hello_handler _request =
  let () = debug "Call hello_handler" in
  let open Yojson.Safe in
  Dream.json @@ to_string @@ `Assoc [("message", `String "hello world")]
      
(** echo the authorization header in body response; for testing purpose *)
let echo_handler request =
  let () = debug "Call echo_handler" in
  let open Yojson.Safe in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    Dream.json ~status:`OK @@ to_string @@ `Assoc [("token", `String token)]


(* verify route *)
let verify_handler request =
  let () = debug "Call verify_handler" in
  let open Yojson.Safe.Util in
  let open LwtSyntax in
  let* body = Dream.body request in
  let json_res =
    try Ok (Yojson.Safe.from_string body) with
    | Failure _ -> Error "Invalid JSON Body" in
  match json_res with
  | Error e -> Dream.json ~status:`Bad_Request e
  | Ok json ->
    let token = json |> job "jwt" |> to_string in
    let verify_result = JwtService.verify_and_get_iss token in
    match verify_result with
    | Error e -> Dream.json ~status:`Forbidden e
    | Ok _ -> Dream.json ~status:`OK ""
 
    
(** create job route *)
let create_handler request =
  let () = debug "Call create_handler" in
  let open Yojson.Safe.Util in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    let verify_result = JwtService.verify_and_get_iss token in
    match verify_result with
    | Error e -> Dream.json ~status:`Forbidden e
    | Ok _ ->
      let* body = Dream.body request in
      let json_res = try Ok (Yojson.Safe.from_string body) with
      | Failure _ -> Error "Invalid JSON Body" in
      match json_res with
      | Error e -> Dream.json ~status:`Bad_Request e
      | Ok json ->
        let title = json |> job "title" |> to_string
        and description  = json |> job "description" |> to_string
        and company = json |> job "company" |> to_string 
        and job_description = json |> job "job_description" |> to_string 
        and company_description = json |> job "company_description" |> to_string 
        and end_date = json |> job "end_date" |> to_string 
        and contact_email = json |> job "contact_email" |> to_string  
        and contract_type = json |> job "contract_type" |> to_string  
        and duration = json |> job "duration" |> to_string in
        let* update_result = 
          Dream.sql request @@ JobService.create ~title ~description ~company ~job_description ~company_description ~end_date ~contact_email ~contract_type ~duration in
        match update_result with
        | Error e -> Dream.json ~status:`Forbidden e
        | Ok _ -> Dream.json ~status:`OK ""
      
(** get job by id route *)
let get_by_id_handler request =
  let () = debug "Call get_by_id_handler" in
  let open Yojson.Safe in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token -> 
    let verify_result = JwtService.verify_and_get_iss token in
    match verify_result with
    | Error e -> Dream.json ~status:`Forbidden e
    | Ok _ ->
      let id = Dream.param request "id" in
      let* get_by_id_result = Dream.sql request @@ JobServive.get_by_id ~id in
      match get_by_id_result with
      | Error e -> Dream.json ~status:`Forbidden e
      | Ok job -> Dream.json ~status:`OK job
      
      
(** update job route *)
let update_handler request =
  let () = debug "Call update_handler" in
  let open Yojson.Safe.Util in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token ->
    let verify_result = JwtService.verify_and_get_iss token in
    match verify_result with
    | Error e -> Dream.json ~status:`Forbidden e
    | Ok _ ->
      let id = Dream.param request "id" in
      let* body = Dream.body request in
      let json_res = try Ok (Yojson.Safe.from_string body) with
      | Failure _ -> Error "Invalid JSON Body" in
      match json_res with
      | Error e -> Dream.json ~status:`Bad_Request e
      | Ok json ->
        let title = json |> job "title" |> to_string
        and description  = json |> job "description" |> to_string
        and company = json |> job "company" |> to_string 
        and job_description = json |> job "job_description" |> to_string 
        and company_description = json |> job "company_description" |> to_string 
        and end_date = json |> job "end_date" |> to_string 
        and contact_email = json |> job "contact_email" |> to_string  
        and contract_type = json |> job "contract_type" |> to_string  
        and duration = json |> job "duration" |> to_string in
        let* update_result = Dream.sql request @@ JobService.update ~id ~title ~description ~company ~job_description ~company_description ~end_date ~contact_email ~contract_type ~duration in
        match update_result with
        | Error e -> Dream.json ~status:`Forbidden e
        | Ok _ -> Dream.json ~status:`OK ""
      
      
(** delete job route *)
let delete_handler request =
  let () = debug "Call delete_handler" in
  let open Yojson.Safe in
  let open LwtSyntax in
  match Dream.header request "Authorization" with
  | None -> Dream.json ~status:`Bad_Request "Authorization header required"
  | Some token -> 
    let verify_result = JwtService.verify_and_get_iss token in
    match verify_result with
    | Error e -> Dream.json ~status:`Forbidden e
    | Ok _ ->
      let id = Dream.param request "id" in
      let* delete_result = Dream.sql request @@ JobServive.delete ~id  in
      match delete_result with
      | Error e -> Dream.json ~status:`Forbidden e
      | Ok _ -> Dream.json ~status:`OK ""
      
      
let routes =
  [
    Dream.get "/" hello_handler;
    Dream.get "/echo" echo_handler;
    Dream.post "/verify" verify_handler;
    Dream.post "/job/create" signup_handler;
    Dream.get "/job/:id" get_by_id_handler;
    Dream.put "/job/:id" update_handler;
    Dream.delete "/job/:id" delete_handler;
  ]      