(* This Source Code Form is subject to the terms of the Mozilla Public License,
v. 2.0. If a copy of the MPL was not distributed with this file, You can
obtain one at https://mozilla.org/MPL/2.0/ *)
open Util
module D = Domain
module E = Infra.Environment
      
module Jwt = struct
  let verify jwt = Jwto.decode_and_verify E.jwt_secret jwt

  let verify_and_get_iss jwt =
    let open ResultSyntax in
    let* decoded = verify jwt in
    let payload = Jwto.get_payload decoded in
    let iss =
      Option.fold ~none:"" ~some:(fun x -> x) @@ Jwto.get_claim "iss" payload
    and exp =
      Option.fold ~none:"" ~some:(fun x -> x) @@ Jwto.get_claim "exp" payload
    and sub =
      Option.fold ~none:"" ~some:(fun x -> x) @@ Jwto.get_claim "sub" payload
    in
    let exp_float =
      Option.fold ~none:0. ~some:(fun x -> x) @@ float_of_string_opt exp in
    if iss = E.app_name && exp_float > Unix.time () then
      Ok sub
    else
      Error "Invalid token"
      
  let days_to_timestamp x = x *. 86400.
end
      
      
module Job (JobRepository : Repository.JOB) = struct
  
  let create ~title ~description ~company ~job_description ~company_description ~end_date ~contact_email ~contract_type ~duration connection =
    match D.Email.make contact_email with
    | Error e -> Lwt.return_error @@ "Invalid email: " ^ contact_email
    | Ok email ->
      let open Lwt in
      let id = D.Uuid.v4_gen E.random_seed ()
      and created_at = "28/03/2022" 
      and ranking = 0.0 in
      JobRepository.create ~id ~title ~description ~company ~job_description ~company_description ~created_at ~end_date ~contact_email:email ~contract_type ~duration ~ranking connection
      >>= function
      | Ok db_result -> Lwt.return_ok ()
      | Error _ -> Lwt.return_error "Unable to create the job offer"

      
  let get_by_id ~id connection =
    match D.Uuid.make id with
    | Error e -> Lwt.return_error @@ "Invalid id: " ^ id
    | Ok job_id ->
      let open Lwt in
      JobRepository.get_by_id ~id:job_id connection
      >>= function
      | Ok db_result -> Lwt.return_ok (D.Job.show db_result)
      | Error _ -> Lwt.return_error "Unable to retrive a job from this id"
      
      
  let update ~id ~title ~description ~company ~job_description ~company_description ~end_date ~contact_email ~contract_type ~duration connection =
    match D.Email.make contact_email with
    | Error e -> Lwt.return_error @@ "Invalid email: " ^ contact_email
    | Ok email ->
      match D.Uuid.make id with
      | Error e -> Lwt.return_error @@ "Invalid id: " ^ id
      | Ok job_id ->
        let open Lwt in
        let ranking = 0.0 in 
        JobRepository.update ~id:job_id ~title ~description ~company ~job_description ~company_description ~end_date ~contact_email:email ~contract_type ~duration ~ranking  connection
        >>= function
        | Ok db_result -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error "Unable to update the job"
        
          
  let delete ~id connection =
    match D.Uuid.make id with
    | Error e -> Lwt.return_error @@ "Invalid id: " ^ id
    | Ok job_id ->
      let open Lwt in
      JobRepository.delete ~id:job_id connection
      >>= function
      | Ok db_result -> Lwt.return_ok ()
      | Error _ -> Lwt.return_error "Unable to delete a job from this id"
end
      