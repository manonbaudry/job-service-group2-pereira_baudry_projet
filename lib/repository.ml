(* This Source Code Form is subject to the terms of the Mozilla Public License,
      v. 2.0. If a copy of the MPL was not distributed with this file, You can
      obtain one at https://mozilla.org/MPL/2.0/ *)

      (*todo remove member*)
      module E = Infra.Environment
      module D = Domain
      
      module type Job = sig
        type ('res, 'err) query_result =
          ('res, ([> Caqti_error.call_or_retrieve] as 'err)) result Lwt.t
    
        val get_by_id :
        id:D.Uuid.t ->
        (module Rapper_helper.CONNECTION) ->
        (D.Job.t, ([> Caqti_error.call_or_retrieve] as 'err)) query_result
      
        val create :
          id:D.Uuid.t ->
          email:D.Email.t ->
          hash:D.Hash.t ->
          (module Rapper_helper.CONNECTION) ->
          (unit, ([> Caqti_error.call_or_retrieve] as 'err)) query_result
      
        val update :
          email:D.Email.t ->
          username:string option ->
          hash:D.Hash.t ->
          id:D.Uuid.t ->
          (module Rapper_helper.CONNECTION) ->
          (unit, ([> Caqti_error.call_or_retrieve] as 'err)) query_result
      
        val delete :
          id:D.Uuid.t ->
          (module Rapper_helper.CONNECTION) ->
          (unit, ([> Caqti_error.call_or_retrieve] as 'err)) query_result
      end
      
      module Job : JOB = struct
        module Uuid = struct
          type t = D.Uuid.t
      
          let t =
            let encode uuid = Ok (D.Uuid.show uuid) in
            let decode uuid =
              D.Uuid.make uuid |> Result.map_error (fun _ -> "Invalid") in
            Caqti_type.(custom ~encode ~decode string)
        end
      
        module Email = struct
          type t = D.Email.t
      
          let t =
            let encode email = Ok (D.Email.show email) in
            let decode email =
              D.Email.make email |> Result.map_error (fun _ -> "Invalid") in
            Caqti_type.(custom ~encode ~decode string)
        end
      
        module Hash = struct
          type t = D.Hash.t
      
          let t =
            let encode hash = Ok (D.Hash.show hash) in
            let decode hash = Ok (D.Hash.of_string hash) in
            Caqti_type.(custom ~encode ~decode string)
        end
      
        type ('res, 'err) query_result =
          ('res, ([> Caqti_error.call_or_retrieve] as 'err)) result Lwt.t
      
          let get_by_id_query =
            let open D.Job in
            [%rapper
              get_one
                {sql| 
                  SELECT @Uuid{id}, @string{title}, @string{description}, @string{company}, @string{job_description}, 
                  @string{company_description}, @string{created_at}, @string{end_date}, @Email{contact_email},
                  @string{contract_type}, @int{duration}, @float{ranking}
                  FROM "Job" 
                  WHERE id = %Uuid{id}
                |sql} 
                record_out]
      
        let create_query =
          [%rapper
            execute
              {sql|
                INSERT INTO "Job" (id, title, description, company, job_description, company_description, 
                created_at, end_date, contact_email, contract_type, duration, ranking, is_deleted) 
                VALUES  (%Uuid{id}, %string{title}, %string{description}, %string{company}, %string{job_description}, 
                %string{company_description}, %string{created_at}, %string{end_date}, %Email{contact_email},
                %string{contract_type}, %int{duration}, %float{ranking}, FALSE)
                |sql}]
      
        let update_query =
          [%rapper
            execute
              {sql|
                UPDATE "Job"
                SET (title, description, company, job_description, company_description, 
                created_at, end_date, contact_email, contract_type, duration, ranking) = 
                (%Uuid{id}, %string{title}, %string{description}, %string{company}, %string{job_description}, 
                %string{company_description}, %string{created_at}, %string{end_date}, %Email{contact_email},
                %string{contract_type}, %int{duration}, %float{ranking})
                WHERE id = %Uuid{id}
              |sql}]
      
        let delete_query =
          let open D.Job in
          [%rapper
            execute
              {sql| UPDATE "Job" SET (is_deleted) = TRUE WHERE id = %Uuid{id} |sql}]
      
      
        let get_by_email_hash = get_by_email_hash_query
        let get_by_id = get_by_id_query
        let create = create_query
        let update = update_query
        let delete = delete_query
      end
      