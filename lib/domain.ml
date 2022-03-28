module Email : sig
  exception Invalid_email

  type t [@@deriving show]

  val make : string -> (t, exn) result
end = struct
  exception Invalid_email

  type t = Emile.address [@@deriving show]

  let make email =
    let maked_email = Emile.address_of_string email in
    match maked_email with
    | Ok address -> Ok address
    | Error _ -> Error Invalid_email
end

module Hash : sig
  exception Invalid_password

  type t

  val make : ?count:int -> ?seed:string -> string -> t
  val verify : string -> t -> (bool, exn) result
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val of_string : string -> t
end = struct
  exception Invalid_password

  type t = Bcrypt.hash

  let make = Bcrypt.hash ~variant:Bcrypt.Y

  let verify password hash =
    if Bcrypt.verify password hash then Ok true else Error Invalid_password

  let show = Bcrypt.string_of_hash
  let pp ppf hash = Format.pp_print_string ppf (show hash)
  let of_string = Bcrypt.hash_of_string
end

module Uuid : sig
  exception Invalid_uuid

  type t

  val v4_gen : Random.State.t -> unit -> t
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val make : string -> (t, exn) result
end = struct
  exception Invalid_uuid

  include Uuidm

  let make uuid_string =
    Uuidm.of_string uuid_string |> Option.to_result ~none:Invalid_uuid

  let show u = to_string u
end

module Job : sig
  type t = {
    id : Uuid.t;
    title : string;
    company : string;
    job_description : string;
    company_description : string;
    created_at : string;
    end_date : string;
    contact_email : Email.t;
    contract_type : string;
    duration : string; 
    is_deleted : bool;
    ranking : string;
  }
  [@@deriving make, show]

  val id : t -> Uuid.t
  val title : t -> string
  val company : t -> string
  val job_description : t -> string
  val company_description : t -> string
  val created_at : t -> string
  val end_date : t -> string
  val contact_email : t -> Email.t
  val contract_type : t -> string
  val duration : t -> string
  val is_deleted : t -> bool
  val ranking : t -> string
end = struct
  type t = {
    id : Uuid.t;
    title : string;
    company : string;
    job_description : string;
    company_description : string;
    created_at : string;
    end_date : string;
    contact_email : Email.t;
    contract_type : string;
    duration : string; 
    is_deleted : bool;
    ranking : string;
  }
  [@@deriving make, show]

  let id job = job.id
  let title job = job.title
  let company job = job.company
  let job_description job = job.job_description
  let company_description job = job.company_description
  let created_at job = job.created_at
  let end_date job = job.end_date
  let contact_email job = job.contact_email
  let contract_type job = job.contract_type
  let duration job = job.duration
  let is_deleted  job = job.is_deleted
  let ranking job = job.ranking
end