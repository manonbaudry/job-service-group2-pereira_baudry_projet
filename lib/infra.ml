(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/ *)
open Util

module Log = struct
  let warn s = Dream.warning @@ fun log -> log s
  let info s = Dream.info @@ fun log -> log s
  let error s = Dream.error @@ fun log -> log s
  let debug s = Dream.debug @@ fun log -> log s
end

module Environment = struct
  let app_name =
    try
      let name = Unix.getenv "APP_NAME" in
      if name <> "" then name else failwith "Empty APP_NAME is not allowed"
    with
    | Not_found ->
      let () =
        Log.warn
          "APP_NAME environment variable is not set, fallback default value"
      in
      "job.miage.rocks"

  let port =
    try Unix.getenv "PORT" |> int_of_string with
    | Failure _
    | Not_found ->
      let () =
        Log.warn
          "PORT environment variable is not properly set, fallback default \
           value 8080" in
      3001

  let hash_seed =
    try Unix.getenv "SEED" with
    | Not_found ->
      let () =
        Log.warn
          "SEED environment variable is not set, fallback default value - USE \
           ONLY FOR DEV" in
      "The first ten million years were the worst"

  let random_seed = Random.State.make_self_init ()

  let jwt_secret =
    try Unix.getenv "JWT_SECRET" with
    | Not_found ->
      let () =
        Log.warn
          "JWT_SECRET environment variable is not set, fallback default value \
           - USE ONLY FOR DEV" in
      "So Long and Thanks For All The Fish"

  let db_uri = "sqlite3:job.db"

  let log_level =
    let fallback_to_debug () =
      let () =
        Log.warn
          "LEVEL environment variable is not properly set, correct values are \
           ERROR, INFO, WARN, ERROR. Fallback to DEBUG" in
      `Debug in
    try
      match Unix.getenv "LEVEL" with
      | "DEBUG" -> `Debug
      | "INFO" -> `Info
      | "WARN" -> `Warning
      | "ERROR" -> `Error
      | _ -> fallback_to_debug ()
    with
    | Not_found -> fallback_to_debug ()
end