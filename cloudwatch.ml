open Lwt

let send0 key opt_value =
  let conf = Config.get () in
  let open Config_t in
(*
  Gator_client.send
    ~host: conf.gator_host
    ~port: conf.gator_port
    key opt_value
*)
  return ()

(*
   Send a (key, value) pair to Amazon Cloudwatch via our gator service,
   which sums up values over 1-min periods before sending them to Cloudwatch.

   The key (metric) is a dot-separated path of lowercase identifiers,
   such as foo.bar.
   See gator_client.mli for details, and look at existing metrics
   in our Cloudwatch dashboard.

   The value must be greater than or equal to 0.
*)
let send key value =
  send0 key (Some value)

let send_event key =
  send0 key None

(*
   Measure a latency and send it to Cloudwatch.
*)
let time key f =
  let t1 = Unix.gettimeofday () in
  f () >>= fun result ->
  let t2 = Unix.gettimeofday () in
  send key (t2 -. t1) >>= fun () ->
  return result
