open Lwt

(*
   Send a (key, value) pair to Amazon Cloudwatch via our gator service,
   which sums up values over 1-min periods before sending them to Cloudwatch.

   The key (metric) is a dot-separated path of lowercase identifiers,
   such as foo.bar.
   See gator_client.mli for details, and look at existing metrics
   in our Cloudwatch dashboard.

   The value must be 0. or a positive float.
*)
let send key value =
  let conf = Config.get () in
  let open Config_t in
  Gator_client.send
    ~host: conf.gator_host
    ~port: conf.gator_port
    key value

let send_event key = send key 1.0

(*
   Measure a latency and send it to Cloudwatch.
*)
let time key f =
  let t1 = Unix.gettimeofday () in
  f () >>= fun result ->
  let t2 = Unix.gettimeofday () in
  send key (t2 -. t1) >>= fun () ->
  return result
