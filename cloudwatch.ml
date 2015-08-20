open Lwt

(* Ensure that we don't evaluate this before the config is loaded. *)
let gator_send = Util_half_lazy.create (fun () ->
  let conf = Config.get () in
  let open Config_t in
  Gator_client.make_send
    ~host: conf.gator_host
    ~port: conf.gator_port
    ()
)

let send0 key opt_value =
  catch
    (fun () ->
       (gator_send ()) key opt_value
    )
    (fun e ->
       let s = Log.string_of_exn e in
       Log.logf `Error "Gator_client.send failed with exception %s" s;
       return ()
    )


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
  Perf.add_latency key value;
  send0 key (Some value)

let send_event ?(n=1) key =
  let l = Array.to_list (Array.make n ()) in
  Lwt_list.iter_p (fun () -> send0 key None) l

(*
   Measure a latency of a computation and send it to Cloudwatch.
   If the computation results in an exception, the ".exn" suffix is
   appended to the key.
*)
let time key f =
  let t1 = Unix.gettimeofday () in
  let finally key =
    let t2 = Unix.gettimeofday () in
    send key (t2 -. t1)
  in
  catch
    (fun () ->
       f () >>= fun result ->
       finally key >>= fun () ->
       return result
    )
    (fun e ->
       finally (key ^ ".exn") >>= fun () ->
       raise e
    )
