open! Core
open! Import

let%expect_test _ =
  let module K1 = String in
  let module K2 = Int in
  let module V = String in
  let m_v = Incr.Var.create K1.Map.empty in
  let change_m_v f = Incr.Var.set m_v (f (Incr.Var.value m_v)) in
  let change k1 k2 data =
    change_m_v (fun m ->
      Map.change m k1 ~f:(fun m_inner ->
        let m_inner =
          Map.change (Option.value m_inner ~default:K2.Map.empty) k2 ~f:(fun _ ->
            data)
        in
        if Map.is_empty m_inner then None else Some m_inner))
  in
  let m_transposed = Incr_map.transpose (module K2) (Incr.Var.watch m_v) in
  let m_transposed_transposed = Incr_map.transpose (module K1) m_transposed in
  let m_transposed_o = Incr.observe m_transposed in
  let m_transposed_transposed_o = Incr.observe m_transposed_transposed in
  let show () =
    Incr.stabilize ();
    assert (
      [%compare.equal: V.t K2.Map.t K1.Map.t]
        (Incr.Var.value m_v)
        (Incr.Observer.value m_transposed_transposed_o |> Or_error.ok_exn));
    print_s
      [%message
        ""
          ~original:(Incr.Var.value m_v : V.t K2.Map.t K1.Map.t)
          ~tranposed:
            (Incr.Observer.value m_transposed_o |> Or_error.ok_exn
             : V.t K1.Map.t K2.Map.t)]
  in
  show ();
  [%expect {|
    ((original  ())
     (tranposed ())) |}];
  change "a" 1 (Some "a_1");
  change "b" 1 (Some "b_1");
  show ();
  [%expect
    {|
    ((original (
       (a ((1 a_1)))
       (b ((1 b_1)))))
     (tranposed ((
       1 (
         (a a_1)
         (b b_1)))))) |}];
  change "c" 2 (Some "c_2");
  change "d" 3 (Some "d_3");
  change "e" 1 (Some "e_1");
  change "a" 1 None;
  change "b" 1 (Some "b_1_prime");
  show ();
  [%expect
    {|
    ((original (
       (b ((1 b_1_prime)))
       (c ((2 c_2)))
       (d ((3 d_3)))
       (e ((1 e_1)))))
     (tranposed (
       (1 (
         (b b_1_prime)
         (e e_1)))
       (2 ((c c_2)))
       (3 ((d d_3)))))) |}]
;;
