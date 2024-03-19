open Core
open Import

module type S = sig
  val unzip_mapi'
    :  data_equal:('d -> 'd -> bool)
    -> data_equal_left:('a -> 'a -> bool)
    -> data_equal_right:('b -> 'b -> bool)
    -> ('c, 'd, 'e) Map.t Incr.t
    -> f:(key:'c -> data:'d Incr.Z.t -> 'a Incr.t * 'b Incr.t)
    -> ('c, 'a, 'e) Map.t Incr.t * ('c, 'b, 'e) Map.t Incr.t
end

module Make_test (S : S) = struct
  let%expect_test "simple unzip_mapi'" =
    let m =
      String.Map.of_alist_exn [ "foo", 3; "bar", 10; "snoo", 5 ] |> Incr.Var.create
    in
    let l, r =
      S.unzip_mapi'
        ~data_equal:Int.equal
        ~data_equal_left:(Option.equal Int.equal)
        ~data_equal_right:Int.equal
        (Incr.Var.watch m)
        ~f:(fun ~key:_ ~data:x ->
        ( (let%map x = x in
           let y = x * x in
           if y > 10 then Some y else None)
        , x ))
    in
    let fm = Incr.both l r |> Incr.observe in
    let dump () =
      Incr.stabilize ();
      [%sexp_of: int String.Map.t * string * (int option String.Map.t * int String.Map.t)]
        (Incr.Var.value m, "->", Incr.Observer.value_exn fm)
      |> Sexp.to_string_hum
      |> print_endline
    in
    let change f =
      Incr.Var.set m (f (Incr.Var.value m));
      dump ()
    in
    dump ();
    [%expect
      {|
      (((bar 10) (foo 3) (snoo 5)) ->
       (((bar (100)) (foo ()) (snoo (25))) ((bar 10) (foo 3) (snoo 5))))
      |}];
    change (fun m -> Map.set m ~key:"foo" ~data:9);
    [%expect
      {|
      (((bar 10) (foo 9) (snoo 5)) ->
       (((bar (100)) (foo (81)) (snoo (25))) ((bar 10) (foo 9) (snoo 5))))
      |}];
    change (fun m -> Map.set m ~key:"bar" ~data:1);
    [%expect
      {|
      (((bar 1) (foo 9) (snoo 5)) ->
       (((bar ()) (foo (81)) (snoo (25))) ((bar 1) (foo 9) (snoo 5))))
      |}];
    change (fun m -> Map.remove m "snoo");
    [%expect {| (((bar 1) (foo 9)) -> (((bar ()) (foo (81))) ((bar 1) (foo 9)))) |}]
  ;;

  let%test_unit "unzip_mapi' randomised fuzz test" =
    Quickcheck.test
      ~sexp_of:[%sexp_of: (int, int) Map_operations.t list]
      (Map_operations.quickcheck_generator Int.quickcheck_generator)
      ~f:(fun operations ->
      let m = Incr.Var.create Int.Map.empty in
      let watch_m = Incr.Var.watch m
      and f ~key ~data =
        ( (let%map data = data in
           let y = data * data in
           Option.some_if (key + y > 33) y)
        , data )
      in
      let incr_left, incr_right =
        S.unzip_mapi'
          watch_m
          ~data_equal:[%equal: int]
          ~data_equal_left:[%equal: int option]
          ~data_equal_right:[%equal: int]
          ~f
      and slow_left, slow_right =
        let paired =
          Incr.Map.mapi' watch_m ~f:(fun ~key ~data ->
            let left, right = f ~key ~data in
            Incr.both left right)
        in
        Incr.Map.map paired ~f:fst, Incr.Map.map paired ~f:snd
      in
      let incr_left = Incr.observe incr_left
      and incr_right = Incr.observe incr_right
      and slow_left = Incr.observe slow_left
      and slow_right = Incr.observe slow_right in
      Map_operations.run_operations operations ~into:m ~after_stabilize:(fun () ->
        [%test_result: int option Int.Map.t]
          ~expect:(Incr.Observer.value_exn slow_left)
          (Incr.Observer.value_exn incr_left);
        [%test_result: int Int.Map.t]
          ~expect:(Incr.Observer.value_exn slow_right)
          (Incr.Observer.value_exn incr_right));
      Incr.Observer.disallow_future_use incr_left;
      Incr.Observer.disallow_future_use incr_right;
      Incr.Observer.disallow_future_use slow_left;
      Incr.Observer.disallow_future_use slow_right)
  ;;

  let%bench_module "unzip_mapi'" =
    (module struct
      let test_data ~size ~operations =
        (* It is important this is deterministic *)
        Quickcheck.random_value
          ~seed:
            (`Deterministic
              (sprintf
                 "%i-%i-%i-hello world, do not forget your towel"
                 42
                 size
                 operations))
          (Map_operations.quickcheck_generator
             Int.quickcheck_generator
             ~keys_size:size
             ~operations)
      ;;

      let benchmark_unzip_mapi' unzip_mapi' ~operations =
        let var = Incr.Var.create Int.Map.empty
        and f ~key ~data =
          ( (let%map data = data in
             let y = data * data in
             Option.some_if (key + y > 33) y)
          , let%map data = data in
            data + 20 )
        in
        let left, right =
          unzip_mapi' ?cutoff:None ?data_equal:(Some Int.equal) (Incr.Var.watch var) ~f
        in
        let left = Incr.observe left
        and right = Incr.observe right in
        Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
          ignore (Incr.Observer.value_exn left);
          ignore (Incr.Observer.value_exn right));
        Incr.Observer.disallow_future_use left;
        Incr.Observer.disallow_future_use right
      ;;

      let%bench_fun ("unzip_mapi' random-ops" [@indexed
                                                operations = [ 5000; 10000; 100000 ]])
        =
        let operations = test_data ~size:(operations / 100) ~operations in
        fun () ->
          benchmark_unzip_mapi' (Incr.Map.unzip_mapi' ?instrumentation:None) ~operations
      ;;

      let slow_unzip_mapi' ?cutoff ?data_equal input ~f =
        let both =
          Incr_map.mapi' ?cutoff ?data_equal input ~f:(fun ~key ~data ->
            let a, b = f ~key ~data in
            Incr.both a b)
        in
        let left = Incr_map.map both ~f:Tuple2.get1 in
        let right = Incr_map.map both ~f:Tuple2.get2 in
        left, right
      ;;

      let%bench_fun ("slow_unzip_mapi' random-ops" [@indexed
                                                     operations = [ 5000; 10000; 100000 ]])
        =
        let operations = test_data ~size:(operations / 100) ~operations in
        fun () -> benchmark_unzip_mapi' slow_unzip_mapi' ~operations
      ;;
    end)
  ;;
end

module Unzip_mapi_prime = struct
  (** version of unzip_mapi' that tests the real implementation against a
      simpler, less incremental one, and fails if the two implementations don't
      match.*)
  let unzip_mapi' ~data_equal ~data_equal_left ~data_equal_right m ~f =
    let a_left, a_right = Incr.Map.unzip_mapi' ~data_equal m ~f in
    let b_left, b_right =
      let paired =
        Incr.Map.mapi' m ~f:(fun ~key ~data ->
          let left, right = f ~key ~data in
          Incr.both left right)
      in
      Incr.Map.map paired ~f:fst, Incr.Map.map paired ~f:snd
    in
    let left =
      let%map a_left = a_left
      and b_left = b_left in
      require [%here] (Map.equal data_equal_left a_left b_left);
      a_left
    in
    let right =
      let%map a_right = a_right
      and b_right = b_right in
      require [%here] (Map.equal data_equal_right a_right b_right);
      a_right
    in
    left, right
  ;;
end

module Prime_test = Make_test (Unzip_mapi_prime)

module Unzip = struct
  let unzip_mapi' ~data_equal ~data_equal_left ~data_equal_right m ~f =
    let m =
      Incr_map.mapi' ~data_equal m ~f:(fun ~key ~data ->
        let a, b = f ~key ~data in
        Incr.both a b)
    in
    let a_left, a_right = Incr.Map.unzip m in
    let b_left, b_right = Incr.Map.map m ~f:fst, Incr.Map.map m ~f:snd in
    let left =
      let%map a_left = a_left
      and b_left = b_left in
      require [%here] (Map.equal data_equal_left a_left b_left);
      a_left
    in
    let right =
      let%map a_right = a_right
      and b_right = b_right in
      require [%here] (Map.equal data_equal_right a_right b_right);
      a_right
    in
    left, right
  ;;
end

module Not_prime_test = Make_test (Unzip)

let%expect_test "unzip_mapi' where only one side of the split is actually used" =
  let m = String.Map.of_alist_exn [ "foo", 3; "bar", 10; "snoo", 5 ] |> Incr.Var.create in
  (* r is intentionally not used *)
  let l, r =
    Incr_map.unzip_mapi' ~data_equal:Int.equal (Incr.Var.watch m) ~f:(fun ~key ~data ->
      let left =
        Incr.map data ~f:(fun data ->
          print_s [%message "computing left" (key : string) (data : int)];
          data)
      and right =
        Incr.map data ~f:(fun data ->
          print_s [%message "computing right" (key : string) (data : int)];
          data)
      in
      left, right)
  in
  let observed_l = Incr.observe l in
  let dump () = Incr.stabilize () in
  let change f =
    Incr.Var.set m (f (Incr.Var.value m));
    dump ()
  in
  dump ();
  [%expect
    {|
    ("computing left"
      (key  snoo)
      (data 5))
    ("computing left"
      (key  foo)
      (data 3))
    ("computing left"
      (key  bar)
      (data 10))
    |}];
  change (fun m -> Map.set m ~key:"foo" ~data:9);
  [%expect {|
    ("computing left"
      (key  foo)
      (data 9))
    |}];
  change (fun m -> Map.set m ~key:"bar" ~data:1);
  [%expect {|
    ("computing left"
      (key  bar)
      (data 1))
    |}];
  change (fun m -> Map.remove m "snoo");
  [%expect {| |}];
  let observed_r = Incr.observe r in
  (* now that [r] is necessary, it should compute the "right" incrementals *)
  Incr.stabilize ();
  [%expect
    {|
    ("computing right"
      (key  foo)
      (data 9))
    ("computing right"
      (key  bar)
      (data 1))
    |}];
  (* Keep [observed_l] and [observed_r] alive by using them here. If this isn't done, then
     there's a chance that a GC would collect the observer, run its finalizer and cause
     all the nodes in the graph to become unobserved *)
  let (_ : _) = Incr.Observer.value observed_l in
  let (_ : _) = Incr.Observer.value observed_r in
  ()
;;
