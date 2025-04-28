open Core
open Import

module type S = sig
  val unzip3_mapi'
    :  data_equal:('d -> 'd -> bool)
    -> data_equal_left:('a -> 'a -> bool)
    -> data_equal_middle:('b -> 'b -> bool)
    -> data_equal_right:('c -> 'c -> bool)
    -> ('k, 'd, 'e) Map.t Incr.t
    -> f:(key:'k -> data:'d Incr.Z.t -> 'a Incr.t * 'b Incr.t * 'c Incr.t)
    -> ('k, 'a, 'e) Map.t Incr.t * ('k, 'b, 'e) Map.t Incr.t * ('k, 'c, 'e) Map.t Incr.t
end

module Make_test (S : S) = struct
  let%expect_test "simple unzip_mapi'" =
    let var =
      String.Map.of_alist_exn [ "foo", 3; "bar", 10; "snoo", 5 ] |> Incr.Var.create
    in
    let l, m, r =
      S.unzip3_mapi'
        ~data_equal:Int.equal
        ~data_equal_left:(Option.equal Int.equal)
        ~data_equal_middle:String.equal
        ~data_equal_right:Int.equal
        (Incr.Var.watch var)
        ~f:(fun ~key:_ ~data:x ->
          ( (let%map x in
             let y = x * x in
             if y > 10 then Some y else None)
          , Incr.map x ~f:Int.to_string
          , x ))
    in
    let obs =
      (let%map l and m and r in
       l, m, r)
      |> Incr.observe
    in
    let dump () =
      Incr.stabilize ();
      [%sexp_of:
        int String.Map.t
        * string
        * (int option String.Map.t * string String.Map.t * int String.Map.t)]
        (Incr.Var.value var, "->", Incr.Observer.value_exn obs)
      |> Sexp.to_string_hum
      |> print_endline
    in
    let change f =
      Incr.Var.set var (f (Incr.Var.value var));
      dump ()
    in
    dump ();
    [%expect
      {|
      (((bar 10) (foo 3) (snoo 5)) ->
       (((bar (100)) (foo ()) (snoo (25))) ((bar 10) (foo 3) (snoo 5))
        ((bar 10) (foo 3) (snoo 5))))
      |}];
    change (fun m -> Map.set m ~key:"foo" ~data:9);
    [%expect
      {|
      (((bar 10) (foo 9) (snoo 5)) ->
       (((bar (100)) (foo (81)) (snoo (25))) ((bar 10) (foo 9) (snoo 5))
        ((bar 10) (foo 9) (snoo 5))))
      |}];
    change (fun m -> Map.set m ~key:"bar" ~data:1);
    [%expect
      {|
      (((bar 1) (foo 9) (snoo 5)) ->
       (((bar ()) (foo (81)) (snoo (25))) ((bar 1) (foo 9) (snoo 5))
        ((bar 1) (foo 9) (snoo 5))))
      |}];
    change (fun m -> Map.remove m "snoo");
    [%expect
      {|
      (((bar 1) (foo 9)) ->
       (((bar ()) (foo (81))) ((bar 1) (foo 9)) ((bar 1) (foo 9))))
      |}]
  ;;

  let%test_unit "unzip_mapi' randomised fuzz test" =
    Quickcheck.test
      ~sexp_of:[%sexp_of: (int, int) Map_operations.t list]
      (Map_operations.quickcheck_generator Int.quickcheck_generator)
      ~f:(fun operations ->
        let var = Incr.Var.create Int.Map.empty in
        let f ~key ~data =
          ( (let%map data in
             let y = data * data in
             Option.some_if (key + y > 33) y)
          , data >>| Int.to_string
          , data )
        in
        let incr_left, incr_middle, incr_right =
          S.unzip3_mapi'
            (Incr.Var.watch var)
            ~data_equal:[%equal: int]
            ~data_equal_left:[%equal: int option]
            ~data_equal_middle:[%equal: string]
            ~data_equal_right:[%equal: int]
            ~f
        and slow_left, slow_middle, slow_right =
          let paired =
            Incr.Map.mapi' (Incr.Var.watch var) ~f:(fun ~key ~data ->
              let left, middle, right = f ~key ~data in
              let%map left and middle and right in
              left, middle, right)
          in
          ( Incr.Map.map paired ~f:Tuple3.get1
          , Incr.Map.map paired ~f:Tuple3.get2
          , Incr.Map.map paired ~f:Tuple3.get3 )
        in
        let incr_left = Incr.observe incr_left
        and incr_middle = Incr.observe incr_middle
        and incr_right = Incr.observe incr_right
        and slow_left = Incr.observe slow_left
        and slow_middle = Incr.observe slow_middle
        and slow_right = Incr.observe slow_right in
        Map_operations.run_operations operations ~into:var ~after_stabilize:(fun () ->
          [%test_result: int option Int.Map.t]
            ~expect:(Incr.Observer.value_exn slow_left)
            (Incr.Observer.value_exn incr_left);
          [%test_result: string Int.Map.t]
            ~expect:(Incr.Observer.value_exn slow_middle)
            (Incr.Observer.value_exn incr_middle);
          [%test_result: int Int.Map.t]
            ~expect:(Incr.Observer.value_exn slow_right)
            (Incr.Observer.value_exn incr_right));
        Incr.Observer.disallow_future_use incr_left;
        Incr.Observer.disallow_future_use incr_middle;
        Incr.Observer.disallow_future_use incr_right;
        Incr.Observer.disallow_future_use slow_left;
        Incr.Observer.disallow_future_use slow_middle;
        Incr.Observer.disallow_future_use slow_right)
  ;;
end

module Unzip_mapi_prime = struct
  (** version of unzip_mapi' that tests the real implementation against a simpler, less
      incremental one, and fails if the two implementations don't match. *)
  let unzip3_mapi' ~data_equal ~data_equal_left ~data_equal_middle ~data_equal_right m ~f =
    let a_left, a_middle, a_right = Incr.Map.unzip3_mapi' ~data_equal m ~f in
    let b_left, b_middle, b_right =
      let paired =
        Incr.Map.mapi' m ~f:(fun ~key ~data ->
          let left, middle, right = f ~key ~data in
          let%map left and middle and right in
          left, middle, right)
      in
      ( Incr.Map.map paired ~f:Tuple3.get1
      , Incr.Map.map paired ~f:Tuple3.get2
      , Incr.Map.map paired ~f:Tuple3.get3 )
    in
    let left =
      let%map a_left and b_left in
      require (Map.equal data_equal_left a_left b_left);
      a_left
    in
    let middle =
      let%map a_middle and b_middle in
      require (Map.equal data_equal_middle a_middle b_middle);
      a_middle
    in
    let right =
      let%map a_right and b_right in
      require (Map.equal data_equal_right a_right b_right);
      a_right
    in
    left, middle, right
  ;;
end

module Prime_test = Make_test (Unzip_mapi_prime)
