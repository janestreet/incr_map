open! Core
open Import

module M (M : sig
    val n : int
    val left_changes : int
    val right_changes : int
  end) =
struct
  let m_0 = Int.Map.of_alist_exn (List.init M.n ~f:(fun x -> x, x))

  (* Change 100 elements *)
  let negate_k_elements k =
    List.init k ~f:(fun x -> x * (M.n / k))
    |> List.fold ~init:m_0 ~f:(fun m x -> Map.change m x ~f:(Option.map ~f:(fun x -> -x)))
  ;;

  let m_left = negate_k_elements M.left_changes
  let m_right = negate_k_elements M.right_changes
  let suffix = sprintf "(%d, %d)" M.left_changes M.right_changes

  module%bench [@name_suffix suffix] _ = struct
    let%bench_fun "merge-sum" =
      let left = Var.create m_0 in
      let right = Var.create m_0 in
      let sum =
        Incr_map.merge (Var.watch left) (Var.watch right) ~f:(fun ~key:_ -> function
          | `Left x | `Right x -> Some x
          | `Both (x, y) -> Some (x + y))
      in
      let sum = Incr.observe sum in
      fun () ->
        Var.set left m_left;
        Var.set right m_right;
        Incr.stabilize ();
        ignore (Obs.value_exn sum : int Int.Map.t);
        Var.set left m_0;
        Var.set right m_0;
        Incr.stabilize ();
        ignore (Obs.value_exn sum : int Int.Map.t)
    ;;
  end
end

module _ = M (struct
    let n = 10_000
    let left_changes = 0
    let right_changes = 100
  end)

module _ = M (struct
    let n = 10_000
    let left_changes = 1
    let right_changes = 1_000
  end)

module _ = M (struct
    let n = 10_000
    let left_changes = 100
    let right_changes = 0
  end)

module _ = M (struct
    let n = 10_000
    let left_changes = 100
    let right_changes = 100
  end)

module _ = M (struct
    let n = 10_000
    let left_changes = 1_000
    let right_changes = 1
  end)

module _ = M (struct
    let n = 10_000
    let left_changes = 10_000
    let right_changes = 10_000
  end)
