(* open Alcotest *)
open QCheck
open Prebag

module StandardStringMultiset = Map.Make(String)

let add_to_map key map =
  StandardStringMultiset.update key (fun count ->
      Some (1 + Option.value ~default:0 count)
    ) map

let remove_from_map key map =
  StandardStringMultiset.update key (function
      | None -> None
      | Some count -> if count > 1 then Some (count - 1) else None
    ) map

let count_in_map key map =
  Option.value ~default:0 (StandardStringMultiset.find_opt key map)

let map_to_list map =
  StandardStringMultiset.fold (fun key count acc ->
    (key, count) :: acc
  ) map []
  

module StringBag = MakeTrie (struct
  type t = string
  let to_string x = x
  let of_string x = x
end)


module IntBag = MakeTrie(struct
  type t = int
  let to_string = string_of_int
  let of_string = int_of_string
end)

let test_add_int =
  Alcotest.test_case "IntTrie add" `Quick (fun () ->
      let trie = IntBag.empty in
      let trie = IntBag.add 123 trie in
      let trie = IntBag.add 456 trie in
      let trie = IntBag.add 456 trie in
      Alcotest.check Alcotest.int "Count of 123" 1 (IntBag.count 123 trie);
      Alcotest.check Alcotest.int "Count of 456" 2 (IntBag.count 456 trie)
    )

let test_remove_int =
  Alcotest.test_case "IntTrie remove" `Quick (fun () ->
      let trie = IntBag.empty in
      let trie = IntBag.add 123 trie in
      let trie = IntBag.add 456 trie in
      let trie = match IntBag.remove 456 trie with
        | Some updated_trie -> updated_trie
        | None -> IntBag.empty
      in
      Alcotest.check Alcotest.int "Count of 123" 1 (IntBag.count 123 trie);
      Alcotest.check Alcotest.int "Count of 456" 0 (IntBag.count 456 trie)
    )

let test_count_int =
  Alcotest.test_case "IntTrie count" `Quick (fun () ->
      let trie = IntBag.empty in
      let trie = IntBag.add 123 trie in
      let trie = match IntBag.remove 123 trie with
        | Some updated_trie -> updated_trie
        | None -> IntBag.empty
      in
      let trie = IntBag.add 123 trie in
      Alcotest.check Alcotest.int "Count of 123" 1 (IntBag.count 123 trie)
    )

let charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let gen_string =
  let open Gen in
  let gen_char = oneofl (String.to_seq charset |> List.of_seq) in
  string_size ~gen:gen_char (int_range 1 100)

let is_valid_string s =
  String.for_all (fun c -> String.contains charset c) s

let arb_string_trie =
  let rec gen_tree n =
    match n with
    | 0 -> Gen.return StringBag.empty
    | _ ->
      Gen.frequency
        [ 1, Gen.return StringBag.empty
        ; 3, Gen.map2 (fun x t -> StringBag.add x t) gen_string (gen_tree (n - 1))
        ]
  in
  make ~print:(fun _ -> Printf.sprintf "Trie with elements") (gen_tree 10)

let arb_operations =
  let open Gen in
  list_size (int_range 1 10) (oneof [
    map (fun s -> `Insert s) gen_string;
    map (fun s -> `Remove s) gen_string;
  ])

let apply_operations ops bag map =
  let rec aux ops bag map =
    match ops with
    | [] -> bag, map
    | `Insert x :: rest -> 
        aux rest (StringBag.add x bag) (add_to_map x map)
    | `Remove x :: rest -> 
        aux rest (Option.value ~default:StringBag.empty (StringBag.remove x bag))
          (remove_from_map x map)
  in
  aux ops bag map

let compare_pairs (s1, n1) (s2, n2) =
  let c = String.compare s1 s2 in
  if c = 0 then Int.compare n1 n2 else c

let test_operations_property =
  QCheck.Test.make
    ~name:"operations produce same result as built-in multiset"
    (make arb_operations)
    (fun ops ->
      let valid_ops = List.for_all (function
        | `Insert s | `Remove s -> is_valid_string s
      ) ops in

      if not valid_ops then
        false
      else
        let prebag, standardbag = apply_operations ops StringBag.empty StandardStringMultiset.empty in
        let sorted_prebag = List.sort compare_pairs (StringBag.to_list prebag) in 
        let sorted_standardbag = List.sort compare_pairs (map_to_list standardbag) in    
        sorted_prebag = sorted_standardbag)

let prop_associativity =
  QCheck.Test.make
    ~name:"associativity of union"
    (QCheck.triple arb_string_trie arb_string_trie arb_string_trie)
    (fun (trie1, trie2, trie3) ->
        let union1 = StringBag.union (StringBag.union trie1 trie2) trie3 in
        let union2 = StringBag.union trie1 (StringBag.union trie2 trie3) in
        StringBag.equal union1 union2)
    
let prop_monoid_identity =
  Test.make
    ~name:"monoid identity"
    arb_string_trie
    (fun trie ->
        let union_with_empty = StringBag.union trie StringBag.empty in
        let empty_with_union = StringBag.union StringBag.empty trie in
        StringBag.equal union_with_empty trie && StringBag.equal empty_with_union trie)

let run_tests() =
  let open Alcotest in
  run "Trie Tests"
    [ ( "unit_tests"
      , [ test_add_int
        ; test_remove_int
        ; test_count_int
        ] )
    ; ( "property_based"
      , [ QCheck_alcotest.to_alcotest prop_associativity
        ; QCheck_alcotest.to_alcotest prop_monoid_identity
        ; QCheck_alcotest.to_alcotest test_operations_property
        ] )
    ]
  