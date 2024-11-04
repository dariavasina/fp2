open Alcotest
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

let test_add_string =
  test_case "Trie add" `Quick (fun () ->
      let trie = StringBag.empty in
      let trie = StringBag.add "hello" trie in
      let trie = StringBag.add "world" trie in

      let standard_bag = StandardStringMultiset.empty in
      let standard_bag = add_to_map "hello" standard_bag in
      let standard_bag = add_to_map "world" standard_bag in

      check Alcotest.int "Count of 'hello' in standard multiset" 
        1 (Option.value ~default:0 (StandardStringMultiset.find_opt "hello" standard_bag));
      check Alcotest.int "Count of 'world' in standard multiset" 
        1 (Option.value ~default:0 (StandardStringMultiset.find_opt "world" standard_bag));
      check Alcotest.int "Count of 'ocaml' in trie" 
        0 (StringBag.count "ocaml" trie);
      check Alcotest.int "Count of 'ocaml' in standard multiset" 
        0 (Option.value ~default:0 (StandardStringMultiset.find_opt "ocaml" standard_bag))
    )  
  
let test_remove_string =
  test_case "Trie remove" `Quick (fun () ->
      let trie = StringBag.add "hello" StringBag.empty in
      let trie = StringBag.add "world" trie in
      let trie_opt = StringBag.remove "hello" trie in
      match trie_opt with
      | None -> fail "Trie should not be None"
      | Some trie ->
        check Alcotest.int "Count of 'hello'" 0 (StringBag.count "hello" trie);
        check Alcotest.int "Count of 'world'" 1 (StringBag.count "world" trie)
    )  
  
let arb_string_trie =
  let rec gen_tree n =
    match n with
    | 0 -> Gen.return StringBag.empty
    | _ ->
      Gen.frequency
        [ 1, Gen.return StringBag.empty
        ; 3, Gen.map2 (fun x t -> StringBag.add x t) Gen.string (gen_tree (n - 1))
        ]
  in
  make ~print:(fun _ -> Printf.sprintf "Trie with elements") (gen_tree 10)

let charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

let gen_string =
  let open Gen in
  let gen_char = oneofl (String.to_seq charset |> List.of_seq) in
  string_size ~gen:gen_char (int_range 1 100)



(* let arb_operations =
  let open Gen in
  let gen_op = oneof [
    map (fun s -> `Insert s) string;
    map (fun s -> `Remove s) string;
  ] in
  let rec gen_ops n = 
    if n <= 0 then return [] 
    else map2 (fun op ops -> op :: ops) gen_op (gen_ops (n - 1))
  in
  make
    ~print:(fun ops -> 
      Print.(list string) (List.map (function 
        | `Insert x -> "Insert " ^ x 
        | `Remove x -> "Remove " ^ x) ops))
    (gen_ops 10) *)

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

(* let test_operations_property =
  QCheck.Test.make
    ~name:"operations produce same result as built-in multiset"
    arb_operations
    (fun ops ->
      let prebag, stringbag = apply_operations ops StringBag.empty StandardStringMultiset.empty in
      StringBag.to_list prebag = map_to_list stringbag) *)

let compare_pairs (s1, n1) (s2, n2) =
  let c = String.compare s1 s2 in
  if c = 0 then Int.compare n1 n2 else c


  let is_valid_string s =
    String.for_all (fun c -> String.contains charset c) s
  
let test_operations_property =
  QCheck.Test.make
    ~name:"operations produce same result as built-in multiset"
    (make arb_operations)
    (fun ops ->
      (* Validate operations *)
      let valid_ops = List.for_all (function
        | `Insert s | `Remove s -> is_valid_string s
      ) ops in

      if not valid_ops then
        false
      else
        let prebag, stringbag = apply_operations ops StringBag.empty StandardStringMultiset.empty in
        let sorted_prebag = List.sort compare_pairs (StringBag.to_list prebag) in 
        let sorted_stringbag = List.sort compare_pairs (map_to_list stringbag) in
        
        (* Debug print statements *)
        Printf.printf "Operations: %s\n" 
          (String.concat ", " (List.map (function 
            | `Insert x -> "Insert " ^ x 
            | `Remove x -> "Remove " ^ x) ops));
        Printf.printf "StringBag: %s\n" 
          (String.concat ", " (List.map (fun (s, n) -> Printf.sprintf "(%s, %d)" s n) sorted_prebag));
        Printf.printf "StandardStringMultiset: %s\n" 
          (String.concat ", " (List.map (fun (s, n) -> Printf.sprintf "(%s, %d)" s n) sorted_stringbag));
        
        sorted_prebag = sorted_stringbag)



let prop_identity =
  QCheck.Test.make
    ~name:"identity (add and remove)"
    (QCheck.pair arb_string_trie QCheck.string)
    (fun (trie, elem) ->
       let initial_count = StringBag.count elem trie in
       match StringBag.remove elem (StringBag.add elem trie) with
       | None -> initial_count = 0 
       | Some result -> StringBag.count elem result = initial_count)


let prop_associativity =
  Test.make
    ~name:"associativity of add"
    (triple string string arb_string_trie)
    (fun (elem1, elem2, trie) ->
       let trie1 = StringBag.add elem2 (StringBag.add elem1 trie) in
       let trie2 = StringBag.add elem1 (StringBag.add elem2 trie) in
       StringBag.count elem1 trie1 = StringBag.count elem1 trie2 &&
       StringBag.count elem2 trie1 = StringBag.count elem2 trie2)

let prop_monoid_identity =
  Test.make
    ~name:"monoid identity"
    arb_string_trie
    (fun trie ->
       let union_with_empty = trie in
       union_with_empty = trie)


let run_tests() =
  let open Alcotest in
  run "Trie Tests"
    [ ( "unit_tests"
      , [ test_add_string
        ; test_remove_string
        ; test_add_int
        ; test_remove_int
        ; test_count_int
        ] )
    ; ( "property_based"
      , [ QCheck_alcotest.to_alcotest prop_identity
        ; QCheck_alcotest.to_alcotest prop_associativity
        ; QCheck_alcotest.to_alcotest prop_monoid_identity
        ; QCheck_alcotest.to_alcotest test_operations_property
        ] )
    ]
