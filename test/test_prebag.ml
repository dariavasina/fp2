open Alcotest
open QCheck
open Prebag


module Trie = MakeTrie (struct
  type t = string
  let to_string x = x
  let of_string x = x
end)



let test_trie_add =
  test_case "Trie add" `Quick (fun () ->
      let trie = Trie.empty in
      let trie = Trie.add "hello" trie in
      let trie = Trie.add "world" trie in
      check Alcotest.int "Count of 'hello'" 1 (Trie.count "hello" trie);
      check Alcotest.int "Count of 'world'" 1 (Trie.count "world" trie);
      check Alcotest.int "Count of 'ocaml'" 0 (Trie.count "ocaml" trie))

let test_trie_remove =
  test_case "Trie remove" `Quick (fun () ->
      let trie = Trie.add "hello" Trie.empty in
      let trie = Trie.add "world" trie in
      let trie_opt = Trie.remove "hello" trie in
      match trie_opt with
      | None -> fail "Trie should not be None"
      | Some trie ->
        check Alcotest.int "Count of 'hello'" 0 (Trie.count "hello" trie);
        check Alcotest.int "Count of 'world'" 1 (Trie.count "world" trie))

(* Property-based Tests *)

let arb_string_trie =
  let rec gen_tree n =
    match n with
    | 0 -> Gen.return Trie.empty
    | _ ->
      Gen.frequency
        [ 1, Gen.return Trie.empty
        ; 3, Gen.map2 (fun x t -> Trie.add x t) Gen.string (gen_tree (n - 1))
        ]
  in
  make ~print:(fun _ -> Printf.sprintf "Trie with elements") (gen_tree 10)

let prop_identity =
  QCheck.Test.make
    ~name:"identity (add and remove)"
    (QCheck.pair arb_string_trie QCheck.string)
    (fun (trie, elem) ->
       let initial_count = Trie.count elem trie in
       match Trie.remove elem (Trie.add elem trie) with
       | None -> initial_count = 0 
       | Some result -> Trie.count elem result = initial_count)


let prop_associativity =
  Test.make
    ~name:"associativity of add"
    (triple string string arb_string_trie)
    (fun (elem1, elem2, trie) ->
       let trie1 = Trie.add elem2 (Trie.add elem1 trie) in
       let trie2 = Trie.add elem1 (Trie.add elem2 trie) in
       Trie.count elem1 trie1 = Trie.count elem1 trie2 &&
       Trie.count elem2 trie1 = Trie.count elem2 trie2)

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
      , [ test_trie_add
        ; test_trie_remove
        ] )
    ; ( "property_based"
      , [ QCheck_alcotest.to_alcotest prop_identity
        ; QCheck_alcotest.to_alcotest prop_associativity
        ; QCheck_alcotest.to_alcotest prop_monoid_identity
        ] )
    ]
