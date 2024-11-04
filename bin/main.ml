open Prebag

module StringTrie = MakeTrie (struct
  type t = string
  let to_string x = x
  let of_string x = x
end)

let () =
  let open StringTrie in
  let empty_trie = empty in
  
  (* Тесты на добавление и подсчет *)
  let trie = add "hello" empty_trie in
  let trie = add "hello" trie in
  let trie = add "world" trie in
  
  (* Вызов метода to_list и вывод результатов *)
  let result = to_list trie in
  List.iter (fun (key, count) ->
    Printf.printf "%s: %d\n" key count
  ) result