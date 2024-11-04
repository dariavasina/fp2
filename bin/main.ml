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
  
  Printf.printf "Count of 'hello': %d (expected 2)\n" (count "hello" trie);
  Printf.printf "Count of 'world': %d (expected 1)\n" (count "world" trie);
  
  (* Удаление элемента и проверка подсчета *)
  let trie = match remove "hello" trie with Some t -> t | None -> empty_trie in
  Printf.printf "Count of 'hello' after removal: %d (expected 1)\n" (count "hello" trie);
  
  (* Удаление еще раз и проверка удаления *)
  let trie = match remove "hello" trie with Some t -> t | None -> empty_trie in
  Printf.printf "Count of 'hello' after second removal: %d (expected 0)\n" (count "hello" trie);
  
  (* Проверка добавления другого слова и пустого элемента *)
  let trie = add "" trie in
  let trie = add "test" trie in
  Printf.printf "Count of empty string: %d (expected 1)\n" (count "" trie);
  Printf.printf "Count of 'test': %d (expected 1)\n" (count "test" trie);
  
  (* Удаление элемента, которого нет в дереве *)
  let trie = match remove "not_in_trie" trie with Some t -> t | None -> trie in
  Printf.printf "Count of 'not_in_trie' after removal attempt: %d (expected 0)\n" (count "not_in_trie" trie)
