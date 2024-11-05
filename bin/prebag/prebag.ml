module type Stringable = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end

module MakeTrie (S : Stringable) = struct
  type elem = S.t
  type trie = Node of int * (char * trie) list
  let empty = Node (0, [])

  let add (elem : S.t) (trie : trie) : trie =
    let rec aux chars node =
      match chars, node with
      | [], Node (count, children) -> Node (count + 1, children)
      | c :: rest, Node (count, children) ->
        let updated_children =
          match List.partition (fun (ch, _) -> ch = c) children with
          | ([], others) -> (c, aux rest (Node (0, []))) :: others
          | ((_, child) :: _, others) -> (c, aux rest child) :: others
        in
        Node (count, updated_children)
    in
    aux (List.of_seq (String.to_seq (S.to_string elem))) trie

  let remove (elem : S.t) (trie : trie) : trie option =
    let rec aux chars node =
      match chars, node with
      | [], Node (count, children) when count > 0 -> Some (Node (count - 1, children))
      | [], Node (_, _) -> None
      | c :: rest, Node (count, children) ->
        let updated_children =
          match List.partition (fun (ch, _) -> ch = c) children with
          | ([], _) -> children
          | ((_, child) :: _, others) ->
            (match aux rest child with
             | None -> others
             | Some new_child -> (c, new_child) :: others)
        in
        Some (Node (count, updated_children))
    in
    aux (List.of_seq (String.to_seq (S.to_string elem))) trie

  let count (elem : S.t) (trie : trie) : int =
    let rec aux chars node =
      match chars, node with
      | [], Node (count, _) -> count
      | c :: rest, Node (_, children) ->
        (match List.find_opt (fun (ch, _) -> ch = c) children with
         | Some (_, child) -> aux rest child
         | None -> 0)
    in
    aux (List.of_seq (String.to_seq (S.to_string elem))) trie

  let contains (elem : S.t) (trie : trie) : bool =
    count elem trie > 0
  let union (trie1 : trie) (trie2 : trie) : trie =
    let rec aux (Node (count1, children1)) (Node (count2, children2)) =
      let merged_count = count1 + count2 in
      let merged_children =
        List.fold_left
          (fun acc (c, child2) ->
              match List.partition (fun (ch, _) -> ch = c) acc with
              | ([], others) -> (c, child2) :: others
              | ((_, child1) :: _, others) -> (c, aux child1 child2) :: others)
          children1
          children2
      in
      Node (merged_count, merged_children)
    in
    aux trie1 trie2

  let equal (trie1 : trie) (trie2 : trie) : bool =
    let rec aux (Node (count1, children1)) (Node (count2, children2)) =
      count1 = count2 &&
      List.length children1 = List.length children2 &&
      List.for_all
        (fun (c1, child1) ->
          match List.find_opt (fun (c2, _) -> c1 = c2) children2 with
          | Some (_, child2) -> aux child1 child2
          | None -> false)
        children1
    in
    aux trie1 trie2
  let map (f : S.t -> S.t) (trie : trie) : trie =
    let rec aux prefix (Node (count, children)) acc =
      let element = S.of_string (List.fold_left (fun acc c -> acc ^ String.make 1 c) "" prefix) in
      let new_elem = f element in
      let updated_trie = if count > 0 then add new_elem acc else acc in
      List.fold_left
        (fun acc (c, child) -> aux (prefix @ [c]) child acc)
        updated_trie
        children
    in
    aux [] trie empty

  let fold (f : S.t -> int -> 'a -> 'a) (trie : trie) (acc : 'a) : 'a =
    let rec aux prefix (Node (count, children)) acc =
      let element = S.of_string (List.fold_left (fun acc c -> acc ^ String.make 1 c) "" prefix) in
      let acc = if count > 0 then f element count acc else acc in
      List.fold_left (fun acc (c, child) -> aux (prefix @ [c]) child acc) acc children
    in
    aux [] trie acc

  let filter (pred : S.t -> int -> bool) (trie : trie) : trie =
    let rec aux prefix (Node (count, children)) acc =
      let element = S.of_string (List.fold_left (fun acc c -> acc ^ String.make 1 c) "" prefix) in
      let updated_trie = if count > 0 && pred element count then add element acc else acc in
      List.fold_left
        (fun acc (c, child) -> aux (prefix @ [c]) child acc)
        updated_trie
        children
    in
    aux [] trie empty

    let to_list (trie : trie) : (S.t * int) list =
      let rec aux prefix (Node (count, children)) acc =
        let element = S.of_string (List.fold_left (fun acc c -> acc ^ String.make 1 c) "" prefix) in
        let acc = if count > 0 then (element, count) :: acc else acc in
        List.fold_left (fun acc (c, child) -> aux (prefix @ [c]) child acc) acc children
      in
      aux [] trie []
      
end

module Prebag (S : Stringable) = MakeTrie(S)