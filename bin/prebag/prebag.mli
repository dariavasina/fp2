module type Stringable = sig
    type t
    val to_string : t -> string
    val of_string : string -> t
  end
  
  module MakeTrie (S : Stringable) : sig
    type trie
    type elem = S.t
  
    val empty : trie
  
    val add : elem -> trie -> trie
  
    val remove : elem -> trie -> trie option
  
    val count : elem -> trie -> int
  
    val contains : elem -> trie -> bool
  
    val map : (elem -> elem) -> trie -> trie
  
    val fold : (elem -> int -> 'a -> 'a) -> trie -> 'a -> 'a

  
    val filter : (elem -> int -> bool) -> trie -> trie

    val to_list : trie -> (elem * int) list
  end
  