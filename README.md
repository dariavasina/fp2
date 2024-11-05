## Prefix-tree bag

### Васина Дарья 367131

`Prebag` - полиморфная структура, реализующая интерфейс multiset.
Модуль `MakeTrie` создает префиксное дерево для произвольного типа данных, удовлетворяющего интерфейсу `Stringable`

```ocaml
module type Stringable = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end
```

### Основные операции

- Создание пустого дерева\*\*:

  ```ocaml
  let empty = Node (0, [])
  ```

- Функция `add` добавляет элемент в дерево, обновляя счетчики узлов:

  ```ocaml
  let add (elem : S.t) (trie : trie) : trie
  ```

- Функция `remove` удаляет элемент, уменьшая счетчики в узлах или удаляя узел, если счетчик становится равен нулю:

  ```ocaml
  let remove (elem : S.t) (trie : trie) : trie option
  ```

- Функция `count` возвращает количество вхождений элемента:

  ```ocaml
  let count (elem : S.t) (trie : trie) : int
  ```

- Функция `contains` проверяет, существует ли элемент в дереве:

  ```ocaml
  let contains (elem : S.t) (trie : trie) : bool
  ```

### Операции с деревьями

- Функция `union` объединяет два дерева, складывая счетчики и объединяя общие узлы:

```ocaml
let union (trie1 : trie) (trie2 : trie) : trie
```

- Функция `equal` проверяет, эквивалентны ли два дерева:

```ocaml
let equal (trie1 : trie) (trie2 : trie) : bool
```

- Функция `map` применяет функцию к каждому элементу и возвращает новое дерево:

```ocaml
let map (f : S.t -> S.t) (trie : trie) : trie
```

- Функция `fold` выполняет свёртку дерева, обрабатывая каждый элемент с накоплением результата:

```ocaml
let fold (f : S.t -> int -> 'a -> 'a) (trie : trie) (acc : 'a) : 'a
```

- Функция `filter` возвращает новое дерево, содержащее только элементы, удовлетворяющие предикату:

```ocaml
let filter (pred : S.t -> int -> bool) (trie : trie) : trie
```

- Функция `to_list` преобразует дерево в список пар `(элемент, количество)`:

```ocaml
let to_list (trie : trie) : (S.t * int) list
```
