val list_sub : 'a list -> 'a list -> 'a list
val array_collect_indices : ('a -> bool) -> 'a array -> int list
val mat_map : ('a -> 'b) -> 'a array array -> 'b array array
val mat_reduce : ('a -> 'a -> 'a) -> 'a array array -> 'a
val flatten_array : 'a array array -> 'a array
val string_of_int_mat : int array array -> string
