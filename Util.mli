val mat_map : ('a -> 'b) -> 'a array array -> 'b array array
val mat_reduce : ('a -> 'a -> 'a) -> 'a array array -> 'a
val flatten_array : 'a array array -> 'a array
val string_of_int_mat : int array array -> string
