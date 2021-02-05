(*
	Huffman 
*)

datatype Arbol = H of char * real | N of real * Arbol * Arbol

val l =
	"aaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\
	\cccccccccccccccddddddddeeeeeeeeeeeeeeeeeeeeeeeee"

fun cmp(H(_, p1), H(_, p2)) = p1<=p2
| cmp(H(_, p1), N(p2, _, _)) = p1<=p2
| cmp(N(p1, _, _), H(_, p2)) = p1<=p2
| cmp(N(p1, _, _), N(p2, _, _)) = p1<=p2

fun qsort _ [] = []
| qsort c (h::t) =
	let	val (m, M)= List.partition (fn x => c(x, h)) t
	in	(qsort c m)@[h]@(qsort c M) end

fun prob(H(_, p1)) = p1
| prob(N(p2, _, _)) = p2

fun huftree [] = raise Empty
fun huftree [x] = x
| huftree l =
	let	val x::y::t = qsort cmp l
		val p = prob(x)+prob(y)
	in	huftree(N(p, x, y)::t) end

fun recorre (H(c, _)) p = [(c, p)]
| recorre (N(_, i, d)) p =
	(recorre i (p^"0"))@(recorre d (p^"1"))

fun cuenta s =
	let	val a = Polyhash.mkPolyTable(size s, Empty)
		fun inc c = Polyhash.insert a (c,
			case Polyhash.peek a c of
			SOME n => n+1 | NONE => 1)
		fun frecA t (c, n) = H(c, real(n)/real(t))
	in
		map inc (explode s);
		map (frecA (size s)) (Polyhash.listItems a)
	 end

fun huffman s = recorre(huftree(cuenta s)) ""
