def compose[X, Y, Z](f: X ⇒ Y, g: Y ⇒ Z): X ⇒ Z = x ⇒ g(f(x))

val a: Int ⇒ Boolean = x ⇒ x % 2 == 0
val b: Boolean ⇒ String = b ⇒ b.toString

val c = compose(a, b)

c(12)
c(11)


