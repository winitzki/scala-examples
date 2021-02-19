package example

object Chapter02_new_examples {
  def parenthesesAreBalanced(s: String): Boolean = {
    val brackets = Map(')' -> '(', ']' -> '[', '}' -> '{')
    val result: (Boolean, List[Char]) = s.toCharArray.foldLeft((true, List[Char]())) { case ((prev, stack), c) => if (prev) {
      if (brackets.values.toSet contains c) (true, c +: stack)
      else if (brackets.keys.toSet contains c) {
        if (stack.headOption contains brackets(c))
          (true, stack.tail)
        else (false, Nil)
      } else (true, stack)
    } else (false, Nil)
    }
    result._1 && result._2.isEmpty
  }
  assert(parenthesesAreBalanced("()"))
  assert(parenthesesAreBalanced("[()]"))
  assert(parenthesesAreBalanced("{[()]}"))
  assert(parenthesesAreBalanced("([{{[(())]}}])"))
  assert(!parenthesesAreBalanced("{{[]()}}}}"))
  assert(!parenthesesAreBalanced("{{[](A}}}}"))
  assert(!parenthesesAreBalanced("{[(])}"))
}
