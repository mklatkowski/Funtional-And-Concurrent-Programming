def suma(xs: List[Double]): Double =
  if xs == Nil then 0.0
  else xs.head + suma(xs.tail)

def ends[A](xs: List[A]): (A, A) =
  if xs == Nil then throw new NoSuchElementException("empty list")
  else
  else

ends(List(1, 3, 5, 6, 9)) == (1,9)
ends(List("Ala", "ma", "kota")) == ("Ala", "kota")
ends(List(1)) == (1,1)
ends(Nil)