//Michał Klatkowski

//zadanie 1

val suma: List[Double] => Double = xs =>
  if xs == Nil then 0.0
  else xs.head + suma(xs.tail)

def ends[A](xs: List[A]): (A, A) =
  if xs == Nil then throw new NoSuchElementException("empty list")
  else if xs.tail == Nil then (xs.head, xs.head)
  else if xs.tail.tail == Nil then (xs.head, xs.tail.head)
  else ends(xs.head::xs.tail.tail)

ends(List(1, 3, 5, 6, 9)) == (1,9)
ends(List("Ala", "ma", "kota")) == ("Ala", "kota")
ends(List(1)) == (1,1)
ends(Nil)


val posortowana: List[Int] => Boolean = xs =>
  if xs == Nil then true
  else if xs.tail == Nil then true
  else if xs.head <= xs.tail.head then posortowana(xs.tail)
  else false

val glue: (List[String], String) => String = (xs, separator) =>
  if xs == Nil then ""
  else if xs.tail == Nil then xs.head
  else xs.head+separator+glue(xs.tail, separator)

posortowana(List(1,3,3,5,6,7)) == true
posortowana(List(1,3,3,2,6,7)) == false
posortowana(List(7)) == true
posortowana(Nil) == true
posortowana(List(1,3,3,5,7,6)) == false


glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil, "-") == ""
glue(List("To"), ".") == "To"


