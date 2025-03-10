//Michał Klatkowski

//zadanie 1

val suma: List[Double] => Double = xs =>
  if xs == Nil then 0.0
  else xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

//zadanie 2

def ends[A](xs: List[A]): (A, A) =
  def last[A](xs: List[A]): A =
    if xs.tail == Nil then xs.head
    else last(xs.tail)
  if xs == Nil then throw new NoSuchElementException("empty list")
  else (xs.head, last(xs))

ends(List(1, 3, 5, 6, 9)) == (1,9)
ends(List("Ala", "ma", "kota")) == ("Ala", "kota")
ends(List(1)) == (1,1)
ends(Nil)

//zadanie 3

val posortowana: List[Int] => Boolean = xs =>
  if xs == Nil || xs.tail == Nil then true
  else xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(List(1,3,3,5,6,7))

posortowana(Nil)
posortowana(List(-5))
posortowana(List(1,1,1,1))
!posortowana(List(1,2,2,2,1,5))
!posortowana(List(1,2,3,2))
!posortowana(List(6,5,4))

//zadanie 4

val glue: (List[String], String) => String = (xs, separator) =>
  if xs == Nil then ""
  else if xs.tail == Nil then xs.head
  else s"${xs.head}${separator}${glue(xs.tail, separator)}"


glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil, "-") == ""

glue(List("Test"), ".") == "Test"
glue(List("Test", "pierwszy"), "+") == "Test+pierwszy"
