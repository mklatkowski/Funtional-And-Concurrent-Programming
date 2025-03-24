import scala.annotation.tailrec
// Michał Klatkowski

//zadanie 1

@tailrec
def existsA[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match
    case h::t => p(h) || existsA(t)(p)
    case Nil => false

existsA (List(5,1,2,3)) (_ == 2)

!existsA (List(5,1,2,3)) (_ == 4)
existsA (List(5,1,2,3)) (_ < 2)
!existsA (Nil) (_ == 2)
existsA (List("Ala", "ma", "kota")) (_ == "ma")

def existsB[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldLeft false) ((acc, elem) => acc || p(elem))

existsB (List(5,1,2,3)) (_ == 2)

!existsB (List(5,1,2,3)) (_ == 4)
existsB (List(5,1,2,3)) (_ < 2)
!existsB (Nil) (_ == 2)
existsB (List("Ala", "ma", "kota")) (_ == "ma")

def existsC[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs foldRight false) ((elem, acc) => p(elem) || acc)

existsC (List(5,1,2,3)) (_ == 2)

!existsC (List(5,1,2,3)) (_ == 4)
existsC (List(5,1,2,3)) (_ < 2)
!existsC (Nil) (_ == 2)
existsC (List("Ala", "ma", "kota")) (_ == "ma")

//zadanie 2

val filterR: [A] => List[A] => (A => Boolean) => List[A] =
  [A] => (xs: List[A]) => (p: A => Boolean) =>
    (xs foldRight List.empty[A]) ((elem, acc) => if p(elem) then elem::acc else acc)

filterR (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filterR (List(2,7,1,3,7,8,4,1,6,9)) (_ > 30) == Nil
filterR(List.empty[Int])(_ > 0) == Nil
filterR(List("Ala", "ma", "kota")) (_ == "ma") == List("ma")


//zadanie 3

val remove1A: [A] => List[A] => (A => Boolean) => List[A] =
  [A] => (xs: List[A]) => (p: A => Boolean) =>
    xs match
      case h :: t => if p(h) then t else h :: remove1A(t)(p)
      case Nil => Nil

remove1A(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)

remove1A(List(1,2,3,2,5)) (_ == 1) == List(2, 3, 2, 5)
remove1A(List(1,2,3,2,5)) (_ == 5) == List(1, 2, 3, 2)
remove1A(List(1,2,3,2,5)) (_ == 6) == List(1, 2, 3, 2, 5)
remove1A(List("Ala", "ma", "ma")) (_ == "ma") == List("Ala", "ma")
remove1A(List.empty[Int]) (_ > 1) == Nil

val remove1B: [A] => List[A] => (A => Boolean) => List[A] =
  [A] => (xs: List[A]) => (p: A => Boolean) =>
    @tailrec
    def remove1BInner(xs: List[A], acc: List[A]): List[A] =
      xs match
        case Nil => acc.reverse
        case h :: t => if p(h) then t.reverse_:::(acc) else remove1BInner(t, h :: acc)
    remove1BInner(xs, Nil)

remove1B(List(1,2,3,2,5)) (_ == 2) == List(1, 3, 2, 5)

remove1B(List(1,2,3,2,5)) (_ == 1) == List(2, 3, 2, 5)
remove1B(List(1,2,3,2,5)) (_ == 5) == List(1, 2, 3, 2)
remove1B(List(1,2,3,2,5)) (_ == 6) == List(1, 2, 3, 2, 5)
remove1B(List("Ala", "ma", "ma")) (_ == "ma") == List("Ala", "ma")
remove1B(List.empty[Int]) (_ > 1) == Nil

//zadanie 4

def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) =
  @tailrec
  def splitAtInner(left: List[A], right: List[A], n: Int): (List[A], List[A]) =
    right match
      case h::t => if n > 0 then splitAtInner(h :: left, t, n-1) else (left.reverse, right)
      case Nil => (left.reverse, right)
  splitAtInner(Nil, xs, n)

splitAt (List('a','b','c','d','e')) (2) == (List('a', 'b'), List('c', 'd', 'e'))

splitAt (List('a','b','c','d','e')) (5) == (List('a', 'b', 'c', 'd', 'e'), Nil)
splitAt (List('a','b','c','d','e')) (7) == (List('a', 'b', 'c', 'd', 'e'), Nil)
splitAt (List('a','b','c','d','e')) (-1) == (Nil, List('a', 'b', 'c', 'd', 'e'))
splitAt (List('a','b','c','d','e')) (0) == (Nil, List('a', 'b', 'c', 'd', 'e'))
splitAt (Nil) (4) == (Nil, Nil)
splitAt (List(1,2,3)) (1) == (List(1), List(2,3))
