import scala.annotation.tailrec
// Michał Klatkowski

//zadanie 1

def take[A](n: Int, xs: List[A]): List[A] =
  xs match
    case h::t => if n>0 then h :: take(n-1, t) else Nil
    case Nil => Nil


take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)

take(0, List(1,2,3)) == Nil
take(0, Nil) == Nil
take(2, Nil) == Nil
take(-4, Nil) == Nil

//zadanie 2

@tailrec
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match
    case _::t => if n > 0 then drop(n-1, t) else xs
    case Nil => xs

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil

drop(0, List(1,2,3)) == List(1,2,3)
drop(0, Nil) == Nil
drop(2, Nil) == Nil
drop(-4, Nil) == Nil

//zadanie 3

def reverse[A](xs: List[A]): List[A] =
  @tailrec
  def reverseInner(xs: List[A], acc: List[A]): List[A] =
    xs match
      case h::t => reverseInner(t, h::acc)
      case Nil => acc
  reverseInner(xs, Nil)


reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")

reverse(List("Ala")) == List("Ala")
reverse(Nil) == Nil
reverse(List(1,2,3)) == List(3,2,1)

//zadanie 4

val replicate: List[Int] => List[Int] = xs =>
  def replicateInner(elem: Int, n: Int, xs: List[Int]): List[Int] =
    if n > 0 then elem :: replicateInner(elem, n-1, xs)
    else
      xs match
        case h::t => replicateInner(h, h, t)
        case Nil => Nil
  replicateInner(0, 0, xs)

replicate (List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)

replicate(Nil) == Nil
replicate(List(-2,-3)) == Nil
replicate(List(0)) == Nil

//zadanie 5

val root3: Double => Double = a =>
  @tailrec
  def root3Inner(xi: Double): Double =
    if Math.abs(xi*xi*xi - a) <= 1.0e-15 * Math.abs(a) then xi
    else root3Inner(xi + ((a/(xi*xi)) - xi)/3 )
  root3Inner(if a>1 then a/3 else a)


math.abs(root3(-8.0) + 2.0) <= 1.0e-70

math.abs(root3(-1000.0) + 10.0) <= 1.0e-70
math.abs(root3(0) + 0.0) <= 1.0e-70
math.abs(root3(64) - 4.0) <= 1.0e-70