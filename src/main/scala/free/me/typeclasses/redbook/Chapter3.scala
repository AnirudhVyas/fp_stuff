package free.me.typeclasses.redbook
object Chapter3 extends App {
  // pattern matching / ugly way
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case _ :: t => t
  }
  def setHead[A](head: A, xs: List[A]): List[A] = xs match {
    case Nil => head :: Nil
    case _ :: t => head :: t
  }
  @scala.annotation.tailrec
  def drop[A](xs: List[A], n: Int): List[A] = if (n < 0) xs else drop(tail(xs), n - 1)
  // remove head if f(a) is satisfied.
  def conditionalTail[A](xs: List[A], f: A => Boolean) = xs match {
    case Nil => Nil
    case h :: t if f(h) => t
  }
  @scala.annotation.tailrec
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = {
    dropWhile(conditionalTail(xs, f), f)
  }
  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case h :: t => h :: append(t, l2)
  }
  // 3.15
  def concat[A](xs: List[List[A]]) = xs.foldLeft(List.empty[A]) { (acc, l) =>
    acc match {
      case Nil => l
      case h :: t => h :: t ++ l
    }
  }
}
