package u04lab.code

sealed trait List[A] {

  def head: Option[A]

  def tail: Option[List[A]]

  def append(list: List[A]): List[A]

  def foreach(consumer: (A) => Unit): Unit

  def get(pos: Int): Option[A]

  def filter(predicate: (A) => Boolean): List[A]

  def map[B](fun: (A) => B): List[B]

  def toSeq: Seq[A]

  def flatMap[B](f: A => List[B]): List[B]

  def zipRight: List[(A,Int)]

  // right-associative construction: 10 :: 20 :: 30 :: Nil()
  def ::(head: A): List[A] = Cons(head,this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
  extends ListImplementation[A]

case class Nil[A]()
  extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
  def unapply[A](l: List[A]): Option[(A,List[A])] = l match {
    case Cons(h,t) => Some((h,t))
    case _ => None
  }
}

// List algorithms
trait ListImplementation[A] extends List[A] {

  override def head: Option[A] = this match {
    case h :: t => Some(h)
    case _ => None
  }
  override def tail: Option[List[A]] = this match {
    case h :: t => Some(t)
    case _ => None
  }
  override def append(list: List[A]): List[A] = this match {
    case h :: t => h :: (t append list)
    case _ => list
  }
  override def foreach(consumer: (A)=>Unit): Unit = this match {
    case h :: t => {consumer(h); t foreach consumer}
    case _ => None
  }
  override def get(pos: Int): Option[A] = this match {
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t get (pos-1)
    case _ => None
  }
  override def filter(predicate: (A) => Boolean): List[A] = this match {
    case h :: t if (predicate(h)) => h :: (t filter predicate)
    case _ :: t => (t filter predicate)
    case _ => Nil()
  }
  override def map[B](fun: (A) => B): List[B] = this match {
    case h :: t => fun(h) :: (t map fun)
    case _ => Nil()
  }

  override def toSeq: Seq[A] = this match {
    case h :: t => h +: t.toSeq // using method '+:' in Seq..
    case _ => Seq()
  }

  override def zipRight: List[(A,Int)] = ??? // questions: what is the type of keyword ???

  override def flatMap[B](f: A => List[B]): List[B] = ???
}

// Factories
object List {

  def apply[A](elems: A*): List[A] = {
    var list: List[A] = Nil()
    for (i <- elems.length-1 to 0 by -1) list = elems(i) :: list
    list
  }

  def of[A](elem: A, n: Int): List[A] =
    if (n==0) Nil() else elem :: of(elem,n-1)
}

object Test extends App {

  import List._  // Working with the above lists
  println(List(10,20,30,40))
  val l = 10 :: 20 :: 30 :: 40 :: Nil() // same as above
  println(l.head) // 10
  println(l.tail) // 20,30,40
  println(l append l) // 10,20,30,40,10,20,30,40
  println(l append l toSeq) // as a list: 10,20,30,40,10,20,30,40
  println(l get 2) // 30
  println(of("a",10)) // a,a,a,..,a
  println(l filter (_<=20) map ("a"+_) ) // a10, a20
  // The code below raises a scala.NotImplementedError until you implement flatMap and zipRight
  println(l flatMap(i => i+"a" :: i+"b" :: Nil()) toSeq) // List(10a, 10b, 20a, 20b, 30a, 30b, 40a, 40b)
  println(l flatMap(i => i+1 :: Nil()) toSeq) // List(11, 21, 31, 41)
  println(l flatMap(i => of(i,i/10)) toSeq) // List(10, 20, 20, 30, 30, 30, 40, 40, 40, 40)
  println(l.zipRight.toSeq) // List((10,0), (20,1), (30,2), (40,3))
}

/** Hints:
  * - move implementation of :: from trait List to ListImplementation
  * - play a bit with already implemented methods
  * - implement flatMap: it maps each element into a list, and flattens the whole list of lists into a list (use append) -- the solution is 2 lines!
  * - implement zipRight: creates a list of pairs, with the second element being 0,1,2,3,.. -- may need an internal recursion
  */