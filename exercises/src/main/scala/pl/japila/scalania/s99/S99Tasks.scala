package pl.japila.scalania.s99

import annotation.tailrec
import scala.collection.GenTraversable

object S99Tasks {

}

trait S99TasksSolution {
  def p1[T](list : Seq[T]) : T
  def p2[T](list : Seq[T]) : T
  def p3[T](k: Int, l: Seq[T]) : T
  def p4(list : Seq[_]) : Int
  def p5[T](list : Seq[T]) : Seq[T]
  def p6[T](list : Seq[T]) : Boolean
  def p7(list : Seq[Any]) : Seq[Any]
  def p8[T](list : Seq[T]) : Seq[T]
  def p9[T](list : Seq[T]) : Seq[Seq[T]]
  def p10[T](list : Seq[T]) : Seq[(Int,T)]
  def p11[T](list : Seq[T]) : Seq[Either[(Int,T),T]]
  def p12[T](list : Seq[(Int,T)]) : Seq[T]
  def p13[T](list : Seq[T]) : Seq[(Int,T)]
  def p14[T](list : Seq[T]) : Seq[T]
  def p15[T](nr: Int, list : Seq[T]) : Seq[T]
  def p16[T](nr: Int, list : Seq[T]) : Seq[T]
  def p17[T](nr: Int, list : Seq[T]) : (Seq[T],Seq[T])
  def p18[T](from: Int,to: Int, list : Seq[T]) : Seq[T]
  def p19[T](nr: Int, list : Seq[T]) : Seq[T]
  def p20[T](nr: Int, list : Seq[T]) : (Seq[T],T)
  def p21[T](toAdd: T, position: Int, list : Seq[T]) : Seq[T]
  def p22(from:Int, to:Int): Seq[Int]
  def p23[T](sel:Int, list: Seq[T]): Seq[T]
  def p24(from:Int, to:Int): Seq[Int]
  def p25[T](list : Seq[T]) : Seq[T]
  def p26[T](combined:Int, list : Seq[T]) : Seq[Seq[T]]
  def p27[T](list : Seq[T]) : Seq[Seq[Seq[T]]]
  def p28[T](list : Seq[Seq[T]]) : Seq[Seq[T]]
}

class S99TasksSolutionNotImplemented extends S99TasksSolution {
  def p1[T](list: Seq[T]): T = list.last
  
  def p2[T](list: Seq[T]): T = list.reverse.tail.head

  def p3[T](k: Int, l: Seq[T]): T = l(k)

  def p4(list: Seq[_]): Int = list.foldLeft(0)( (l, _) => l+1)

  def p5[T](list: Seq[T]): Seq[T] = list.reverse

  def p6[T](list: Seq[T]): Boolean = ! list.splitAt(math.ceil((list.length.toDouble)/2.0).toInt).zipped.map((x,y) => x == y).contains(false)

  def p7(list: Seq[Any]): Seq[Any] =  list.flatMap {
     case l: Seq[_] => p7(l)
     case i => Seq(i)
  }

  def p8[T](list: Seq[T]): Seq[T] = list.foldRight(Seq[T]())( (t:T, b:Seq[T]) => if(b.isEmpty || b.head != t)  t +: b else b)

  def p9[T](list: Seq[T]): Seq[Seq[T]] = list.foldRight(Seq[Seq[T]](Seq[T]()))( (t, b) => if(b.head.isEmpty || b.head.head == t) {(t +: b.head) +: b.tail } else Seq[T](t) +: b)

  def p10[T](list: Seq[T]): Seq[(Int, T)] = p9(list).foldRight(Seq[(Int,T)]())((t,b) => (t.length, t.head) +: b)

  def p11[T](list: Seq[T]): Seq[Either[(Int, T), T]] = p10(list).map( _ match {
    case (n: Int, t) if(n > 1) => Left[(Int, T), T]((n,t))
    case (n: Int, t)  => Right[(Int, T), T](t)
     })

  def p12[T](list: Seq[(Int, T)]): Seq[T] = list.foldRight(Seq[T]())( (t, b) => Seq[T]().padTo(t._1, t._2) ++ b)

  def p13[T](list: Seq[T]): Seq[(Int, T)] =

  def p14[T](list: Seq[T]): Seq[T] = ???

  def p15[T](nr: Int, list: Seq[T]): Seq[T] = ???

  def p16[T](nr: Int, list: Seq[T]): Seq[T] = ???

  def p17[T](nr: Int, list: Seq[T]): (Seq[T], Seq[T]) = ???

  def p18[T](from: Int, to: Int, list: Seq[T]): Seq[T] = ???

  def p19[T](nr: Int, list: Seq[T]): Seq[T] = ???

  def p20[T](nr: Int, list: Seq[T]): (Seq[T],T) = ???

  def p21[T](toAdd: T, position: Int, list: Seq[T]): Seq[T] = ???

  def p22(from: Int, to: Int): Seq[Int] = ???

  def p23[T](sel: Int, list: Seq[T]): Seq[T] = ???

  def p24(from: Int, to: Int): Seq[Int] = ???

  def p25[T](list: Seq[T]): Seq[T] = ???

  def p26[T](combined: Int, list: Seq[T]): Seq[Seq[T]] = ???

  def p27[T](list: Seq[T]): Seq[Seq[Seq[T]]] = ???

  def p28[T](list: Seq[Seq[T]]): Seq[Seq[T]] = ???

}