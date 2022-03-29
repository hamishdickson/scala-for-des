package com.hamishdickson.training

trait MyOption[+T]
object MyOption {
  final case class MySome[T](value: T) extends MyOption[T]
  final object MyNone                  extends MyOption[Nothing]

  def map[A, B](a: MyOption[A])(f: A => B): MyOption[B] =
    a match {
      case MySome(value) => MySome(f(value))
      case MyNone        => MyNone
    }
}

trait MyList[+T]
object MyList {
  final case class MyCons[T](head: T, tail: MyList[T]) extends MyList[T]
  final object MyNil                                   extends MyList[Nothing]

  def map[A, B](ls: MyList[A])(f: A => B): MyList[B] = ls match {
    case MyCons(head, tail) => MyCons(f(head), map(tail)(f))
    case MyNil              => MyNil
  }
}

// now abstract
trait Mapper[F[_]] {
  def map2[A, B](a: F[A])(f: A => B): F[B]
}

object Mapper {
  implicit val myOptionMapper = new Mapper[MyOption] {
    def map2[A, B](a: MyOption[A])(f: A => B): MyOption[B] = a match {
      case MyOption.MySome(a) => MyOption.MySome(f(a))
      case MyOption.MyNone    => MyOption.MyNone
    }
  }
}

object Training2 {
  def foo(o: MyOption[Int])(g: Int => String)(implicit mapper: Mapper[MyOption]): MyOption[String] =
    mapper.map2(o)(g)
}

object Training {
  def foo(file: String): MyOption[Int] =
    // go to filesystem and if files exists get filesize
    ???

  import MyOption._

  val a: MySome[Int] = MySome(12)

  lazy val b: MySome[String] = MySome("baosufbsaoub")

  def bar(a: String) = a + "2"

  val l: Int => String = a =>
    // import noasubnfaou
    a.toString + "sfufabf" + b.toString()

  l(12)
}
