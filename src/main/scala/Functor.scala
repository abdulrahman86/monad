
trait Functor[F[_]] {
  def map[A, B] : F[A] => (A => B) => F[B]

  def distribute[A, B] : F[(A, B)] => (F[A], F[B]) = in => (map(in)(_._1), map(in)(_._2))

  def codistribute[A, B] : Either[F[A], F[B]] => F[Either[A, B]] =
    {
      case Left(x) => map(x)(Left(_))
      case Right(x) => map(x)(Right(_))
    }
}

object Functor{

  implicit object ListFunctor extends Functor[List] {

    override def map[A, B]: (List[A]) => ((A) => B) => List[B] = l => f => l.map(f)
  }
}

trait Monad[F[_]] extends Functor[F]{

  implicit def functor: Functor[F]

  override def map[A, B] : F[A] => (A => B) => F[B] = functor.map
  def map2[A, B, C]: F[A] => F[B] => ((A, B) => C) => F[C] =
    in1 => in2 => f => flatMap(in1)(a => map(in2)(b => f(a, b)))
  def flatMap[A, B] : F[A] => (A => F[B]) => F[B]
  def unit[A]: A => F[A]
}

object Monad {


}
