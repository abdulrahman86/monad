
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
