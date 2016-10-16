
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
  def flatMap[A, B] : F[A] => (A => F[B]) => F[B] = in => f => compose[Any, A, B](_ => in)(f)(())
  def unit[A]: (=>A )=> F[A]
  def sequence[A]: List[F[A]] => F[List[A]] = {
    case List() => unit(List())
    case h :: t => map2(h)(sequence(t))(_ :: _)
  }
  def traverse[A, B]: List[A] => (A => F[B]) => F[List[B]] = in => f => sequence(in.map(f))
  def replicateM[A]: Int => F[A] => F[List[A]] = n => in => n match {
    case 0 => flatMap(in)(a => unit(List(a)))
    case n => map2(in)(replicateM(n-1)(in))(_ :: _)
  }
  def compose[A, B, C] : (A => F[B]) => (B => F[C]) => (A => F[C])
}

object Monad {

  implicit def stateFunctor[S] = new Functor[({type f[X] = State[S, X]})#f] {

    override def map[A, B]: (State[S, A]) => ((A) => B) => State[S, B] = (in: State[S, A]) => f => in map f
  }

  implicit def stateMonad[S] = new Monad[({type f[X] = State[S, X]})#f] {

    override implicit def functor: Functor[({type f[X] = State[S, X]})#f] = stateFunctor[S]

    override def compose[A, B, C]: ((A) => State[S, B]) => ((B) => State[S, C]) => (A) => State[S, C] =
      f1 => f2 => a => {
        f1(a) flatMap f2
      }

    override def unit[A]: (=>A) => State[S, A] = a => (State((s: S)  => (s, a)))
  }
}

case class Id[A](value: A) {

  def flatMap[B]: (A => Id[B]) => Id[B] = f => f(value)
}

object Id{

  implicit object IdFunctor extends Functor[Id] {
    override def map[A, B]: (Id[A]) => ((A) => B) => Id[B] = id => f => id flatMap (a => Id(f(a)))
  }

  implicit object IdMonad extends Monad[Id] {

    override implicit def functor: Functor[Id] = IdFunctor

    override def compose[A, B, C]: ((A) => Id[B]) => ((B) => Id[C]) => (A) => Id[C] =
      f1 => f2 => a => f1(a) flatMap f2

    override def unit[A]: (=> A) => Id[A] = a => Id(a)
  }
}

case class Reader[R, A](run: R => A)

object Reader{

  implicit def readerFunctor[R] = new Functor[({type f[X] = Reader[R, X]})#f]{

    override def map[A, B]: (Reader[R, A]) => ((A) => B) => Reader[R, B] =
      r => f => Reader((in: R) => f(r.run(in)))
  }

  implicit def readerMonad[R] = new Monad[({type f[X] = Reader[R, X]})#f] {

    override implicit def functor: Functor[({type f[X] = Reader[R, X]})#f] = readerFunctor[R]

    override def compose[A, B, C]: ((A) => Reader[R, B]) => ((B) => Reader[R, C]) => (A) => Reader[R, C] =
      f1 => f2 => a => {
        Reader((r: R) => f2(f1(a).run(r)).run(r))
      }

    override def unit[A]: (=> A) => Reader[R, A] = a => Reader((r: R) => a)
  }
}
