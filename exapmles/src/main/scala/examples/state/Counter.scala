package examples.state

import cats.Functor
import cats.effect.kernel.{ Ref, Sync }
import cats.effect.{ IO, IOApp }
import cats.syntax.all._

trait Counter[F[_]] {
  def incr: F[Unit]
  def get: F[Int]
}

object Counter {
  def make[F[_]: Functor: Ref.Make]: F[Counter[F]] =
    Ref.of[F, Int](0).map { ref =>
      new Counter[F] {
        override def incr: F[Unit] = ref.update(_ + 1)
        override def get: F[Int]   = ref.get
      }
    }
}

class LiveCounter[F[_]] private (ref: Ref[F, Int]) extends Counter[F] {
  override def incr: F[Unit] = ref.update(_ + 1)
  override def get: F[Int]   = ref.get
}
object LiveCounter {
  def make[F[_]: Sync]: F[Counter[F]] =
    Ref.of[F, Int](0).map(new LiveCounter[F](_))
}

object CounterApp extends IOApp.Simple {
  override def run: IO[Unit] =
    Counter.make[IO].flatMap { c =>
      for {
        _ <- c.get.flatMap(IO.println)
        _ <- c.incr
        _ <- c.get.flatMap(IO.println)
        _ <- c.incr.replicateA(5).void
        _ <- c.get.flatMap(IO.println)
      } yield ()
    }
}
