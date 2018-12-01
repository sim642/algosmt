package eu.sim642.algosmt

package object util {
  implicit class HeadIterator[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next) else None
  }

  implicit class StreamUnfoldOps(stream: Stream.type) {
    // https://github.com/tpolecat/examples/blob/ab444af9101b9049d6bd7ebf13ae583bc77ac60a/src/main/scala/eg/Unfold.scala
    def unfold[A, B](a: A)(f: A => Option[(A, B)]): Stream[B] =
      f(a).map{ case (a, b) => b #:: unfold(a)(f)}.getOrElse(Stream.empty)

    def unfold0[A](a: A)(f: A => Option[A]): Stream[A] =
      unfold(a)(a => f(a).map(a => (a, a)))
  }
}
