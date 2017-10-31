package priv

object Looper extends App {
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  def loop[T, U](syncFunc: => T, heavyComputeFunc: T => Future[U], accumulator: Seq[Future[U]]): Future[Seq[U]] =
    Future {
      Looper.synchronized {
        syncFunc
      }
    }.flatMap { t: T =>
      val longComputation = heavyComputeFunc(t)
      loop(syncFunc, heavyComputeFunc, accumulator :+ longComputation)
    }.recoverWith {
      case _ => Future.sequence(accumulator)
    }

  val stream = (1 to 100).toStream.toIterator

  val k = loop(
    if (stream.hasNext) stream.next else throw new RuntimeException("EOF"),
    (v: Int) => Future {
      Thread.sleep(834)
      println(v)
      v
    },
    Seq.empty)
  println(s"done ${Await.result(k, 20 second).sum}")
}