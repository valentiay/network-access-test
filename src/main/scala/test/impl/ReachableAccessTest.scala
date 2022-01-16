package test.impl

import cats.effect.IO
import test.{AccessTest, AccessTestResult, Status}

import java.net.{InetAddress, UnknownHostException}
import scala.concurrent.duration.FiniteDuration

case class ReachableAccessTest(target: String, timeout: FiniteDuration) extends AccessTest {
  def test: IO[AccessTestResult] =
    for {
      start <- IO.monotonic
      status <- IO.blocking(InetAddress.getByName(target)).flatMap( host =>
        IO.blocking(host.isReachable(timeout.toMillis.toInt))
          .map(reachable => if (reachable) Status.ok else Status.unreachable)
        )
        .handleError {
          case _: UnknownHostException => Status.dns_err
          case other => Status.other_err(other)
        }
      end <- IO.monotonic
      time <- IO.realTimeInstant
    } yield AccessTestResult(target, status, end.minus(start), time)
}

object ReachableAccessTest {
  def init(target: String, timeout: FiniteDuration): IO[AccessTest] =
    for {
      _ <- IO(InetAddress.getByName(target)).handleErrorWith{
        case _: UnknownHostException => IO.unit
        case other => IO.raiseError(other)
      }
    } yield ReachableAccessTest(target, timeout)
}
