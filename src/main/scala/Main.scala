import cats.effect.{ExitCode, IO, IOApp, Ref, Resource}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.instances.list.*
import cats.syntax.traverse.*
import io.netty.channel.nio.NioEventLoopGroup

import java.time.*
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import formatting.*
import test.{AccessTest, AccessTestResult}
import test.impl.{HttpStatusAccessTest, ReachableAccessTest, TcpConnectAccessTest}

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object Main extends IOApp {
  def test(config: Config, queue: Queue[IO, AccessTest], ref: Ref[IO, Map[String, AccessTestResult]]): IO[Unit] =
    Resource.make(queue.take)(test => (IO.sleep(config.interval) >> queue.offer(test)).start.void).use(test =>
      test.test.flatMap(result => ref.update(_.updated(result.target, result)))
    ) >> test(config, queue, ref)

  def run(args: List[String]): IO[ExitCode] =
    (for {
      workerGroup <- Resource.make[IO, NioEventLoopGroup](
        IO(new NioEventLoopGroup)
      )(workerGroup =>
        IO(workerGroup.shutdownGracefully().sync)
      )
      config <- Config.parseFromArgs(args).toResource
      queue <- Queue.unbounded[IO, AccessTest].toResource
      ref <- Ref.of[IO, Map[String, AccessTestResult]](Map.empty).toResource
      tests <-
        (config.method match {
          case Method.tcp_connect =>
            config.targets.traverse(target => TcpConnectAccessTest.init(target, config.timeout)(workerGroup))
          case Method.reachable =>
            config.targets.traverse(target => ReachableAccessTest.init(target, config.timeout))
          case Method.http =>
            config.targets.traverse(target => HttpStatusAccessTest.init(target, config.timeout)(workerGroup))
        }).toResource
      _ <- tests.traverse[IO, Unit](queue.offer).toResource
      _ <- IO.parReplicateAN(config.parallelism)(config.parallelism, test(config, queue, ref)).start.toResource
      _ <- ResultPrinter(ref).start.toResource
      _ <- IO.never.toResource
    } yield ()).use_
      .as(ExitCode.Success)
      .handleErrorWith(error => IO.println(error.getMessage).as(ExitCode.Error))
}