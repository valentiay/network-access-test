package test.impl

import cats.effect.{Deferred, IO, Resource}
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer, ChannelOption, ConnectTimeoutException}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.socket.SocketChannel

import scala.concurrent.duration.*
import test.{AccessTest, AccessTestResult, Status}

import java.net.{InetAddress, UnknownHostException}

case class TcpConnectAccessTest(target: String, host: String, port: Int, timeout: FiniteDuration)(workerGroup: NioEventLoopGroup) extends AccessTest {
  def test: IO[AccessTestResult] =
    for {
      start <- IO.monotonic
      status <- IO.async[Status] { callback =>
        IO {
          val bootstrap = Bootstrap()
          bootstrap.group(workerGroup)
          bootstrap.channel(classOf[NioSocketChannel])
          bootstrap.handler(new TcpConnectClientChannelInitializer(status => callback(Right(status))))
          val f = bootstrap.connect(host, port).sync()
          Some(IO(f.channel().disconnect()))
        }
      }.timeoutTo(timeout, IO.pure(Status.timeout))
        .handleError{
          case _: UnknownHostException => Status.dns_err
          case other => Status.other_err(other)
        }
      end <- IO.monotonic
      time <- IO.realTimeInstant
    } yield AccessTestResult(target, status, end.minus(start), time)
}

object TcpConnectAccessTest {
  private def parseHostAndPort(target: String): IO[(String, Int)] = {
    val colonIndex = target.lastIndexOf(":")
    if (colonIndex < 0) {
      IO.raiseError(new IllegalArgumentException(s"Could not parse host and port from $target"))
    } else {
      val (host, port) = target.splitAt(colonIndex)
      port.tail.toIntOption match {
        case Some(value) => IO.pure((host, value))
        case None => IO.raiseError(new IllegalArgumentException(s"Could not parse host and port from $target"))
      }
    }
  }

  def init(target: String, timeout: FiniteDuration)(workerGroup: NioEventLoopGroup): IO[AccessTest] =
    for {
      hostAndPort <- parseHostAndPort(target)
    } yield TcpConnectAccessTest(target, hostAndPort._1, hostAndPort._2, timeout)(workerGroup)
}

class TcpConnectClientHandler(callback: Status => Unit) extends ChannelInboundHandlerAdapter {
  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    callback(Status.ok)
    ctx.close()
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    callback(
      cause match {
        case timeout: ConnectTimeoutException => Status.timeout
        case other => Status.other_err(other)
      }
    )
    ctx.close()
  }
}

class TcpConnectClientChannelInitializer(callback: Status => Unit) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = ch.pipeline().addLast(new TcpConnectClientHandler(callback))
}
