package test.impl

import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter, ChannelInitializer, ChannelOption, ChannelPipeline, ConnectTimeoutException, SimpleChannelInboundHandler}
import io.netty.handler.codec.DelimiterBasedFrameDecoder
import io.netty.handler.codec.Delimiters
import io.netty.handler.codec.string.StringDecoder
import io.netty.handler.codec.string.StringEncoder
import cats.effect.{Deferred, IO, Resource}
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.socket.SocketChannel
import io.netty.channel.ChannelInitializer
import io.netty.channel.ChannelPipeline
import io.netty.handler.codec.DelimiterBasedFrameDecoder
import io.netty.handler.codec.Delimiters
import io.netty.handler.codec.string.StringDecoder
import io.netty.handler.codec.string.StringEncoder
import io.netty.handler.ssl.{SslContext, SslContextBuilder}
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.handler.ssl.util.InsecureTrustManagerFactory

import scala.concurrent.duration.*
import test.{AccessTest, AccessTestResult, Status}

import java.net.{InetAddress, URI, UnknownHostException}

case class HttpStatusAccessTest(target: String, uri: URI, timeout: FiniteDuration)(workerGroup: NioEventLoopGroup) extends AccessTest {

  private def httpGet(uri: URI): String =
    s"""GET ${uri.getPath} HTTP/1.1
       |Host: ${uri.getHost}
       |cache-control: no-cache
       |connection: close
       |accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9
       |accept-language: ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7\n\n""".stripMargin

  def test: IO[AccessTestResult] =
    for {
      start <- IO.monotonic
      status <- IO.async[Status] { callback =>
        IO {
          val port = Option(uri.getPort).filter(_ > 0).getOrElse(if (uri.getScheme == "https") 443 else 80)
          val bootstrap = Bootstrap()
            .group(workerGroup)
            .channel(classOf[NioSocketChannel])
            .handler(new TelnetClientInitializer(status => callback(Right(status)), uri.getHost, port))
          val ch = bootstrap.connect(uri.getHost, port).sync().channel()
          ch.writeAndFlush(httpGet(uri)).sync()
          Some(IO(ch.disconnect()))
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

object HttpStatusAccessTest {
  def init(target: String, timeout: FiniteDuration)(workerGroup: NioEventLoopGroup): IO[AccessTest] =
    for {
      uri <- IO(URI.create(target))
        .handleErrorWith(error =>
          IO.raiseError(new IllegalArgumentException(s"Failed to parse URI: ${error.getMessage}", error))
        )
    } yield HttpStatusAccessTest(target, uri, timeout)(workerGroup)
}

@Sharable
class TelnetClientHandler(callback: Status => Unit) extends SimpleChannelInboundHandler[String] {
  private def parseHttpStatus(string: String): Option[Int] = {
    val httpVersion = "http/1.1 "
    if (string.toLowerCase.startsWith(httpVersion)) {
      string.drop(httpVersion.length).trim.take(3).toIntOption
    } else None
  }

  @throws[Exception]
  override protected def channelRead0(ctx: ChannelHandlerContext, msg: String): Unit = {
    parseHttpStatus(msg) match {
      case Some(code) if code < 400 => callback(Status.http_ok(code))
      case Some(code) if code >= 400 => callback(Status.http_err(code))
      case _ => ()
    }
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
    callback(Status.other_err(cause))
    ctx.close
  }
}

class TelnetClientInitializer(callback: Status => Unit, host: String, port: Int) extends ChannelInitializer[SocketChannel] {
  private val decoder = new StringDecoder
  private val encoder = new StringEncoder
  private val sslCtx = Option.when(System.getProperty("ssl") != null)(
    SslContextBuilder.forClient().trustManager(InsecureTrustManagerFactory.INSTANCE).build()
  )

  override def initChannel(ch: SocketChannel): Unit = {
    val pipeline = ch.pipeline
    sslCtx.foreach(c => pipeline.addLast(c.newHandler(ch.alloc, host, port)))
    pipeline.addLast(new DelimiterBasedFrameDecoder(8192, Delimiters.lineDelimiter: _*))
    pipeline.addLast(decoder)
    pipeline.addLast(encoder)
    pipeline.addLast(new TelnetClientHandler(callback))
  }
}