import scala.concurrent.duration.FiniteDuration
import org.apache.commons.cli.HelpFormatter
import cats.effect.*
import org.apache.commons.cli.{CommandLineParser, DefaultParser, Options}
import cats.syntax.traverse.*
import cats.instances.list.*
import java.io.IOException
import java.nio.file.*
import java.util

import scala.jdk.CollectionConverters.*
import java.util.concurrent.*
import scala.concurrent.duration.*
import scala.util.Try

case class Config(
  timeout: FiniteDuration,
  interval: FiniteDuration,
  parallelism: Int,
  targets: List[String],
  method: Method,
)

enum Method {
  case http
  case reachable
  case tcp_connect
}

object Config {
  private val timeoutDescription =
    "Test timeout in seconds. If test takes longer than this value, it is considered unsuccessful. Default: 5 seconds."
  private val intervalDescription =
    "Minimal interval between two consequent scans of single target in seconds. Default: 10 seconds."
  private val parallelismDescription =
    "Maximal number of simultaneously running tests. Default: 10."
  private val targetsFileDescription =
    "Name of a file containing list of targets (hosts with ports or URLs) to test. " +
      "Each target must be on a new line. " +
      "This option cannot be used with -ts (-targets)."
  private val targetsDescription =
    "List of targets (hosts with ports or URLs) to test, separated by comma. Cannot be used with -f (-targets-file)."
  private val tcpConnectDescription =
    "Check accessibility of host by making tcp connection with specified port. " +
      "Host is considered accessible if connection is successful. " +
      "Target must be specified as <host>:<port>" +
      "This is a default option for checking accessibility." +
      "See also: -r (-reachable), -h (-http)."
  private val reachableDescription =
    "Check accessibility of host by pinging it. Target must be specified as <host>. " +
      "Super user priveleges are required. " +
      "See also: -tcp (-tcp-connect), -h (-http)."
  private val httpDescription =
    "Check accessibility of host by sending http GET request. " +
      "Host is considered accessible if it responds with 4** or 5** HTTP status. " +
      "Target must be specified as <url> (<scheme>://<host>[:<port>][<path>][<query]). " +
      "See also: -tcp (-tcp-connect), -r (-reachable)"

  private val options =
    Options()
      .addOption("t", "timeout", true, timeoutDescription)
      .addOption("i", "interval", true, intervalDescription)
      .addOption("p", "parallelism", true, parallelismDescription)
      .addOption("f", "targets-file", true, targetsFileDescription)
      .addOption("ts", "targets", true, targetsDescription)
      .addOption("tcp", "tcp-connect", false, tcpConnectDescription)
      .addOption("r", "reachable", false, reachableDescription)
      .addOption("h", "http", false, httpDescription)

  private val parser: CommandLineParser = new DefaultParser

  private def raiseIfNotInt(value: String, name: String): IO[Int] =
    value.toIntOption.fold(IO.raiseError(new IllegalArgumentException(s"$name must be integer, got $value")))(IO.pure)

  private def parseTargetsFromString(string: String): List[String] =
    string.split(",").toList.map(_.trim)

  private def parseTargetsFromFile(filename: String): IO[List[String]] =
    IO {
      Files.readAllLines(Paths.get(filename)).asScala.toList.filter(_.nonEmpty).map(_.trim)
    }.handleErrorWith { error =>
      IO.raiseError(new IllegalArgumentException(s"Failed to read file $filename", error))
    }

  val helpformatter: HelpFormatter = new HelpFormatter
  def parseFromArgs(args: List[String]): IO[Config] = {
    val cmd = parser.parse(options, args.toArray)
    for {
      _ <- if (cmd.getOptions.isEmpty) {
        IO(helpformatter.printHelp("java -jar network-access-test.jar", options)) *>
          IO.raiseError(new IllegalArgumentException("You must specify some options"))
      } else IO.unit
      timeout <- Option(cmd.getOptionValue("t"))
        .fold(IO.pure(5))(t => raiseIfNotInt(t, "Timeout"))
        .map(t => FiniteDuration(t, TimeUnit.SECONDS))
      interval <- Option(cmd.getOptionValue("i"))
        .fold(IO.pure(10))(i => raiseIfNotInt(i, "Interval"))
        .map(i => FiniteDuration(i, TimeUnit.SECONDS))
      parallelism <- Option(cmd.getOptionValue("p"))
        .fold(IO.pure(10))(i => raiseIfNotInt(i, "Parallelism"))

      targets <- if (cmd.hasOption("ts") && cmd.hasOption("h")) IO.raiseError(new IllegalArgumentException("Targets (-targets, -ts) and targets file (-targets-file, -f) must not be specified together")) else if (cmd.hasOption("f")) parseTargetsFromFile(cmd.getOptionValue("f")) else if (cmd.hasOption("ts")) IO.pure(parseTargetsFromString(cmd.getOptionValue("ts"))) else IO.raiseError(new IllegalArgumentException("Targets (-targets, -ts) or targets file (-targets-file, -f) must be specified"))

      method <- if (
        (if (cmd.hasOption("tcp")) 1 else 0) +
          (if (cmd.hasOption("r")) 1 else 0) +
          (if (cmd.hasOption("h")) 1 else 0) > 1
      ) IO.raiseError(
        new IllegalArgumentException("Only one option of -tcp (-tcp-connect), -r (-reachable), -h (-http) can be specified")
      ) else if (cmd.hasOption("r")) IO.pure(Method.reachable) else if (cmd.hasOption("h")) IO.pure(Method.http) else IO.pure(Method.tcp_connect)
    } yield Config(timeout, interval, parallelism, targets, method)
  }
}