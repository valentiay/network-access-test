import cats.effect.{IO, Ref}
import formatting.{formatDuration, indentLeft, indentRight}
import test.AccessTestResult

import java.time.*
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

class ResultPrinter(resultsRef: Ref[IO, Map[String, AccessTestResult]]) {

  private val targetChars = 30
  private val statusChars = 14
  private val durationChars = 7
  private val timeChars = 20
  private val relativeTimeChars = 10

  private val legend =
    indentRight("target", targetChars) +
      indentRight("status", statusChars) +
      indentRight("test duration", durationChars + 10) +
      indentLeft("test time", timeChars - 10 + relativeTimeChars)

  def formatResultRow(now: Instant, result: AccessTestResult): String = {
    val targetF = indentRight(result.target, targetChars)
    val statusF = indentRight(result.status.toString, statusChars)
    val durationF = indentLeft(formatDuration(result.duration), durationChars)
    val time = result.time.atZone(ZoneId.systemDefault).toLocalDateTime.truncatedTo(ChronoUnit.SECONDS)
    val timeF = indentLeft(time.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME), timeChars)
    val relativeTime = FiniteDuration(now.toEpochMilli - result.time.toEpochMilli, TimeUnit.MILLISECONDS)
    val relativeTimeF = indentLeft(formatDuration(relativeTime) + " ago", relativeTimeChars)
    s"$targetF$statusF$durationF$timeF$relativeTimeF"
  }
  
  def formatResults(now: Instant, results: List[AccessTestResult]): String = {
    val nowFormatted = now.atZone(ZoneId.systemDefault()).toLocalDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    val formattedResultRows = 
      if (results.nonEmpty) results.map(result => formatResultRow(now, result)).sorted.mkString(System.lineSeparator())
      else "NO DATA YET"
      
    s"""${"=" * 80}
       |${indentRight(s"Testing results at $nowFormatted", 80)}
       |${"-" * 80}
       |$legend
       |${"-" * 80}
       |$formattedResultRows
       |${"=" * 80}
       |
       |""".stripMargin
  }
    

  import cats.syntax.traverse._
  import cats.instances.list._

  def print: IO[Unit] =
    for {
      results <- resultsRef.get
      now <- IO.realTimeInstant
      formatted = formatResults(now, results.values.toList)
      _ <- IO.println(formatted)
      _ <- IO.sleep(5.seconds)
      _ <- print
    } yield ()

  def start: IO[Unit] = (IO.sleep(1.second) *> print).start.void
}
