import scala.concurrent.duration.*

object formatting {

  def indentLeft(string: String, limit: Int): String =
    if (string.length >= limit) string.take(limit - 4) + "... "
    else " " * (limit - string.length - 1) + string + " "

  def indentRight(string: String, limit: Int): String =
    if (string.length >= limit) string.take(limit - 4) + "... "
    else string + " " * (limit - string.length)

  def formatDuration(duration: FiniteDuration): String = {
    if (duration.toNanos < 1000) s"${duration.toNanos}ns"
    else if (duration.toMicros < 1000) s"${duration.toMicros}us"
    else if (duration.toMillis < 1000) s"${duration.toMillis}ms"
    else if (duration.toSeconds < 60) s"${duration.toSeconds}s"
    else if (duration.toMinutes < 60) s"${duration.toMinutes}m"
    else if (duration.toHours < 24) s"${duration.toMinutes}h"
    else s"${duration.toDays}d"
  }

}
