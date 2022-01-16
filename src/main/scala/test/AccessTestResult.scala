package test

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

case class AccessTestResult(target: String, status: Status, duration: FiniteDuration, time: Instant)
