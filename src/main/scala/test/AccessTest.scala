package test

import cats.effect.IO

trait AccessTest {
  def test: IO[AccessTestResult]
}
