package playground.protocol

import zio._

trait ExampleService {
  def magicNumber: UIO[Int]
}
