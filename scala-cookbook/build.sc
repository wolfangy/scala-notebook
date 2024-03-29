import mill._
import mill.scalalib._

object scalacookbook extends ScalaModule {

  def scalaVersion = "2.13.12"

  object test extends ScalaTests with TestModule.Munit {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::0.7.29"
    )
  }
}

