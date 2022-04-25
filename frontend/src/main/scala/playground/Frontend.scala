package playground

import com.raquo.laminar.api.L._
import playground.protocol.ExampleService
import zio._
import zio.app.DeriveClient

object Frontend {
  val runtime = Runtime.default
  val client  = DeriveClient.gen[ExampleService]

  val nameVar = Var("")
  val signVar = Var("")

  def view: Div =
    div(
      p("The adventures of ", child.text <-- GlobalState.nameVar, " in ", child.text <-- GlobalState.signVar),
//      div(
//        p("Inventory"),
//        child <-- GlobalState.hasGlassShard.signal.map(hasGlassShard =>
//          if (hasGlassShard) {
//            span("You have a glass shard")
//          } else {
//            span("You don't have a glass shard")
//          }
//        )
//      ),
      hr(),
      child <-- GlobalState.currentStage.signal.map(_.stageDiv)
    )

}
