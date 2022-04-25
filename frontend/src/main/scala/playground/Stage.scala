package playground

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import playground.GlobalState.{nameVar, signVar}

trait Stage {
  def stageDiv: Div
}

trait Step {
  def stepDiv: Div
}

object Separator extends Step {
  override def stepDiv: Div = div(hr())
}
case class TextStep(text: String) extends Step {
  override def stepDiv: Div = div(
    text
  )
}

class ActionStep(text: String, action: => Action) extends Step {
  override def stepDiv: Div = div(
    p(
      text,
      action.actionElems
    )
  )
}
object ActionStep {
  def apply(text: String, action: => Action): ActionStep = new ActionStep(text, action)
}

case class ConditionalStep(text: String, action: Action, condition: Signal[Boolean]) extends Step {
  override def stepDiv: Div = div(
    child <-- condition.map {
      case true =>
        p(
          text,
          action.actionElems
        )
      case false => span()
    }
  )
}

case class ComplexStep(div: Div) extends Step {
  override def stepDiv: Div = div
}

trait Action {
  def actionElems: List[ReactiveHtmlElement[_]]
}
case class StageChange(text: String, stage: Stage) extends Action {
  override def actionElems = List(
    button(
      text,
      inContext { thisNode => onClick.map(_ => stage) --> GlobalState.currentStage }
    )
  )
}

case class CharacterAction[A](
    text: String,
    actionOutcome: String,
    action: (A, Var[A])
) extends Action {
  val textVar = Var("")
  override def actionElems = List(
    button(
      text,
      inContext { thisNode => { onClick.map(_ => actionOutcome) --> textVar } },
      inContext { thisNode => { onClick.map(_ => action._1) } --> action._2 }
    ),
    span(child.text <-- textVar.signal)
  )
}

case class SimpleStage(steps: List[Step]) extends Stage {
  def stageDiv: Div = div(
    steps.map(_.stepDiv)
  )
}

object Stages {

  def start = SimpleStage(
    List(
      TextStep("You wake up in the middle of an empty road. You look around and see a sign that says: "),
      ComplexStep(
        div(
          p(
            "\"Welcome to ",
            input(
              inContext { thisNode => onInput.map(_ => thisNode.ref.value) --> signVar }
            ),
            ", place of wonder\""
          )
        )
      ),
      ComplexStep(
        div(
          p(
            "You can feel your head is heavy and it even takes you a second to remember your name, ",
            input(
              inContext { thisNode => onInput.map(_ => thisNode.ref.value) --> nameVar }
            )
          )
        )
      ),
      ConditionalStep("", StageChange("Stand up", standUp), GlobalState.nameVar.signal.map(_.nonEmpty)),
      ConditionalStep("", StageChange("Go back to sleep", goBackToSleep), GlobalState.nameVar.signal.map(_.nonEmpty))
    )
  )

  def standUp = SimpleStage(
    List(
      TextStep("You stand up"),
      ActionStep(
        "You see a residential building to your left",
        StageChange("Go towards the building", goToTheBuilding)
      ),
      ActionStep("A children park in front of you", StageChange("Go towards the park", goToThePark))
    )
  )

  def goBackToSleep = SimpleStage(
    List(
      TextStep("You go back to sleep"),
      TextStep("Everything faced to black..."),
      TextStep("You are dead")
    )
  )

  def goToTheBuilding: SimpleStage = SimpleStage(
    List(
      TextStep("There is a building in front of you"),
      ActionStep(
        "There is rubble around the building",
        CharacterAction(
          "Pick up a brick",
          "You picked up a brick",
          (true, GlobalState.hasBrick)
        )
      ),
      ActionStep(
        "Some windows are broken and glass shards are around the floor",
        CharacterAction(
          "Pick up glass",
          "You picked up a glass shard",
          (true, GlobalState.hasGlassShard)
        )
      ),
      ConditionalStep("", StageChange("Break window with brick", InsideBuilding), GlobalState.hasBrick.signal)
    )
  )

  def goToThePark = SimpleStage(
    List(
      TextStep("There is a park in front of you"),
      TextStep("Nothing interesting here"),
      Separator,
      ActionStep(
        "You see a residential building to your left",
        StageChange("Go towards the building", goToTheBuilding)
      )
    )
  )

  def InsideBuilding = SimpleStage(
    List(
      TextStep("You enter the building"),
      TextStep("It is quite dark"),
      ConditionalStep("", StageChange("Cut plastic with glass shard", InsidePlastic), GlobalState.hasGlassShard.signal),
      ActionStep("", StageChange("Exit the building", goToTheBuilding))
    )
  )

  def InsidePlastic = SimpleStage(
    List(
      TextStep("A zombie ate you"),
      TextStep("You are dead")
    )
  )
}
