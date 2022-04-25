package playground

import com.raquo.laminar.api.L._

object GlobalState {
  val nameVar = Var("")
  val signVar = Var("")

  val hasGlassShard = Var(false)
  val hasBrick      = Var(false)

  val currentStage: Var[Stage] = Var(Stages.start)
}
