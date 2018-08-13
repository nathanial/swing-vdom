package tomato.examples.counter

import java.awt.event.{ActionEvent, ActionListener}

import diode._
import tomato.vdom.VDOM._

object Counter {

  def apply(count: Int): VdomNode = {
    Frame(children = Seq(
      Panel(Vertical, Seq(
        Label(s"Hello, the count is: ${count}"),
        Panel(Horizontal,
          Button("Increment", onClick=increment),
          Button("Decrement", onClick=decrement)),
        if(count < 0) Label("It's gone negative") else null
      ).filterNot(_ == null):_*)
    ))
  }

  private lazy val increment: ActionListener = (_: ActionEvent) => {
    AppCircuit.dispatch(Increment)
  }

  private lazy val decrement: ActionListener = (_: ActionEvent) => {
    AppCircuit.dispatch(Decrement)
  }
}

case class RootModel(counter: Int = 0)

case object Increment extends diode.Action
case object Decrement extends diode.Action
case object Reset extends diode.Action

object AppCircuit extends diode.Circuit[RootModel] {
  override protected def initialModel: RootModel = RootModel()

  val incrementHandler =  new ActionHandler(zoomTo(_.counter)) {
    override def handle: PartialFunction[Any, ActionResult[RootModel]] = {
      case Increment => updated(value + 1)
      case Decrement => updated(value - 1)
      case Reset => updated(0)
    }
  }

  override protected def actionHandler = composeHandlers(incrementHandler)
}

object Main {

  def main(args: Array[String]): Unit = {
    val renderer = new VDOMRenderer()
    AppCircuit.subscribe(AppCircuit.zoom(identity))((model: ModelRO[RootModel]) => {
      renderer.render(Counter(model.value.counter))
    })
    renderer.render(Counter(0))
  }

}
