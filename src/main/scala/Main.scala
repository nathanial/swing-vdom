import java.awt.event.ActionEvent

import diode._
import diode.ActionResult.ModelUpdate
import javax.swing._

object VDOM {
  sealed trait VdomNode

  case class FrameProps()
  case class PanelProps()
  case class LabelProps(text: String)
  case class ButtonProps(text: String, onClick: () => Unit)

  case class Frame(
    props: FrameProps = FrameProps(),
    children: Seq[VdomNode] = Seq()
  ) extends VdomNode

  case class Panel(
    props: PanelProps = PanelProps(),
    children: Seq[VdomNode] = Seq()
  ) extends VdomNode
  case class Label(
    props: LabelProps
  ) extends VdomNode
  case class Button(
    props: ButtonProps
  ) extends VdomNode

  object Button {
    def apply(text: String, onClick: () => Unit = () => Unit): Button = {
      Button(ButtonProps(text, onClick))
    }
  }

  object Label {
    def apply(text: String): Label = {
      Label(LabelProps(text))
    }
  }

  trait Component[A] {
    def render(props: A): VdomNode
    def shouldComponentUpdate(newProps: A, oldProps: A): Boolean = { true }
    def componentDidMount(): Unit = {}
    def componentWillUnmount():Unit = {}
  }



  class VDOMRenderer {
    var currentDOM: VdomNode = null

    def render(newDOM: VdomNode): Unit = {
      SwingUtilities.invokeLater(() => {
        if(currentDOM == null){
          initialRender(newDOM)
          currentDOM = newDOM
        } else {
          applyChanges(newDOM)
        }
      })
    }

    private def initialRender(node: VdomNode): java.awt.Component = {
      node match {
        case c:Frame => {
          val frame = new JFrame()
          frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
          for(child <- c.children){
            frame.add(initialRender(child))
          }
          frame.pack()
          frame.setVisible(true)
          frame
        }
        case c:Panel => {
          val panel = new JPanel()
          for(child <- c.children){
            panel.add(initialRender(child))
          }
          panel
        }
        case c:Label => {
          new JLabel(c.props.text)
        }
        case c:Button => {
          val btn = new JButton(c.props.text)
          btn.addActionListener((event: ActionEvent) => {
            c.props.onClick()
          })
          btn
        }
      }
    }

    private def applyChanges(node: VDOM.VdomNode): Unit = {
      throw new Exception("Not Implemented")
    }
  }

}

import VDOM._

case class HelloWorldProps(count: Int)
case class HelloWorld() extends Component[HelloWorldProps] {
  override def render(props: HelloWorldProps): VdomNode = {
    Frame(children = Seq(
      Panel(children = Seq(
        Label(s"Hello, this count is: ${props.count}"),
        Button("Increment", onClick=() => AppCircuit.dispatch(Increment)),
        Button("Decrement", onClick=() => AppCircuit.dispatch(Decrement))
      ))
    ))
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
    val App = HelloWorld()
    val renderer = new VDOMRenderer()
    val root = App.render(HelloWorldProps(0))
    AppCircuit.subscribe(AppCircuit.zoom(identity))((model: ModelRO[RootModel]) => {
      println("Re-Render")
      renderer.render(App.render(HelloWorldProps(model.value.counter)))
    })
    renderer.render(root)
  }

}
