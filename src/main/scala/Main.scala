import javax.swing._

object VDOM {
  sealed trait VdomNode

  case class FrameProps()
  case class PanelProps()
  case class LabelProps(text: String)
  case class ButtonProps(text: String)

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
    def apply(text: String): Button = {
      Button(ButtonProps(text))
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
          new JButton(c.props.text)
        }
      }
    }

    private def applyChanges(node: VDOM.VdomNode): Unit = {
      throw new Exception("Not Implemented")
    }
  }

}

import VDOM._

case class HelloWorld() extends Component[Unit] {
  override def render(props: Unit): VdomNode = {
    Frame(children = Seq(
      Panel(children = Seq(
        Label("Hello")
      ))
    ))
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    val App = HelloWorld()
    val renderer = new VDOMRenderer()
    val root = App.render(Unit)
    renderer.render(root)
  }

}
