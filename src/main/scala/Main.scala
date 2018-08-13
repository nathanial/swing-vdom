import java.awt.event.{ActionEvent, ActionListener}
import java.util.UUID

import diode._
import diode.ActionResult.ModelUpdate
import javax.swing._

import scala.collection.mutable

object VDOM {
  sealed trait VdomNode {
    def id: UUID
    def children: Seq[VdomNode]
  }
  case class Changed(old: VdomNode, `new`: VdomNode)
  case class DiffChildren(added: Seq[VdomNode], removed: Seq[VdomNode], changed: Seq[Changed])

  object DiffChildren {
    lazy val empty = DiffChildren(Seq(), Seq(), Seq())
  }

  case class FrameProps()
  case class PanelProps()
  case class LabelProps(text: String)
  case class ButtonProps(text: String, onClick: ActionListener)

  case class Frame(
    id: UUID,
    props: FrameProps = FrameProps(),
    children: Seq[VdomNode] = Seq()
  ) extends VdomNode

  case class Panel(
    id: UUID,
    props: PanelProps = PanelProps(),
    children: Seq[VdomNode] = Seq()
  ) extends VdomNode
  case class Label(
    id: UUID,
    props: LabelProps
  ) extends VdomNode {
    val children = Seq()
  }
  case class Button(
    id: UUID,
    props: ButtonProps
  ) extends VdomNode {
    val children = Seq()
  }

  object Button {
    def apply(id: UUID, text: String, onClick: ActionListener): Button = {
      Button(id, ButtonProps(text, onClick))
    }
  }

  object Label {
    def apply(id: UUID, text: String): Label = {
      Label(id, LabelProps(text))
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
    val componentsTable = new mutable.HashMap[UUID, java.awt.Component]()

    def render(newDOM: VdomNode): Unit = {
      SwingUtilities.invokeLater(() => {
        if(currentDOM == null){
          initialRender(newDOM)
        } else {
          applyChanges(newDOM)
        }
        currentDOM = newDOM
      })
    }

    private def initialRender(node: VdomNode): java.awt.Component = {
      node match {
        case c:Frame => {
          val frame = new JFrame()
          componentsTable.update(c.id, frame)
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
          componentsTable.update(c.id, panel)
          for(child <- c.children){
            panel.add(initialRender(child))
          }
          panel
        }
        case c:Label => {
          val label = new JLabel(c.props.text)
          componentsTable.update(c.id, label)
          label
        }
        case c:Button => {
          val btn = new JButton(c.props.text)
          componentsTable.update(c.id, btn)
          btn.addActionListener(c.props.onClick)
          btn
        }
      }
    }

    private def applyChanges(newDOM: VDOM.VdomNode): Unit = {
      applyChanges(currentDOM, newDOM)
    }

    private def applyChanges(old: VdomNode, `new`: VdomNode): Unit = {
      applyPropertyChanges(old, `new`)
      applyChildChanges(old, `new`)
    }

    private def applyPropertyChanges(old: VdomNode, `new`: VdomNode): Unit = {
      old match {
        case oldLabel:Label =>
          applyLabelPropertyChanges(findComponent(old), oldLabel.props, `new`.asInstanceOf[Label].props)
        case oldButton:Button =>
          applyButtonPropertyChanges(findComponent(old), oldButton.props, `new`.asInstanceOf[Button].props)
        case oldPanel:Panel =>
          applyPanelPropertyChanges(findComponent(old), oldPanel.props, `new`.asInstanceOf[Panel].props)
        case oldFrame:Frame =>
          applyFramePropertyChanges(findComponent(old), oldFrame.props, `new`.asInstanceOf[Frame].props)
      }
    }

    private def applyChildChanges(old: VdomNode, `new`: VdomNode): Unit = {
      val DiffChildren(added, removed, changed) = diffChildren(old, `new`)
      for(toAdd <- added) {
        old match {
          case panel:Panel =>
            throw new Exception("add to panel - not implemented")
          case frame:Frame =>
            throw new Exception("add to frame - not implemented")
          case x =>
            throw new Exception(s"Cannot add children to: $x")
        }
      }
      for(toRemove <- removed){
        old match {
          case panel:Panel =>
            throw new Exception("remove from panel - not implemented")
          case frame: Frame =>
            throw new Exception("remove from frame - not implemented")
          case x =>
            throw new Exception(s"Cannot remove children from: $x")
        }
      }
      for(toChange <- changed){
        applyChanges(toChange.old, toChange.`new`)
      }
    }

    private def applyLabelPropertyChanges(label: JLabel, oldProps: LabelProps, newProps: LabelProps): Unit = {
      if(oldProps.text != newProps.text){
        println("Update Label Text", newProps.text)
        label.setText(newProps.text)
      }
    }

    private def applyButtonPropertyChanges(button: JButton, oldProps: ButtonProps, newProps: ButtonProps): Unit = {
      if(oldProps.text != newProps.text){
        println("Update Button Text", newProps.text)
        button.setText(newProps.text)
      }
      if(oldProps.onClick != newProps.onClick){
        println("Update Button OnClick", newProps.onClick)
        button.removeActionListener(oldProps.onClick)
        button.addActionListener(newProps.onClick)
      }
    }

    private def applyPanelPropertyChanges(panel: JPanel, oldProps: PanelProps, newProps: PanelProps): Unit = {
      println("applyFramePropertyChanges - Nothing to Apply")
    }

    private def applyFramePropertyChanges(frame: JFrame, oldProps: FrameProps, newProps: FrameProps): Unit = {
      println("applyFramePropertyChanges - Nothing to Apply")
    }

    private def findComponent[A](node: VDOM.VdomNode): A = {
      componentsTable(node.id).asInstanceOf[A]
    }

    private def diffChildren(oldNode: VdomNode, newNode: VdomNode): DiffChildren = {
      if(oldNode == newNode){
        DiffChildren.empty
      } else {
        val added = newNode.children.filterNot(c => oldNode.children.exists(o => o.id == c.id))
        val removed = oldNode.children.filterNot(o => newNode.children.exists(c => o.id == c.id))
        val changed = oldNode.children.map(x => {
          newNode.children.find(_.id == x.id).flatMap(y => {
            if(x != y){
              Some(Changed(x,y))
            } else {
              None
            }
          })
        }).filter(_.isDefined).map(_.get)
        DiffChildren(added, removed, changed)
      }
    }

  }

}

import VDOM._

case class HelloWorldProps(count: Int)
case class HelloWorld() extends Component[HelloWorldProps] {

  private lazy val frameID = UUID.randomUUID()
  private lazy val panelID = UUID.randomUUID()
  private lazy val labelID = UUID.randomUUID()
  private lazy val incrementID = UUID.randomUUID()
  private lazy val decrementID = UUID.randomUUID()

  override def render(props: HelloWorldProps): VdomNode = {
    println("RENDER", props.count)
    Frame(frameID, children = Seq(
      Panel(panelID, children = Seq(
        Label(labelID, s"Hello, this count is: ${props.count}"),
        Button(incrementID, "Increment", onClick=increment),
        Button(decrementID, "Decrement", onClick=decrement)
      ))
    ))
  }

  private lazy val increment = new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      AppCircuit.dispatch(Increment)
    }
  }

  private lazy val decrement = new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = {
      AppCircuit.dispatch(Decrement)
    }
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
