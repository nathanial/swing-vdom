import java.awt.event.{ActionEvent, ActionListener}

import diode._
import javax.swing._

object VDOM {

  sealed trait VdomNode {
    type Props
    type SwingComponent <: java.awt.Component

    def props: Props
    def children: Seq[VdomNode]
    def component: SwingComponent

    def initialRender: VdomNode

    protected def updateProps(props: Props): VdomNode
    protected def addChild(node: VdomNode): Unit = ???
    protected def removeChild(node: VdomNode): Unit = ???
    protected def destroy:Unit = {}
    protected def copy(
      props: Props = this.props,
      children: Seq[VdomNode] = this.children,
      component: SwingComponent = this.component
    ): VdomNode

    final def update(newNode: VdomNode): VdomNode = {
      var updatedNode = this
      if(this.props != newNode.props){
        updatedNode = this.updateProps(newNode.props.asInstanceOf[Props])
      }
      if(this.children != newNode.children){
        updatedNode = this.updateChildren(newNode.children)
      }
      updatedNode
    }


    final protected def updateChildren(newChildren: Seq[VdomNode]): VdomNode = {
      if(this.children == newChildren){
        this
      } else {
        // group children by type, and then zip them with indexes, and compare
        // the type+index can be the id for the purposes of DiffChildren

        val DiffChildren(added, removed, updated) = diff(this.children, newChildren)

//        println("Diff Children", added, removed)

        val addedChildren = added.map(_.initialRender)
        addedChildren.foreach(this.addChild)

        removed.foreach(this.removeChild)
        removed.foreach(_.destroy)

        val updatedChildren = updated.map(change => change.old.update(change.`new`))

        // update component after children are added or removed
        if(added.nonEmpty || removed.nonEmpty){
          this.component.invalidate()
          this.component.repaint()
        }
        this.copy(children=updatedChildren ++ addedChildren)
      }
    }

    override def toString: String = {
      this.getClass.getSimpleName + "(" + this.props.toString + "," + this.children + ")"
    }


  }
  case class Changed(old: VdomNode, `new`: VdomNode)
  case class DiffChildren(added: Seq[VdomNode], removed: Seq[VdomNode], changed: Seq[Changed])

  object DiffChildren {
    lazy val empty = DiffChildren(Seq(), Seq(), Seq())
  }

  private def diff(oldChildren: Seq[VdomNode], newChildren: Seq[VdomNode]): DiffChildren = {
    val oldChildrenWithIDs = combineWithIDs(oldChildren)
    val newChildrenWithIDs = combineWithIDs(newChildren)
    val added = newChildrenWithIDs.filterNot(c => oldChildrenWithIDs.exists(o => o.id == c.id)).map(_.node)
    val removed = oldChildrenWithIDs.filterNot(o => newChildrenWithIDs.exists(c => o.id == c.id)).map(_.node)
    val changed = newChildrenWithIDs.map(`new` => {
      oldChildrenWithIDs.find(_.id == `new`.id).flatMap(old => {
        if(`new` != old){
            Some(Changed(old.node,`new`.node))
          } else {
            None
          }
      })
    }).filter(_.isDefined).map(_.get)
    DiffChildren(added, removed, changed)
  }

  case class NodeWithID(node: VdomNode, id: String)

  private def combineWithIDs(nodes: Seq[VDOM.VdomNode]): Seq[NodeWithID] = {
    nodes.zipWithIndex.groupBy{
      case (node, index) => node.getClass.getName
    }.flatMap {
      case (group, ns) =>
        ns.sortBy(_._2).map(_._1).zipWithIndex.map {
          case (node, index) =>
            NodeWithID(node, s"$group-$index")
        }
    }.toSeq
  }

  case class FrameProps()
  case class PanelProps()
  case class LabelProps(text: String)
  case class ButtonProps(text: String, onClick: ActionListener) {
    override def toString: String = s"ButtonProps($text)"
  }

  case class Frame(
    props: FrameProps = FrameProps(),
    children: Seq[VdomNode] = Seq(),
    component: JFrame = null
  ) extends VdomNode {
    type Props = FrameProps
    type SwingComponent = JFrame

    protected override def updateProps(newProps: FrameProps):VdomNode = {
      if(this.props != newProps){
        println("Apply Frame Props")
      }
      this
    }

    override def initialRender: VdomNode = {
      val frame = new JFrame()
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      val childNodes = this.children.map(_.initialRender)
      childNodes.foreach(node => frame.add(node.component))
      frame.pack()
      frame.setVisible(true)
      this.copy(component = frame, children = childNodes)
    }

    override def copy(props: FrameProps, children: Seq[VdomNode], component: JFrame): VdomNode = {
      Frame(props, children, component)
    }

    override def addChild(node: VdomNode): Unit = {
      this.component.add(node.component)
    }

    override def removeChild(node: VdomNode): Unit = {
      this.component.remove(node.component)
    }
  }

  case class Panel(
    props: PanelProps,
    children: Seq[VdomNode],
    component: JPanel
  ) extends VdomNode {
    type Props = PanelProps
    type SwingComponent = JPanel

    println("Create Panel", children)

    protected override def updateProps(props: Props): VdomNode = {
      this
    }

    override def initialRender: VdomNode = {
      val panel = new JPanel()
      val childNodes = this.children.map(_.initialRender)
      for(child <- childNodes){
        panel.add(child.component)
      }
      this.copy(component = panel, children = childNodes)
    }

    override def copy(props: PanelProps, children: Seq[VdomNode], component: JPanel): VdomNode = {
      Panel(props, children, component)
    }

    override def addChild(node: VdomNode): Unit = {
      println("Add to Panel", node)
      this.component.add(node.component)
    }

    override def removeChild(node: VdomNode): Unit = {
      println("Remove from Panel", node)
      this.component.remove(node.component)
    }
  }

  object Panel {
    def apply(children: VdomNode*): Panel = {
      Panel(PanelProps(), children, null)
    }
  }

  case class Label(
    props: LabelProps,
    component: JLabel = null
  ) extends VdomNode {
    type Props = LabelProps
    type SwingComponent = JLabel

    val children = Seq()

    override def updateProps(newProps: Props): VdomNode = {
      if(props.text != newProps.text){
//        println("Update Label Text")
        component.setText(newProps.text)
      }
      this.copy(props=newProps)
    }

    override def initialRender: VdomNode = {
      println("Create JLAbel")
      val label = new JLabel(this.props.text)
      this.copy(component = label)
    }

    override def copy(props: LabelProps, children: Seq[VdomNode], component: JLabel): VdomNode = {
      Label(props, component)
    }
  }
  case class Button(
    props: ButtonProps,
    component: JButton = null
  ) extends VdomNode {
    type Props = ButtonProps
    type SwingComponent = JButton
    type Node = this.type
    val children = Seq()

    protected override def updateProps(newProps: Props): VdomNode = {
      if(this.props.text != newProps.text){
        println("Update Button Text")
        component.setText(newProps.text)
      }
      if(this.props.onClick != newProps.onClick){
        println("Update Button onClick")
        component.removeActionListener(this.props.onClick)
        component.addActionListener(newProps.onClick)
      }
      this.copy(props = newProps)
    }

    override def initialRender: VdomNode = {
      println("Update initialRender")
      val btn = new JButton(this.props.text)
      btn.addActionListener(this.props.onClick)
      this.copy(component = btn)
    }

    override def copy(props: ButtonProps, children: Seq[VdomNode], component: JButton): VdomNode = {
      Button(props, component)
    }
  }

  object Button {
    def apply(text: String, onClick: ActionListener): Button = {
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

  class VDOMRenderer{
    var currentDOM: VdomNode = _

    def render(newDOM: VdomNode): Unit = {
      SwingUtilities.invokeLater(() => {
        if(currentDOM == null){
          currentDOM = newDOM.initialRender
        } else {
          currentDOM = currentDOM.update(newDOM)
        }
      })
    }
  }

}

import VDOM._

case class HelloWorldProps(count: Int)
case class HelloWorld() extends Component[HelloWorldProps] {

  override def render(props: HelloWorldProps): VdomNode = {
    Frame(children = Seq(
      Panel(Seq(
        Label(s"Hello, this count is: ${props.count}"),
        Button("Increment", onClick=increment),
        Button("Decrement", onClick=decrement),
        if(props.count < 0) Label("It's gone negative") else null
      ).filterNot(_ == null):_*)
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
      renderer.render(App.render(HelloWorldProps(model.value.counter)))
    })
    renderer.render(root)
  }

}
