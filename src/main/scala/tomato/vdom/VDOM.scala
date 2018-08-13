package tomato.vdom

import java.awt.event.ActionListener

import javax.swing._
import net.miginfocom.swing.MigLayout

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

  sealed trait PanelDirection
  case object Horizontal extends PanelDirection
  case object Vertical extends PanelDirection

  case class FrameProps()
  case class PanelProps(direction: PanelDirection)
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

    protected override def updateProps(props: Props): VdomNode = {
      this
    }

    override def initialRender: VdomNode = {
      val panel = new JPanel()
      val layout = props.direction match {
        case Horizontal => new MigLayout()
        case Vertical => new MigLayout("wrap 1")
      }
      panel.setLayout(layout)
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
      this.component.add(node.component)
    }

    override def removeChild(node: VdomNode): Unit = {
      this.component.remove(node.component)
    }
  }

  object Panel {
    def apply(direction: PanelDirection, children: VdomNode*): Panel = {
      Panel(PanelProps(direction), children, null)
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
        component.setText(newProps.text)
      }
      this.copy(props=newProps)
    }

    override def initialRender: VdomNode = {
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
        component.setText(newProps.text)
      }
      if(this.props.onClick != newProps.onClick){
        component.removeActionListener(this.props.onClick)
        component.addActionListener(newProps.onClick)
      }
      this.copy(props = newProps)
    }

    override def initialRender: VdomNode = {
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
