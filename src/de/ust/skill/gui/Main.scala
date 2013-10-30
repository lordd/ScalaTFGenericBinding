package de.ust.skill.gui

import java.io.File

import scala.swing._
import scala.swing.ListView.Renderer
import scala.swing.TabbedPane.Page
import scala.swing.event.ListSelectionChanged

import de.ust.skill.scala.generic.FieldDefinition
import de.ust.skill.scala.generic.State

import javax.swing.filechooser.FileFilter

object Main extends SimpleSwingApplication {
  private val fileChooser = new FileChooser(new File("."))
  fileChooser.fileFilter = new FileFilter() {
    override def accept(f: File): Boolean = f.getName.endsWith(".sf")

    override def getDescription = "binary SKilL files"
  }

  def top = new MainFrame {
    title = "My Frame"

    val tabs = new TabbedPane
    contents = tabs

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("read File")({
          fileChooser.showOpenDialog(null)
          val state = State.read(fileChooser.selectedFile.getAbsolutePath())
          tabs.pages += new Page(fileChooser.selectedFile.getName(), new FieldView(state).panel)
        }))
      }
    }

    minimumSize = new Dimension(300, 200)
  }
}

class FieldView(state: State) {

  // selected type name
  var selectedName: Option[String] = None
  // selected instance ID
  var selectedInstance: Option[Long] = None

  val names: ListView[String] = new ListView(state.fieldData.keySet.toList) {
    listenTo(selection)
    reactions += {
      case ListSelectionChanged(s, r, l) ⇒ {
        val i = selection.items
        if (i.isEmpty) {
          selectedName = None
          instances.listData = Nil
        } else {
          selectedName = Some(i.head)
          instances.listData = state.fieldData(selectedName.get).keys.to
        }
        selectedInstance = None
        fields.listData = Nil
      }
    }
  }

  val instances: ListView[Long] = new ListView[Long](Nil) {
    listenTo(selection)
    reactions += {
      case ListSelectionChanged(s, r, l) ⇒ {
        val i = selection.items
        if (i.isEmpty) {
          selectedInstance = None
          fields.listData = Nil
        } else {
          selectedInstance = Some(i.head)
          fields.listData = state.fieldData(selectedName.get)(i.head).toList
        }
      }
    }
  }

  val fields = new ListView[(Long, Any)](Nil) {
    renderer = Renderer({
      case (nr, value) ⇒ selectedName match {
        case None           ⇒ ""
        case Some("string") ⇒ s"""#$nr → "$value""""
        case Some(name) ⇒ {
          val FieldDefinition(t, n) = state.typeDefinitions(selectedName.get).fields(nr)
          s"$t $n: $value"
        }
      }
    })
  }

  val panel = new GridPanel(1, 2) {

    contents ++= List(
      new GridPanel(1, 2) {
        contents ++= List(
          new ScrollPane(names),
          new ScrollPane(instances)
        )
      },
      new ScrollPane(fields)
    )
  }
}
