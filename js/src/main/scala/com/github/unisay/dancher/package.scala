package com.github.unisay

import org.scalajs.dom.document._
import org.scalajs.dom.raw._

package object dancher {

  sealed trait Action
  case class Log(text: String) extends Action
  case class Alert(text: String) extends Action
  case class AppendChild(parent: Node, child: Node)

  sealed trait DomAction extends Action

  sealed trait Widget {
    def render: List[Action]
  }

  case class MessageBox(message: String) extends Widget {
    override def render: List[Action] = List(
      Alert(message),
      AppendChild(body, createElement("div"))
    )
  }

}
