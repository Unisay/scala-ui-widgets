package com.github.unisay.dancher.widget

import org.scalatest.{FlatSpec, MustMatchers}
import scalatags.JsDom.all._

class BasicWidgetsSpec extends FlatSpec with MustMatchers with BasicWidgets {

  behavior of "TagWidget"

  it must "render correctly" in {
    val tag = div (
      h1 (id := "title", "This is a title"),
      p ("This is a big paragraph of text"),
      button (onclick := { })
    )
    TagWidget(tag)
  }

}
