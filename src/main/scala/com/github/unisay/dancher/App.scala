package com.github.unisay.dancher

import com.github.unisay.dancher.DemoPage.Model
import com.github.unisay.dancher.interpreter.{DomInterpreter, JsInterpreter}
import com.github.unisay.dancher.widget.EffectAction
import monix.execution.Ack
import monix.execution.Scheduler.Implicits.global
import com.github.unisay.dancher.widget.Widget._

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  val loremIpsum =
    """
      |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut ut massa eget erat dapibus sollicitudin. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Pellentesque a augue. Praesent non elit. Duis sapien dolor, cursus eget, pulvinar eget, eleifend a, est. Integer in nunc. Vivamus consequat ipsum id sapien. Duis eu elit vel libero posuere luctus. Aliquam ac turpis. Aenean vitae justo in sem iaculis pulvinar. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam sit amet mi.
      |Aenean auctor, mi sit amet ultricies pulvinar, dui urna adipiscing odio, ut faucibus odio mauris eget justo. Mauris quis magna quis augue interdum porttitor. Sed interdum, tortor laoreet tincidunt ullamcorper, metus velit hendrerit nunc, id laoreet mauris arcu vitae est. Nulla nec nisl. Mauris orci nibh, tempor nec, sollicitudin ac, venenatis sed, lorem. Quisque dignissim tempus erat. Maecenas molestie, pede ac ultrices interdum, felis neque vulputate quam, in sodales felis odio quis mi. Aliquam massa pede, pharetra quis, tincidunt quis, fringilla at, mauris. Vestibulum a massa. Vestibulum luctus odio ut quam. Maecenas congue convallis diam. Cras urna arcu, vestibulum vitae, blandit ut, laoreet id, risus. Ut condimentum, arcu sit amet placerat blandit, augue nibh pretium nunc, in tempus sem dolor non leo. Etiam fringilla mauris a odio. Nunc lorem diam, interdum eget, lacinia in, scelerisque sit amet, purus. Nam ornare.
      |Donec placerat dui ut orci. Phasellus quis lacus at nisl elementum cursus. Cras bibendum egestas nulla. Phasellus pulvinar ullamcorper odio. Etiam ipsum. Proin tincidunt. Aliquam aliquet. Etiam purus odio, commodo sed, feugiat volutpat, scelerisque molestie, velit. Aenean sed sem sit amet libero sodales ultrices. Donec dictum, arcu sed iaculis porttitor, est mauris pulvinar purus, sit amet porta purus neque in risus. Mauris libero. Maecenas rhoncus. Morbi quis nisl.
      |Vestibulum laoreet tortor eu elit. Cras euismod nulla eu sapien. Sed imperdiet. Maecenas vel sapien. Nulla at purus eu diam auctor lobortis. Donec pede eros, lacinia tincidunt, tempus eu, molestie nec, velit. Nullam ipsum odio, euismod non, aliquet nec, consequat ac, felis. Duis fermentum mauris sed justo. Suspendisse potenti. Praesent at libero sit amet ipsum imperdiet fermentum. Aliquam enim nisl, dictum id, lacinia sit amet, elementum posuere, ipsum. Integer luctus dictum libero. Pellentesque sed pede sed nisl bibendum porttitor. Phasellus tempor interdum nisi. Mauris nec magna. Phasellus massa pede, vehicula sed, ornare at, ullamcorper ut, nisl. Sed turpis nisl, hendrerit sit amet, consequat id, auctor nec, arcu. Quisque fringilla tincidunt massa. In eleifend, nulla sed mollis vestibulum, mauris orci facilisis ante, id pharetra dolor ipsum vitae sem. Integer dictum.
      |Nunc ut odio. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec mauris tellus, dapibus vel, hendrerit vel, sollicitudin ut, ligula. Ut justo metus, accumsan placerat, ultrices sit amet, congue at, nulla. Integer in quam. Cras sollicitudin mattis magna. Vestibulum neque eros, egestas ut, tincidunt vel, ullamcorper non, ligula. Vivamus eu lacus. Donec rhoncus metus et odio. Donec est. Nulla facilisi. Suspendisse potenti. Etiam tempor pede nec ante. Vestibulum adipiscing velit vel neque.
      |Quisque ornare erat rhoncus lectus. Donec vitae ante at enim mollis egestas. Mauris convallis. Fusce convallis, nisl eu sagittis suscipit, risus ligula aliquam libero, in imperdiet neque mi non risus. Aenean dictum ultricies risus. Praesent ut ligula vitae purus ornare auctor. Cras tellus mauris, adipiscing ac, dignissim auctor, faucibus in, sem. Cras mauris libero, pharetra sit amet, lacinia eu, vehicula eleifend, sapien. Donec ac tellus. Sed eros dui, vulputate vel, auctor pharetra, tincidunt at, ipsum. Duis at dolor ac leo condimentum pulvinar. Donec molestie, dolor et fringilla elementum, nibh nibh iaculis orci, eu elementum odio turpis et odio. Phasellus fermentum, justo id placerat egestas, arcu nunc molestie ante, non imperdiet ligula lectus sed erat. Quisque sed ligula. Sed ac nulla. Nullam massa.
      |Sed a purus. Mauris non nibh blandit neque cursus scelerisque. Quisque ultrices sem nec dolor. Donec non diam ut dui consequat venenatis. Nullam risus massa, egestas in, facilisis tristique, molestie sed, mi. Duis euismod turpis sit amet quam. Vestibulum ornare felis eget dolor. Phasellus ac urna vel sapien lacinia adipiscing. Donec egestas felis id mi. Sed erat. Vestibulum porta vulputate neque. Maecenas scelerisque, sem id sodales pretium, sem mauris rhoncus magna, at scelerisque tortor mauris nec dui. Nullam blandit rhoncus velit. Nam accumsan, enim id vestibulum feugiat, lorem nibh placerat urna, eget laoreet diam tortor at lorem. Suspendisse imperdiet consectetur dolor.
    """.stripMargin

  val demoModel = Model(tabs = TabsModel(activeTab = 1),
    text1 = loremIpsum, text2 = loremIpsum, text3 = loremIpsum)

  implicit val interpreter = new DomInterpreter()
  import interpreter._

  @JSExport
  def debug(action: EffectAction): String = JsInterpreter.interpretScript(demoModel, action)._2

  @JSExport
  override def main(): Unit = {
    println("App started")

    DemoPage(Body).render(demoModel).events.subscribe(nextFn = {
      case (_, TabActivated(index)) =>
        println(s"Tab $index activated!")
        Ack.Continue
    })

    ()
  }

}

