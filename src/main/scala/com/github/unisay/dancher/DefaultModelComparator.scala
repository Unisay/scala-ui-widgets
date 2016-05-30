package com.github.unisay.dancher

import dom._

class DefaultModelComparator extends ModelComparator {

  override def diff(old: Model, updated: Model): ActionF[_] = {
    if (old == updated) {
      noAction
    } else {
      log("Models are different")
      (old, updated) match {
        case (o: VerticalLayout, u: VerticalLayout) ⇒
          noAction
        case (o: HorizontalLayout, u: HorizontalLayout) ⇒
          noAction
        case _ ⇒
          old replaceWith updated
      }
    }

  }

}
