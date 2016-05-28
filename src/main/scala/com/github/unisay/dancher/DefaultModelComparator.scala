package com.github.unisay.dancher

import DomAction._

class DefaultModelComparator extends ModelComparator {

  override def diff(old: Model, updated: Model): DomActionF[_] = {
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
