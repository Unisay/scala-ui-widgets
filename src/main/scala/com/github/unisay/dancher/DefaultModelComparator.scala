package com.github.unisay.dancher

import DomAction._

class DefaultModelComparator extends ModelComparator {

  override def diff(old: Model, updated: Model): DomActionF[Unit] = {
    if (old == updated) {
      log("No changes")
    } else {
      log("Models are different")
    }

  }

}
