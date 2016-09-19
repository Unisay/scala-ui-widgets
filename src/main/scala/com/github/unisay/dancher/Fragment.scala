package com.github.unisay.dancher

import cats.syntax.cartesian._
import fs2.interop.cats._

object Fragment {

  implicit class FragmentOps(val fragment: Fragment) extends AnyVal {

    def <*>(right: Widget)(implicit ec: EventsComposer = EventsComposer.both) =
      (fragment |@| right) map Bindings.append

    def <*(right: Widget): Fragment =
      <*>(right)(EventsComposer.left)

    def *>(right: Widget): Fragment =
      <*>(right)(EventsComposer.right)
  }

}
