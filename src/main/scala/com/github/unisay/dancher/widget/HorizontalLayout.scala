package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Widget
import com.github.unisay.dancher.dom.DomId


case class HorizontalLayout[W : Widget](domId: DomId, children: Vector[W])
