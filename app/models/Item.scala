package models

import scala.collection.mutable.ListBuffer

class Item(var id: Int, var value: String) {
  override def toString = s"id:$id, value:$value"
}
object Item
