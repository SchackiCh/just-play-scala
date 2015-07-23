package models

/**
 * Created by cschackerl on 23.07.2015.
 */
class Itemset(var id: Int, var itemIds: Seq[Int]) {
  override def toString = s"id: $id, itemRefs:" + itemIds
}
object Itemset