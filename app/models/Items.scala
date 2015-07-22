package models

import scala.collection.mutable.ListBuffer

/**
 * Created by cschackerl on 22.07.2015.
 */
class Items(val allItems: Seq[Item]){
  override def toString = (for (v <- allItems) yield sys.props("line.separator") + v.toString).toString()
  def getItemIds(values: ListBuffer[String]):Seq[Int] = allItems.filter(i => values.exists(p => i.value == p)).map(_.id)
  def getItemValues(itemIds: Seq[Int]): Seq[String] = allItems.filter(i => itemIds.contains(i.id)).map(_.value)
}
object Items {
  // convert XML to an Item object
  def fromEntireXml(node: scala.xml.Node):Items = {
    val allItems = (node \\ "PMML" \\ "AssociationModel" \\ "Item" ).map( x => new Item((x \\ "@id").toString.toInt,(x \ "@value").toString()))
    new Items(allItems)
  }
}