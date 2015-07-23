package models

/**
 * Created by cschackerl on 23.07.2015.
 */
class Itemsets(var itemsets: Seq[Itemset]){
  override def toString = (for (is <- itemsets) yield sys.props("line.separator") + "ItemSetId:" + is.id + ": " + {
    for(itemId <- is.itemIds) yield itemId.toString()
  }).toString()

  def getItemsetIds(itemIds: Seq[Int]):Seq[Int] = (itemsets.filter(_.itemIds == itemIds).map(_.id))
  def getItemsetByItemsetId(itemsetId: Int):Itemset = itemsets.filter(_.id == itemsetId).head
}
object Itemsets {

  // convert XML to an Item object
  def fromEntireXml(node: scala.xml.Node):Itemsets = {
    val itemsets = (node \\ "PMML" \\ "AssociationModel" \\ "Itemset" )
      .map( x => new Itemset((x \\ "@id").toString.toInt,(x \\ "ItemRef").map(iref =>(iref \\ "@itemRef").toString().toInt)))
    new Itemsets(itemsets)
  }
}