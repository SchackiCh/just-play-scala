package controllers

import play.api.mvc._
import play.api.libs.json._
import java.io._
import scala.collection.mutable.ListBuffer
import scala.xml.XML

object Application extends Controller {
  val sourceXml : String = new java.io.File(".").getCanonicalPath() + "/app/assets/Rules.xml"  //"C:/github/just-play-scala/app/assets/Rules.xml"
  val rawXml = scala.xml.XML.loadFile(sourceXml)
  val sourceItems: Items = Items.fromEntireXml(rawXml)
  val sourceItemsets: Itemsets = Itemsets.fromEntireXml(rawXml)
  val sourceAssociationRules: AssociationRules = AssociationRules.fromEntireXml(rawXml)

  class AssociationRule(val support: Double, val confidence: Double, val lift: Double, val antecedent: Int, val consequent: Int) {
    override def toString =  s"AssociationRule ... support: $support, confidence: $confidence, lift: $lift, antecedent: $antecedent, consequent: $consequent"
  }
  object AssociationRule

  class AssociationRules(val rules: Seq[AssociationRule]) {
    override def toString = rules.map(sys.props("line.separator") + _.toString).toString
    def getMatchingRules(itemsetIds: Seq[Int]):Seq[AssociationRule] = rules.filter(r => itemsetIds.contains(r.antecedent))
    }

  object AssociationRules {
    def fromEntireXml(node: scala.xml.Node): AssociationRules = {
      val associationRules = (node \\ "PMML" \\ "AssociationModel" \\ "AssociationRule")
        .map(x => new AssociationRule(
            (x \ "@support").toString.toDouble,
            (x \ "@confidence").toString.toDouble,
            (x \ "@lift").toString.toDouble,
            (x \ "@antecedent").toString.toInt,
            (x \ "@consequent").toString.toInt)
        )
      new AssociationRules(associationRules)
    }
  }

  class Item(var id: Int, var value: String) {
    override def toString = s"id:$id, value:$value"
  }
  object Item

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

  class Itemset(var id: Int, var itemIds: Seq[Int]) {
    override def toString = s"id: $id, itemRefs:" + itemIds
  }
  object Itemset

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

  def items = Action { Ok(sourceItems.toString)  }
  def itemsets = Action {  Ok(sourceItemsets.toString) }
  def rules = Action {  Ok(sourceAssociationRules.toString) }

  def index = Action {
    Ok(views.html.main())
  }

  def getNewSearch(terrasse: Boolean, balkon: Boolean, dachgeschoss: Boolean,garage: Boolean, lift: Boolean, rank: Option[Int]) = Action {
    //map parameters to item values/ids
    val input: ListBuffer[String] = ListBuffer()
    if (terrasse) input += "Terrasse=1"
    if (balkon) input += "Balkon=1"
    if (dachgeschoss) input += "Dachgeschoss=1"
    if (lift) input += "Lift=1"
      input+="Preis_Bis_Cluster_1300-1800=1"
        //<Item id="14" value="Preis_Bis_Cluster_von1800=1"/>

    //find items and get there ids
    val itemIds = sourceItems.getItemIds(input)

    //find itemsetIds with this parameters
    val itemsetIds = sourceItemsets.getItemsetIds(itemIds)

    //find rules and get the consequent
    val matchingRules = sourceAssociationRules.getMatchingRules(itemsetIds)
    if(matchingRules.length > 0) {
      //choose the most suitable itemset/rule
      val sortedrules = matchingRules.sortBy(r => (r.lift, r.confidence, r.support))

      val bestrule = if(rank == Option(null)) sortedrules.head else sortedrules(rank.get-1)

      val resultItemsetId = bestrule.consequent

      //get itemset by itemsetId
      val resultItemset: Itemset = sourceItemsets.getItemsetByItemsetId(resultItemsetId)

      //get item values
      val values = sourceItems.getItemValues(resultItemset.itemIds)

      Ok(values.toString)
    }
    else
      Ok("Sorry, there are no matching rules!")
  }
}
