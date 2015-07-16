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

  class AssociationRule(support: Double, confidence: Double, lift: Double, antecedent: Int, consequent: Int) {
    override def toString =  s"AssociationRule ... support: $support, confidence: $confidence, lift: $lift, antecedent: $antecedent, consequent: $consequent"
  }
  object AssociationRule

  class AssociationRules(rules: Seq[AssociationRule]) {
    override def toString = rules.map(sys.props("line.separator") + _.toString).toString
    }

  object AssociationRules {
    def fromEntireXml(node: scala.xml.Node): AssociationRules = {
      val associationRules = (node \\ "PMML" \\ "AssociationModel" \\ "AssociationRule")
        .map(x => new AssociationRule((x \ "@support").toString.toDouble,
            (x \ "@confidence").toString.toDouble,
            (x \ "@lift").toString.toDouble,
            (x \ "@antecedent").toString.toInt,
            (x \ "@consequent").toString.toInt))
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
  }
  object Items {
    // convert XML to an Item object
    def fromEntireXml(node: scala.xml.Node):Items = {
      val allItems = (node \\ "PMML" \\ "AssociationModel" \\ "Item" ).map( x => new Item((x \\ "@id").toString.toInt,(x \ "@value").toString()))
      new Items(allItems)
    }
  }

  class Itemset(var id: Int, var itemIds: Seq[Int]) {
    override def toString = s"id: $id, itemRefs: $itemsets.toString()"
  }
  object Itemset

  class Itemsets(var itemsets: Seq[Itemset]){
    override def toString = (for (is <- itemsets) yield sys.props("line.separator") + "ItemSetId:" + is.id + ": " + {
      for(itemId <- is.itemIds) yield itemId.toString()
    }).toString()

    //def getItemsetIds(itemIds: Seq[Int]):Seq[Int] = (itemsets.filter(is => itemIds.toSet.subsetOf(is.itemIds.toSet)).map(_.id))
    def getItemsetIds(itemIds: Seq[Int]):Seq[Int] = (itemsets.filter(_.itemIds == itemIds).map(_.id))
    //def getItemsetIds(itemIds: Seq[Int]):Seq[Int] = itemsets.map(_.id)
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

  def getNewSearch(hasTerrace: Boolean, hasLift: Boolean) = Action {
    //map parameters to item values/ids
    val input: ListBuffer[String] = ListBuffer()
    if(hasTerrace) input += "Terrasse=terrasse:ja"
    if(hasLift)  input += "Lift=lift:ja"


    //find items and get there ids
    val itemIds = sourceItems.getItemIds(input)

    //find itemsets with this parameters
    val itemsetIds = sourceItemsets.getItemsetIds(itemIds)
    //find rules and get the consequent
    //get items from itemsets and return it

    //return result
    Ok(itemsetIds.toString)
    //Ok(input.toString)
  }
}
