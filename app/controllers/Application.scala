package controllers

import models._
import play.api.mvc._
import scala.collection.mutable.ListBuffer

object Application extends Controller {
  //val sourceXml : String = new java.io.File(".").getCanonicalPath() + "/app/assets/Rules.xml"  //"C:/github/just-play-scala/app/assets/Rules.xml"
  val sourceXml : String = "public/Rules.xml"  //"C:/github/just-play-scala/app/assets/Rules.xml"
  val rawXml = scala.xml.XML.loadFile(sourceXml)
  val sourceItems: Items = Items.fromEntireXml(rawXml)
  val sourceItemsets: Itemsets = Itemsets.fromEntireXml(rawXml)
  val sourceAssociationRules: AssociationRules = AssociationRules.fromEntireXml(rawXml)

  def index = Action { Ok(views.html.main())  }
  def items = Action { Ok(sourceItems.toString)  }
  def itemsets = Action {  Ok(sourceItemsets.toString) }
  def rules = Action {  Ok(sourceAssociationRules.toString) }

  //Validate parameter rank ... e.g. value 0
  def getNewSearch(terrasse: Boolean, balkon: Boolean, dachgeschoss: Boolean,garage: Boolean, lift: Boolean, rank: Option[Int]) = Action {
    //map parameters to item values/ids
    val input: ListBuffer[String] = ListBuffer()
    if (terrasse) input += "Terrasse=1"
    if (balkon) input += "Balkon=1"
    if (dachgeschoss) input += "Dachgeschoss=1"
    if (lift) input += "Lift=1"
    input += "Preis_Bis_Cluster_1300-1800=1"
    //<Item id="14" value="Preis_Bis_Cluster_von1800=1"/>

    //find items and get there ids
    val itemIds = sourceItems.getItemIds(input)

    //find itemsetIds with this parameters
    val itemsetIds = sourceItemsets.getItemsetIds(itemIds)

    //find rules and get the consequent
    val matchingRules = sourceAssociationRules.getMatchingRules(itemsetIds)

    if (matchingRules.length > 0) {
      //choose the most suitable itemset/rule
      val sortedrules = matchingRules.sortBy(r => (r.lift, r.confidence, r.support))

      // Ok((rank != Option(null) && (sortedrules.length - rank.get - 1) > 0).toString)

      if (rank != Option(null) && (sortedrules.length - rank.get ) < 0) {
        Ok("Sorry FAT LIP")
        //Ok(((sortedrules.length - rank.get - 1) >0).toString)
      }
      else {
        val bestrule = if(rank == Option(null)) sortedrules.head else sortedrules(rank.get-1)

        val resultItemsetId = bestrule.consequent

        //get itemset by itemsetId
        val resultItemset: Itemset = sourceItemsets.getItemsetByItemsetId(resultItemsetId)

        //get item values
        val values = sourceItems.getItemValues(resultItemset.itemIds)

        Ok(values.toString)
       }
     }
     else
       Ok("Sorry, there are no matching rules!")
    }
}
