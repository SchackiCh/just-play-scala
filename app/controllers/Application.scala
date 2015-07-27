package controllers

import models._
import play.api.mvc._
import scala.collection.mutable.ListBuffer


object Application extends Controller {

  val sourceXml: String = "public/Rules.xml"
  //"C:/github/just-play-scala/app/assets/Rules.xml"
  val url = "https://recoservice.blob.core.windows.net/recocontainer/Rules.xml"
  val rawXml = scala.xml.XML.load(url)
  val sourceItems: Items = Items.fromEntireXml(rawXml)
  val sourceItemsets: Itemsets = Itemsets.fromEntireXml(rawXml)
  val sourceAssociationRules: AssociationRules = AssociationRules.fromEntireXml(rawXml)


  def index = Action {
    Ok(views.html.main())
  }

  def items = Action {
    Ok(sourceItems.toString)
  }

  def itemsets = Action {
    Ok(sourceItemsets.toString)
  }

  def rules = Action {
    Ok(sourceAssociationRules.toString)
  }

  //Validate parameter rank ... e.g. value 0
  def getNewSearch(dachgeschoss: Option[Boolean],balkon: Option[Boolean], terrasse: Option[Boolean], garage: Option[Boolean], lift: Option[Boolean], energieausweis: Option[Boolean], zimmeranzahl_von: Option[Int],
                   preis_bis: Option[Int], flaeche_von: Option[Int], geo: Option[String], rank: Option[Int]) = Action {
    if (rank != Option(null) && rank.get <= 0)
      throw new IllegalArgumentException("The value of rank must be greater than 0!")

    //map parameters to item values/ids
    val input: ListBuffer[String] = ListBuffer()
    if (dachgeschoss != Option(null) && dachgeschoss.get) input += "Dachgeschoss=1"
    if (balkon != Option(null) && balkon.get) input += "Balkon=1"
    if (terrasse != Option(null) && terrasse.get) input += "Terrasse=1"
    if (garage != Option(null) && garage.get) input += "Garage=1"
    if (lift != Option(null) && lift.get) input += "Lift=1"
    if (energieausweis != Option(null) && energieausweis.get) input += "Energieausweis=1"
    input +=matchingZimmerAnzahl(zimmeranzahl_von)
    input += matchingFlaeche(flaeche_von)
    input += matchingPreis(preis_bis)
    if(geo != Option(null)) geo.get.split(',').map(g => input+= (matchingGeo(g)))

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

      if (rank != Option(null) && (sortedrules.length - rank.get) < 0) {
        Ok("Sorry FAT LIP")
        //Ok(((sortedrules.length - rank.get - 1) >0).toString)
      }
      else {
        val bestrule = if (rank == Option(null)) sortedrules.head else sortedrules(rank.get - 1)

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

  private def matchingZimmerAnzahl(zimmer: Option[Int]): String = zimmer match {
    case n if n == Option(null) => ""
    case n if n.get == 1 => "ZimmerAnzahl_Von=1"
    case n if n.get == 2=> "ZimmerAnzahl_Von=2"
    case n if n.get == 3 => "ZimmerAnzahl_Von=3"
    case n if n.get == 4 => "ZimmerAnzahl_Von=4"
    case n if n.get == 5 => "ZimmerAnzahl_Von=5"
    case n if n.get >= 6 => "ZimmerAnzahl_Von=6"
    case n if n.get >= 25 => "ZimmerAnzahl_Von=25"
    case n if n.get >= 70 => "ZimmerAnzahl_Von=70"
    case n if n.get >= 110 => "ZimmerAnzahl_Von=110"
  }

  private def matchingPreis(preis: Option[Int]): String = preis match {
    case n if n == Option(null) => ""
    case n if n.get <= 550 => "Preis_Bis_Cluster_bis550=1"
    case n if n.get  > 550 && n.get <= 700 => "Preis_Bis_Cluster_550-700=1"
    case n if n.get  > 700 && n.get <= 850 => "Preis_Bis_Cluster_700-850=1"
    case n if n.get  > 850 && n.get <= 1000 => "Preis_Bis_Cluster_850-1000=1"
    case n if n.get  > 1000 && n.get <= 1300 => "Preis_Bis_Cluster_1000-1300=1"
    case n if n.get  > 1300 && n.get <= 1800 => "Preis_Bis_Cluster_1300-1800=1"
    case n if n.get  > 1800 => "Preis_Bis_Cluster_von1800=1"
  }

  private def matchingFlaeche(flaeche: Option[Int]): String = flaeche match {
    case n if n == Option(null) => ""
    case n if n.get <= 44 => "Flaeche_Von_Cluster_bis44=1"
    case n if n.get  > 44 && n.get <= 60 => "Flaeche_Von_Cluster_44-60"
    case n if n.get  > 60 && n.get <= 75 => "Flaeche_Von_Cluster_60-75"
    case n if n.get  > 75 && n.get <= 85 => "Flaeche_Von_Cluster_75-85"
    case n if n.get  > 85 && n.get <= 100 => "Flaeche_Von_Cluster_85-100"
    case n if n.get  > 100 && n.get <= 130 => "Flaeche_Von_Cluster_100-130"
    case n if n.get  > 130 => "Flaeche_Von_Cluster_von130"
  }

  private def matchingGeo(geo: String): String = geo match {
    case "901" => "Geographie_AUT-PCD-901=1"
    case "902" => "Geographie_AUT-PCD-902=1"
    case "903" => "Geographie_AUT-PCD-903=1"
    case "904" => "Geographie_AUT-PCD-904=1"
    case "905" => "Geographie_AUT-PCD-905=1"
    case "906" => "Geographie_AUT-PCD-906=1"
    case "907" => "Geographie_AUT-PCD-907=1"
    case "908" => "Geographie_AUT-PCD-908=1"
    case "909" => "Geographie_AUT-PCD-909=1"
    case "910" => "Geographie_AUT-PCD-910=1"
    case "911" => "Geographie_AUT-PCD-911=1"
    case "912" => "Geographie_AUT-PCD-912=1"
    case "913" => "Geographie_AUT-PCD-913=1"
    case "914" => "Geographie_AUT-PCD-914=1"
    case "915" => "Geographie_AUT-PCD-915=1"
    case "916" => "Geographie_AUT-PCD-916=1"
    case "917" => "Geographie_AUT-PCD-917=1"
    case "918" => "Geographie_AUT-PCD-918=1"
    case "919" => "Geographie_AUT-PCD-919=1"
    case "920" => "Geographie_AUT-PCD-920=1"
    case "921" => "Geographie_AUT-PCD-921=1"
    case "922" => "Geographie_AUT-PCD-922=1"
    case "923" => "Geographie_AUT-PCD-923=1"
  }
}