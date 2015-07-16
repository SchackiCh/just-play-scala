package controllers

import play.api.mvc._
import play.api.libs.json._
import java.io._
import scala.collection.mutable.ListBuffer
import scala.xml.XML

object Application extends Controller {
  val sourceXml : String = new java.io.File(".").getCanonicalPath() + "/app/assets/Rules.xml"  //"C:/github/just-play-scala/app/assets/Rules.xml"
  val rawXml = scala.xml.XML.loadFile(sourceXml)

  class Rule(var id: Integer, var value: String) {
    override def toString =
      s"id: $id, value: $value"
  }
  object Rule

  class Item(var id: String, var value: String) {
    override def toString =
      s"id:$id, value:$value"
  }
  object Item

  class Items(var allItems: Seq[Item]){
    override def toString = (for (v <- allItems) yield sys.props("line.separator") + v.id + ": " + v.value).toString()
  }
  object Items {

    // convert XML to an Item object
    def fromEntireXml(node: scala.xml.Node):Items = {
      val allItems = (node \\ "PMML" \\ "AssociationModel" \\ "Item" ).map( x => new Item((x \\ "@id").toString(),(x \ "@value").toString()))
      new Items(allItems)
    }

  }

  class Itemset(var id: String, var itemRefs: Seq[String]) {
    override def toString =
      s"id: $id, itemRefs: $itemRefs.toString()"
  }
  object Itemset

  class Itemsets(var itemsets: Seq[Itemset]){
    override def toString = (for (is <- itemsets) yield sys.props("line.separator") + "ItemSetId:" + is.id + ": " + {
      for(itemRef <- is.itemRefs) yield itemRef.toString()
    }).toString()
  }
  object Itemsets {

    // convert XML to an Item object
    def fromEntireXml(node: scala.xml.Node):Itemsets = {
      val itemsets = (node \\ "PMML" \\ "AssociationModel" \\ "Itemset" )
        .map( x => new Itemset((x \\ "@id").toString(),(x \\ "ItemRef").map(iref =>(x \\ "@itemRef").toString())))
      new Itemsets(itemsets)
    }
  }


  case class SearchParameter(id: Int, name: String)
    
  val searchParameters = List(
        SearchParameter(1, "Fabio"),
        SearchParameter(2, "Reinhard"),
        SearchParameter(3, "Christian")
  )

    //implicit val placesWrites = Json.writes[SearchParameter]
    
  def items = Action {
        
        //val json = Json.toJson(searchParameters)
        //Ok(json)
        //val rawXml = scala.xml.XML.loadFile(sourceXml)
        
        // find what i want
        //var items: List[String] = List("apples", "oranges", "pears")
       
        //GUT
        //val value = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@value" )
        //val value = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@value" ).map(_.text).mkString(" ")


      //val value = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@value")
        //val new = value.foreach( item => value)
        //.foreach( item =>
        //   items = items ::: List(item)  )
           
        //Ok(items.toString)
        //val value2 = (rules \\ "PMML" \\ "AssociationModel" \  "@functionName") text
     
        
        //Ok((rules \\ "PMML" \\ "AssociationModel" ))
        
        
        
        //val x = <div class="content"><p>Hello</p><p>world</p></div>
        //val result = (x \\ "p")
        //val result2 =for (n <- x.child) yield n.text
        
        //val a = Array(1, 2, 3, 4, 5)
        
        
        //Ok(for (e <- a) yield e)
        //Ok(value.toString)


      //-------------
      //http://alvinalexander.com/scala/serializing-deserializing-xml-scala-classes


      //var value = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@value" ).map(_.text).mkString(" ")
      // value = value +  (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@id" ).map(_.text).mkString(" ")
      //val value = rules.attributes("version")


      //var values = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@value" ).map(_.text)
      //var ids = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" \\ "@id" ).map(_.text)

      //Get all Items
      //var value = (rules \\ "PMML" \\ "AssociationModel" \\ "Item" ).map( x => List((x \\ "@id"),(x \ "@value")))
      //val result =(for (v <- value) yield v.head.toString() + ": " + v(1).toString()).toString()

      //val result = new Item("3", "juhu test")
      val result = Items.fromEntireXml(rawXml)
      Ok(result.toString)
  }

  def itemsets = Action {
    //val rawXml = scala.xml.XML.loadFile(sourceXml)
    val result = Itemsets.fromEntireXml(rawXml)
    Ok(result.toString)
  }

  def rules = TODO

  def index = Action {
    Ok(views.html.main())
  }

  def getNewSearch(hasTerrace: Boolean, hasLift: Boolean) = Action {
    //map parameters to item values/ids
    val input: ListBuffer[String] = ListBuffer()
    if(hasTerrace) {
      input += "Terrasse=terrasse:ja"
    }
    if(hasLift) {
      input += "Lift=lift:ja"
    }

    //find items and get there ids
    //find itemsets with this parameters
    //find rules and get the consequent
    //get items from itemsets and return it
    Ok(input.toString())
  }
}
