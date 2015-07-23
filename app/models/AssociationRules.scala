package models

/**
 * Created by cschackerl on 23.07.2015.
 */
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
