package models

/**
 * Created by cschackerl on 23.07.2015.
 */
class AssociationRule(val support: Double, val confidence: Double, val lift: Double, val antecedent: Int, val consequent: Int) {
  override def toString =  s"AssociationRule ... support: $support, confidence: $confidence, lift: $lift, antecedent: $antecedent, consequent: $consequent"
}
object AssociationRule

