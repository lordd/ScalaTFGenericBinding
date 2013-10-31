package de.ust.skill.scala.generic

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

/**
 * A collection of operators on states.
 *
 * @author Timm Felden
 */
object StateOperators {

  type →[A, B] = HashMap[A, B]

  val undefined = "⊥"

  /**
   * Checks equivalence of two states.
   *
   * @note very inefficient
   */
  def equivalent(x: State, y: State): Boolean = diff(x, y).isEmpty

  /**
   * Checks if state >= subState
   */
  def subset(state: State, subState: State): Boolean = diff(state, subState).forall {
    case DiffEntry(_, _, _, _, StateOperators.undefined) ⇒ true
    case _                            ⇒ false
  }

  case class DiffEntry(typeName: String, fieldName: String, index: Long, value1: Any, value2: Any) {
    override def toString = s"$typeName($index).$fieldName: $value1 <> $value2"
  }
  /**
   * Calculates the difference between two states.
   */
  def diff(x: State, y: State): Iterable[DiffEntry] = {

    def flatten(s: State) = for {
      (name, instances) ← s.fieldData
      (index, fields) ← instances
      (fieldIndex, value) ← fields
    } yield (name, index, fieldIndex, value)

    val sx: Set[(String, Long, Long, Any)] = flatten(x).to
    val sy: Set[(String, Long, Long, Any)] = flatten(y).to
    val d = (sx diff sy) union (sy diff sx)

    def tryGet(s: State, name: String, index: Long, fieldIndex: Long) = try {
      s.fieldData(name)(index)(fieldIndex)
    } catch {
      case e: Exception ⇒ undefined
    }

    (sx diff sy).map {
      case ("string", index, fieldIndex, v) ⇒ DiffEntry("string", "#", index, v, tryGet(y, "string", index, fieldIndex))
      case (name, index, fieldIndex, v) ⇒
        DiffEntry(name, x.typeDefinitions(name).fields(fieldIndex).name, index, v, tryGet(y, name, index, fieldIndex))
    } union
      (sy diff sx).map {
        case ("string", index, fieldIndex, v) ⇒ DiffEntry("string", "#", index, tryGet(x, "string", index, fieldIndex), v)
        case (name, index, fieldIndex, v) ⇒
          DiffEntry(name, y.typeDefinitions(name).fields(fieldIndex).name, index, tryGet(x, name, index, fieldIndex), v)
      }
  }

}