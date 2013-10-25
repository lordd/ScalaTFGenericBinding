package de.ust.skill.scala.generic

import scala.collection.mutable.HashMap
import java.io.File

/**
 * The interface to fully generic access of skill files.
 *
 * @author Timm Felden
 */
final class State {
  /**
   * References are stored as (name:String, index:Long).
   */
  type refType = (String, Long)

  /**
   * ᚠ: typeName ⇀ instance index ⇀ field index ⇀ data
   */
  var fieldData = new HashMap[String, HashMap[Long, HashMap[Long, Any]]]

  /**
   * Contains all type definitions by name.
   */
  var typeDefinitions = new HashMap[String, TypeDefinition]

  /**
   * field data bypass
   */
  def getString(index: Long): String = fieldData("string")(0)(index).asInstanceOf[String]

  /**
   * little helper for updating fehu
   */
  private[generic] def newMap(name: String) = {
    val result = new HashMap[Long, HashMap[Long, Any]]
    fieldData(name) = result
    result
  }

  override def toString = s"""ᚠ:
${
    fieldData.map {
      case ("string", v) ⇒ s"string pool→${
        v(0).map { case (k, v) ⇒ s"""#$k:"$v"""" }.mkString("{", ", ", "}")
      }}"
      case (k, v) ⇒ s"$k: ${
        v.map {
          case (k, v) ⇒ s"#$k: [${
            v.values.map {
              case (n: String, i: Long) ⇒ s"$n#$i"
              case s: String            ⇒ s""""$s""""
              case _                    ⇒ "<<needs implementation>>"
            }.mkString(", ")
          }]"
        }.mkString("{\n  ", "\n  ", "\n}")
      }"
    }.mkString("\n")
  }
τ:
${typeDefinitions.mkString("\n")}"""
}

/**
 * Provides basic read capabilities for states.
 *
 * @author Timm Felden
 */
object State {
  def read(path: String): State = FileParser.read(new File(path).toPath);
}