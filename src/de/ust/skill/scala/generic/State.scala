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
   * Contains all field data.
   */
  var fehu = new HashMap[String, HashMap[Long, HashMap[Long, Any]]]

  /**
   * Contains all type definitions by name.
   */
  var tau = new HashMap[String, TypeDefinition]

  /**
   * fehu bypass
   */
  def getString(index: Long): String = fehu("string")(0)(index).asInstanceOf[String]

  /**
   * little helper for updating fehu
   */
  private[generic] def newMap(name: String) = {
    val result = new HashMap[Long, HashMap[Long, Any]]
    fehu(name) = result
    result
  }

  override def toString = s"ᚠ = ${fehu.toString}\nτ = ${tau.toString}"
}

/**
 * Provides basic read capabilities for states.
 *
 * @author Timm Felden
 */
object State {
  def read(path: String): State = FileParser.read(new File(path).toPath)
}