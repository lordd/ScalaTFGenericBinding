package de.ust.skill.scala.generic

import scala.collection.mutable.HashMap

/**
 * Holds generic type definitions.
 *
 * @author Timm Felden
 */
final case class TypeDefinition(name: String, superName: Option[String], fields: HashMap[Long, FieldDefinition])

final case class FieldDefinition(typeName: String, name: String)