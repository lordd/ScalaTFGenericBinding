/*  ___ _  ___ _ _                                                            *\
** / __| |/ (_) | |       Your SKilL Scala Binding                            **
** \__ \ ' <| | | |__     <<debug>>                                           **
** |___/_|\_\_|_|____|    by: <<some developer>>                              **
\*                                                                            */
package de.ust.skill.scala.generic

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

/**
 * The type info objects are used to reflect the type information stored in a skill file. They are required for
 *  the deserialization of objects into storage pools.
 */
sealed abstract class TypeInfo {
  def toString(): String

  /**
   * serializable type ID according to skill §App.F
   */
  def typeId: Long

  override def equals(obj: Any) = if (obj.isInstanceOf[TypeInfo]) typeId == obj.asInstanceOf[TypeInfo].typeId else false
  override def hashCode = typeId.toInt
}

sealed abstract class ConstantIntegerInfo[T] extends TypeInfo {
  def value: T

  def expect(arg: T) = assert(arg == value)
}

case class ConstantI8Info(value: Byte) extends ConstantIntegerInfo[Byte] {
  override def toString(): String = "const i8 = "+("%02X" format value)

  def typeId: Long = 0
}
case class ConstantI16Info(value: Short) extends ConstantIntegerInfo[Short] {
  override def toString(): String = "const i16 = "+("%04X" format value)

  def typeId: Long = 1
}
case class ConstantI32Info(value: Int) extends ConstantIntegerInfo[Int] {
  override def toString(): String = "const i32 = "+value.toHexString

  def typeId: Long = 2
}
case class ConstantI64Info(value: Long) extends ConstantIntegerInfo[Long] {
  override def toString(): String = "const i64 = "+value.toHexString

  def typeId: Long = 3
}
case class ConstantV64Info(value: Long) extends ConstantIntegerInfo[Long] {
  override def toString(): String = "const v64 = "+value.toHexString

  def typeId: Long = 4
}

sealed abstract class IntegerInfo extends TypeInfo {}

case object I8Info extends IntegerInfo {
  override def toString(): String = "i8"

  def typeId: Long = 7
}
case object I16Info extends IntegerInfo {
  override def toString(): String = "i16"

  def typeId: Long = 8
}
case object I32Info extends IntegerInfo {
  override def toString(): String = "i32"

  def typeId: Long = 9
}
case object I64Info extends IntegerInfo {
  override def toString(): String = "i64"

  def typeId: Long = 10
}
case object V64Info extends IntegerInfo {
  override def toString(): String = "v64"

  def typeId: Long = 11
}

case object AnnotationInfo extends TypeInfo {
  override def toString(): String = "annotation"

  def typeId: Long = 5
}

case object BoolInfo extends TypeInfo {
  override def toString(): String = "bool"

  def typeId: Long = 6
}
case object F32Info extends TypeInfo {
  override def toString(): String = "f32"

  def typeId: Long = 12
}
case object F64Info extends TypeInfo {
  override def toString(): String = "f64"

  def typeId: Long = 13
}

case object StringInfo extends TypeInfo {
  override def toString = "string"

  def typeId: Long = 14
}

sealed abstract class CompoundTypeInfo extends TypeInfo {}

class ConstantLengthArrayInfo(val length: Int, var groundType: TypeInfo) extends CompoundTypeInfo {

  override def toString(): String = groundType+"["+length+"]"

  def typeId: Long = 15
}
class VariableLengthArrayInfo(var groundType: TypeInfo) extends CompoundTypeInfo {

  override def toString(): String = groundType+"[]"

  def typeId: Long = 17
}

class ListInfo(var groundType: TypeInfo) extends CompoundTypeInfo {

  override def toString(): String = "list<"+groundType+">"

  def typeId: Long = 18
}
class SetInfo(var groundType: TypeInfo) extends CompoundTypeInfo {

  override def toString(): String = "set<"+groundType+">"

  def typeId: Long = 19
}
class MapInfo(var groundType: List[TypeInfo]) extends CompoundTypeInfo {

  override def toString(): String = groundType.mkString("map<", ", ", ">")

  def typeId: Long = 20
}

/**
 * This type is used to ease deserialization a lot. There must not be any instances of this class in a usable reflection
 *  pool.
 */
case class PreliminaryUserType(val index: Long) extends TypeInfo {
  override def toString(): String = "<preliminary usertype: "+index+">"

  def typeId: Long = 32 + index
}

/**
 * Holds generic type definitions.
 *
 * @author Timm Felden
 */
case class TypeDefinition(
    val index: Long,
    val name: String,
    val superName: Option[String],
    var fields: HashMap[Long, FieldDefinition]) extends TypeInfo {

  def typeId: Long = 32 + index
}

final case class FieldDefinition(var t: TypeInfo, val name: String){
  override def toString = s"FieldDefinition[[${t match {
    case t:TypeDefinition ⇒ t.name
    case t ⇒ t.toString 
  }} $name]]"
}