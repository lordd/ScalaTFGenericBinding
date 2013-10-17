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

sealed abstract class ConstantIntegerInfo[T](private val _value: T) extends TypeInfo {
  def value = _value

  def expect(arg: T) = assert(arg == value)
}

class ConstantI8Info(value: Byte) extends ConstantIntegerInfo[Byte](value) {
  override def toString(): String = "const i8 = "+("%02X" format value)

  def typeId: Long = 0
}
class ConstantI16Info(value: Short) extends ConstantIntegerInfo[Short](value) {
  override def toString(): String = "const i16 = "+("%04X" format value)

  def typeId: Long = 1
}
class ConstantI32Info(value: Int) extends ConstantIntegerInfo[Int](value) {
  override def toString(): String = "const i32 = "+value.toHexString

  def typeId: Long = 2
}
class ConstantI64Info(value: Long) extends ConstantIntegerInfo[Long](value) {
  override def toString(): String = "const i64 = "+value.toHexString

  def typeId: Long = 3
}
class ConstantV64Info(value: Long) extends ConstantIntegerInfo[Long](value) {
  override def toString(): String = "const v64 = "+value.toHexString

  def typeId: Long = 4
}

sealed abstract class IntegerInfo extends TypeInfo {}

class I8Info extends IntegerInfo {
  override def toString(): String = "i8"

  def typeId: Long = 7
}
class I16Info extends IntegerInfo {
  override def toString(): String = "i16"

  def typeId: Long = 8
}
class I32Info extends IntegerInfo {
  override def toString(): String = "i32"

  def typeId: Long = 9
}
class I64Info extends IntegerInfo {
  override def toString(): String = "i64"

  def typeId: Long = 10
}
class V64Info extends IntegerInfo {
  override def toString(): String = "v64"

  def typeId: Long = 11
}

class AnnotationInfo extends TypeInfo {
  override def toString(): String = "annotation"

  def typeId: Long = 5
}

class BoolInfo extends TypeInfo {
  override def toString(): String = "bool"

  def typeId: Long = 6
}
class F32Info extends TypeInfo {
  override def toString(): String = "f32"

  def typeId: Long = 12
}
class F64Info extends TypeInfo {
  override def toString(): String = "f64"

  def typeId: Long = 13
}

class StringInfo extends TypeInfo {
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
class PreliminaryUserType(val index: Long) extends TypeInfo {
  override def toString(): String = "<preliminary usertype: "+index+">"

  def typeId: Long = 32 + index
}

class UserType(
  val index: Int,
  val name: String,
  val superName: Option[String])
    extends TypeInfo {
  // convenience constructor to create a usertype from a map of fields
  def this(index: Int, name: String, superName: Option[String], fields: HashMap[String, FieldDefinition]) = {
    this(index, name, superName)
    fields.foreach { case (_, f) ⇒ addField(f) }
  }

  // Total number of instances seen until now.
  var instanceCount = 0L;

  private final var _fields = new HashMap[String, FieldDefinition]
  private final var _fieldByIndex = new HashMap[Int, FieldDefinition]
  def fields = _fields
  def fieldByIndex(i: Int) = _fieldByIndex(i)
  def addField(f: FieldDefinition) {
    _fields.put(f.name, f)
    _fieldByIndex.put(_fieldByIndex.size, f)
  }

  // various links between types
  var baseType: UserType = superName match { case None ⇒ this; case _ ⇒ null }
  var superType: UserType = null
  var subTypes = new ArrayBuffer[UserType]

  /**
   * Pretty printed skill declaration of this user type.
   */
  def getDeclaration(): String = {
    var r = new StringBuilder(name)
    if (superName.isDefined)
      r ++= ":" ++= superName.get

    r ++= "{"
    fields.values.foreach({ f ⇒ r.append(s" ${f.typeName} ${f.name};") });
    r ++= "}"

    return r.toString
  }

  override def toString = name

  // TODO provide an actual implementation, which allows for calculating the correct type IDs in the preparation phase
  def typeId: Long = 32 + index
}
