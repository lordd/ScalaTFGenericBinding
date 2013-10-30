package de.ust.skill.scala.generic

import java.io.ByteArrayOutputStream
import java.io.File
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import scala.collection.mutable.HashMap

/**
 * The interface to fully generic access of skill files.
 *
 * @author Timm Felden
 */
final class State {
  type ⇀[A, B] = HashMap[A, B]

  /**
   * References are stored as (name:String, index:Long).
   */
  type refType = (String, Long)

  /**
   * ᚠ: typeName ⇀ instance index ⇀ field index ⇀ data
   */
  var fieldData = new (String ⇀ (Long ⇀ (Long ⇀ Any)))

  /**
   * Contains all type definitions by name.
   */
  var typeDefinitions = new (String ⇀ TypeDefinition)

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
              case u                    ⇒ u.toString
            }.mkString(", ")
          }]"
        }.mkString("{\n  ", "\n  ", "\n}")
      }"
    }.mkString("\n")
  }
τ:
${typeDefinitions.mkString("\n")}"""

  import State.v64

  /**
   * writes the state to a file, not using any append operations
   *
   * @note if one were ever to implement "append changes", fieldData and typeDefinitions need to be hidden behind a facade
   */
  def write(target: Path) {
    val file = Files.newByteChannel(target,
      StandardOpenOption.CREATE,
      StandardOpenOption.READ,
      StandardOpenOption.WRITE,
      StandardOpenOption.TRUNCATE_EXISTING).asInstanceOf[FileChannel]

    @inline def put(value: Long) = file.write(ByteBuffer.wrap(v64(value)))

    // write string pool
    writeStrings(file)

    // number of type definitions
    put(typeDefinitions.size)

    // write header to file and field data to out
    val out = new ByteArrayOutputStream
    val reverseStrings = fieldData("string")(0).toList.map { case (k, v: String) ⇒ (v, k) }.toMap
    // TODO type definitions have to be stored in type order!!
    typeDefinitions.values.foreach { td ⇒
      val fields = fieldData(td.name)
      put(reverseStrings(td.name))
      td.superName match {
        case None ⇒ put(0)
        case Some(name) ⇒
          put(reverseStrings(name))
          put(0)
      }
      put(fields.size)
      put(0)
      put(td.fields.size)
      td.fields.values.foreach { f ⇒
        put(0)
        put(f.t.typeId)
        put(reverseStrings(f.name))

        f.t match {
          case _ ⇒ // TODO
        }
        //        [[data]]
        //        out.positions
        put(out.size)
      }
    }

    // append data
    file.write(ByteBuffer.wrap(out.toByteArray()))

    // done:)
    file.close()
  }

  private[this] def writeStrings(file: FileChannel) {
    implicit val order = new Ordering[(Long, Any)] { override def compare(x: (Long, Any), y: (Long, Any)) = x._1.compare(y._1) }
    val strings = fieldData("string")(0).toBuffer.sorted.toList.unzip._2.collect { case s: String ⇒ s }

    val out = new ByteArrayOutputStream

    // number of instances
    file.write(ByteBuffer.wrap(v64(strings.size)))

    val header = ByteBuffer.allocate(4 * strings.length)

    // offsets & data
    for (s ← strings) {
      out.write(s.getBytes())
      header.putInt(out.size)
    }

    // append data
    header.rewind()
    file.write(header)
    file.write(ByteBuffer.wrap(out.toByteArray()))
  }

}

/**
 * Provides basic read capabilities for states.
 *
 * @author Timm Felden
 */
object State {
  def read(path: String): State = FileParser.read(new File(path).toPath);

  /**
   *  encode a v64 value into a stream
   */
  def v64(v: Long): Array[Byte] = {
    // calculate effective size
    var size = 0;
    {
      var q = v;
      while (q != 0) {
        q >>>= 7;
        size += 1;
      }
    }
    if (0 == size) {
      val rval = new Array[Byte](1);
      rval(0) = 0;
      return rval;
    } else if (10 == size)
      size = 9;

    // split
    val rval = new Array[Byte](size);
    var count = 0;
    while (count < 8 && count < size - 1) {
      rval(count) = (v >> (7 * count)).asInstanceOf[Byte];
      rval(count) = (rval(count) | 0x80).asInstanceOf[Byte];
      count += 1;
    }
    rval(count) = (v >> (7 * count)).asInstanceOf[Byte];
    return rval;
  }

  def v64(in: Array[Byte]): Long = {
    var next = 0
    var count = 0
    var rval: Long = 0
    var r: Long = in(next)
    next += 1
    while (count < 8 && 0 != (r & 0x80)) {
      rval |= (r & 0x7f) << (7 * count);

      count += 1;
      r = in(next)
      next += 1
    }
    rval = (rval | (count match {
      case 8 ⇒ r
      case _ ⇒ (r & 0x7f)
    }) << (7 * count));

    return rval
  }
}