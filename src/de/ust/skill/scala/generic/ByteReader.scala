/*  ___ _  ___ _ _                                                            *\
** / __| |/ (_) | |       Your SKilL Scala Binding                            **
** \__ \ ' <| | | |__     <<debug>>                                           **
** |___/_|\_\_|_|____|    by: <<some developer>>                              **
\*                                                                            */
package de.ust.skill.scala.generic

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import scala.collection.mutable.Stack
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader
import java.util.Arrays



case class ByteOffsetPosition(offset: Int) extends Position {
  final val line = 1
  def column = offset + 1
  def lineContents: String = ""
}

/**
 * @note the push/pop capabilities must be treated with care
 * @note the underlying implementation will very likely break backtracking capabilities of a combined parser
 *
 * @author Timm Felden
 */
final class ByteReader(val stream: FileChannel) extends Reader[Byte] {
  def this(path: Path) = this(Files.newByteChannel(path, StandardOpenOption.READ).asInstanceOf[FileChannel])
  // this buffer is reused for any allocation below 64bytes
  private val smallBuf = ByteBuffer.allocate(64)

  private var positions = new Stack[Long]
  /**
   * saves the current position onto a stack and jumps to the argument position
   */
  def push(next: Long) { positions.push(stream.position); stream.position(next) }
  /**
   * returns to the last position saved
   */
  def pop: Unit = stream.position(positions.pop)

  override def offset = stream.position().toInt
  def position = stream.position()

  override def first: Byte = throw new NoSuchMethodError("unsupported operation")
  override def rest: ByteReader = throw new NoSuchMethodError("unsupported operation")
  def pos: Position = ByteOffsetPosition(offset)
  def atEnd = stream.size() == stream.position()
  def minimumBytesToGo = stream.size() - stream.position()

  override def drop(n: Int): ByteReader = {
    if (has(n))
      stream.position(stream.position + n);
    else
      throw UnexpectedEOF(s"@$position while dropping $n bytes", null)
    this
  }
  /**
   * takes n bytes from the stream; the buffer may be invalid after the next call of this function
   */
  private[generic] def take(n: Int): ByteBuffer = {
    if (n < 64) {
      smallBuf.clear(); smallBuf.limit(n)
      stream.read(smallBuf); smallBuf.position(0)
      smallBuf
    } else {
      val r = ByteBuffer.allocate(n);
      stream.read(r); r.rewind();
      r
    }
  }
  /**
   * fills the argument buffer with input from the stream
   *
   * @note does not rewind the buffer
   */
  private[generic] def fill(buffer: ByteBuffer): Unit = stream.read(buffer)

  /**
   * like take, but creates a copy of the taken bytes to ensure correct usage
   */
  private[generic] def bytes(n: Int): Array[Byte] = {
    if (n < 64)
      Arrays.copyOf(take(n).array, n)
    else
      take(n).array
  }

  def has(n: Int): Boolean = stream.size() >= (n + stream.position())
  def next: Byte = try {
    take(1).get
  } catch { case e: Exception ⇒ throw UnexpectedEOF("there's no next byte", e) }

  def v64: Long = {
    var count: Long = 0
    var rval: Long = 0
    var r: Long = next
    while (count < 8 && 0 != (r & 0x80)) {
      rval |= (r & 0x7f) << (7 * count);

      count += 1;
      r = next
    }
    rval = (rval | (count match {
      case 8 ⇒ r
      case _ ⇒ (r & 0x7f)
    }) << (7 * count));
    rval
  }
}
