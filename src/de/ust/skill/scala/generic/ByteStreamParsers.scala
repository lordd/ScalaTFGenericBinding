/*  ___ _  ___ _ _                                                            *\
** / __| |/ (_) | |       Your SKilL Scala Binding                            **
** \__ \ ' <| | | |__     <<debug>>                                           **
** |___/_|\_\_|_|____|    by: <<some developer>>                              **
\*                                                                            */
package de.ust.skill.scala.generic

import java.util.Arrays

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

/**
 * @note This parser does not support backtracking. If one would be interest in backtracking the atomic parsers should
 *  use the in.mark and in.reset methods if they fail.
 *
 * @author Timm Felden
 */
class ByteStreamParsers extends Parsers {
  type Elem = Byte
  protected implicit def readerToByteReader(x: Input): ByteReader = x.asInstanceOf[ByteReader]

  def hasMore(): Parser[Unit] = new Parser[Unit] {
    def apply(in: Input) = {
      if (in.atEnd)
        Failure("EOF", in)
      else
        Success((), in)
    }
  }

  def bytes(n: Int): Parser[Array[Byte]] = new Parser[Array[Byte]] {
    def apply(in: Input) = {
      if (in has n)
        Success(Arrays.copyOf((in take n).array, n), in)
      else
        throw UnexpectedEOF(s"while taking $n bytes", null)
    }
  }
  def bytes(n: Long): Parser[Array[Byte]] = bytes(n.asInstanceOf[Int])

  def i8 = new Parser[Byte] {
    def apply(in: Input) = {
      if (in has 1)
        Success(in.next, in)
      else
        throw UnexpectedEOF("while reading i8", null)
    }
  }
  def i16 = new Parser[Short] {
    def apply(in: Input) = {
      if (in has 2)
        Success((in take 2).getShort, in)
      else
        throw UnexpectedEOF("while reading i16", null)
    }
  }
  def i32 = new Parser[Int] {
    def apply(in: Input) = {
      if (in has 4)
        Success((in take 4).getInt, in)
      else
        throw UnexpectedEOF("while reading i32", null)
    }
  }
  def i64 = new Parser[Long] {
    def apply(in: Input) = {
      if (in has 8)
        Success((in take 8).getLong, in)
      else
        throw UnexpectedEOF("while reading i64", null)
    }
  }

  def v64 = new Parser[Long] {
    def apply(in: Input) = try {
      Success(in.v64, in)
    } catch {
      case e: Exception ⇒ throw UnexpectedEOF("while reading v64", e)
    }
  }

  def f32 = new Parser[Float] {
    def apply(in: Input) = {
      if (in has 4)
        Success((in take 4).getFloat, in)
      else
        throw UnexpectedEOF("while reading f32", null)
    }
  }
  def f64 = new Parser[Double] {
    def apply(in: Input) = {
      if (in has 8)
        Success((in take 8).getDouble, in)
      else
        throw UnexpectedEOF("while reading f64", null)
    }
  }

  def string(σ: State) = v64 ^^ {
    _ match {
      case 0 ⇒ null
      case i ⇒ σ.getString(i)
    }
  }
}
