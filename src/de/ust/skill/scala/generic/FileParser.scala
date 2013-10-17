/*  ___ _  ___ _ _                                                            *\
** / __| |/ (_) | |       Your SKilL Scala Binding                            **
** \__ \ ' <| | | |__     <<debug>>                                           **
** |___/_|\_\_|_|____|    by: <<some developer>>                              **
\*                                                                            */
package de.ust.skill.scala.generic

import java.nio.channels.FileChannel
import java.nio.file.Files
import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.language.implicitConversions

/**
 * see skill ref man §6.2.
 *
 * The file parser reads a file and gathers type declarations.
 * It creates a new SerializableState which contains types for these type declaration.
 *
 * @author Timm Felden
 */
final private class FileParser(path: Path) extends ByteStreamParsers {

  /**
   * the state to be created is shared across a file parser instance; file parser instances are used to turn a file into
   * a new state anyway.
   */
  private val σ = new State;
  private val fromReader = new ByteReader(Files.newByteChannel(path).asInstanceOf[FileChannel])

  // helper structures required to build user types

  // index(0+) → type
  private var userTypeIndexMap = new HashMap[Long, TypeDefinition]
  // name → type
  private var userTypeNameMap = σ.tau
  // type → base type
  private var baseTypes = new HashMap[String, String]
  // base type → index(1+) [[also: base type → finished count]]
  private var lastValidIndex = new HashMap[String, Long]
  // the current block
  private var blockCounter = 0;

  // helper structures which are reseted after each block

  // name → count
  private var countMap = new HashMap[String, Long]
  // name ⇀ lbpsi (partial!)
  private var lbpsiMap = new HashMap[String, Long]
  // name → List(Field, EndOffset, New?)
  private var fieldMap = new HashMap[String, ArrayBuffer[(FieldDefinition, Long, Boolean)]]
  // list of end-offsets
  private var endOffsets = List(0L)

  /**
   * @param σ the processed serializable state
   * @return a function that maps logical indices to the corresponding strings
   */
  private[this] implicit def poolAccess(σ: State): (Long ⇒ String) = σ.getString(_)

  /**
   * turns a file into a raw serializable state
   *
   * @note hasMore is required for proper error reporting
   */
  private def file: Parser[State] = rep(hasMore ~> stringBlock ~ typeBlock) ^^ { _ ⇒ σ }

  /**
   * reads a string block
   */
  private def stringBlock: Parser[Unit] = v64 >> { count ⇒ repN(count.toInt, i32) } >> stringBlockData

  /**
   * reads individual strings
   */
  private def stringBlockData(offsets: List[Int]) = new Parser[Unit] {
    def apply(in: Input) = {
      var last = 0
      val map = σ.fehu.get("strings").getOrElse(σ.newMap("strings"))

      offsets.foreach({ off ⇒
        val s = new String(in.take(off - last).array, "UTF-8")
        map.put(map.size + 1L, HashMap[Long, Any]((0, s)))
      })

      Success((), in)
    }
  }

  /**
   * reads a type block and adds the contained type information to the pool.
   * At the moment, it will process fields as well, because we do not have a good random access file stream
   *  implementation, yet.
   */
  private[this] def typeBlock: Parser[Unit] = (v64 >> { count ⇒
    repN(count.toInt, typeDeclaration)
  }) ^^ { _ ⇒
    if (!fieldMap.isEmpty) {
      // TODO read field data

      // reset helper structures!

    }
    ()
  }

  /**
   * see skill ref man §6.2
   *
   * @note restrictions are dropped anyway
   */
  private[this] def typeDeclaration = (v64 ^^ { i ⇒ σ(i) }) >> { name ⇒
    // check if we append to an existing type
    (userTypeNameMap.get(name) match {
      case None ⇒ {
        (v64 >> (_ match {
          case 0L    ⇒ success(None: Option[String]) ~ success(0L)
          case index ⇒ success(Some(σ(index))) ~ v64
        })) ~ v64 ~ restrictions ~ fieldDeclarations(name)
      }
      case Some(typeDef) ⇒ (typeDef.superName match {
        case None ⇒ success(None: Option[String]) ~ success(0L)
        case sn   ⇒ success(sn) ~ v64
      }) ~ v64 ~ success(List[String]()) ~ fieldDeclarations(name)
    }) ^^ {
      case s ~ l ~ c ~ r ~ f ⇒
        // TODO
        ()
    }
  }

  private[this] def fieldDeclarations(typeName: String) = v64 >> { count ⇒ repN(count.toInt, fieldDeclaration(typeName)) }
  private[this] def fieldDeclaration(typeName: String) = restrictions ~ fieldTypeDeclaration ~ (v64 ^^ { i ⇒ σ(i) }) ~ v64 ^^ {
    case r ~ t ~ n ~ e ⇒
      val isNew = true
      // TODO 
      (FieldDefinition(t.toString, n), e, isNew)
  }

  /**
   * restrictions are currently restored to their textual representation
   */
  private[this] def restrictions: Parser[List[String]] = v64 >> { i ⇒ repN(i.toInt, restriction) }
  private[this] def restriction = v64 >> { i ⇒
    i match {
      case 0 ⇒ try { v64 ~ v64 ~ v64 ^^ { case l ~ r ~ b ⇒ s"range(${σ(l)}, ${σ(r)}, ${σ(b)})" } }
      catch { case e: Exception ⇒ throw new SkillException("malformed range extension", e) }

      case 1  ⇒ success("nullable")
      case 2  ⇒ success("unique")
      case 3  ⇒ success("singleton")
      case 4  ⇒ success("constantLengthPointer")
      case 5  ⇒ success("monotone")
      case id ⇒ throw new java.lang.Error(s"Restrictions ID $id not yet supported!")
    }
  }

  /**
   * Turns a field type into a preliminary type information. In case of user types, the declaration of the respective
   *  user type may follow after the field declaration.
   */
  private def fieldTypeDeclaration: Parser[TypeInfo] = v64 >> { i ⇒
    i match {
      case 0            ⇒ i8 ^^ { new ConstantI8Info(_) }
      case 1            ⇒ i16 ^^ { new ConstantI16Info(_) }
      case 2            ⇒ i32 ^^ { new ConstantI32Info(_) }
      case 3            ⇒ i64 ^^ { new ConstantI64Info(_) }
      case 4            ⇒ v64 ^^ { new ConstantV64Info(_) }
      case 5            ⇒ success(new AnnotationInfo())
      case 6            ⇒ success(new BoolInfo())
      case 7            ⇒ success(new I8Info())
      case 8            ⇒ success(new I16Info())
      case 9            ⇒ success(new I32Info())
      case 10           ⇒ success(new I64Info())
      case 11           ⇒ success(new V64Info())
      case 12           ⇒ success(new F32Info())
      case 13           ⇒ success(new F64Info())
      case 14           ⇒ success(new StringInfo())
      case 15           ⇒ v64 ~ baseTypeInfo ^^ { case i ~ t ⇒ new ConstantLengthArrayInfo(i.toInt, t) }
      case 17           ⇒ baseTypeInfo ^^ { new VariableLengthArrayInfo(_) }
      case 18           ⇒ baseTypeInfo ^^ { new ListInfo(_) }
      case 19           ⇒ baseTypeInfo ^^ { new SetInfo(_) }
      case 20           ⇒ v64 >> { n ⇒ repN(n.toInt, baseTypeInfo) } ^^ { new MapInfo(_) }
      case i if i >= 32 ⇒ success(new PreliminaryUserType(i - 32))
      case id           ⇒ throw ParseException(fromReader, blockCounter, s"Invalid type ID: $id")
    }
  }

  /**
   * matches only types which are legal arguments to ADTs
   */
  private def baseTypeInfo: Parser[TypeInfo] = v64 ^^ { i ⇒
    i match {
      case 5            ⇒ new AnnotationInfo()
      case 6            ⇒ new BoolInfo()
      case 7            ⇒ new I8Info()
      case 8            ⇒ new I16Info()
      case 9            ⇒ new I32Info()
      case 10           ⇒ new I64Info()
      case 11           ⇒ new V64Info()
      case 12           ⇒ new F32Info()
      case 13           ⇒ new F64Info()
      case 14           ⇒ new StringInfo()
      case i if i >= 32 ⇒ new PreliminaryUserType(i - 32)
    }
  }

  def readFile: State = {
    file(fromReader) match {
      case Success(r, i) ⇒ r
      case NoSuccess(msg, i) ⇒ throw new SkillException(
        s"""Failed to parse ${path}:
  Message: $msg
  Got stuck at byte ${fromReader.pos.column} with at least ${fromReader.minimumBytesToGo} bytes to go.
  The next Byte is a ${try { fromReader.next.toHexString } catch { case _: Exception ⇒ "EOF" }}.
        """)
    }
  }
}

object FileParser extends ByteStreamParsers {
  import FileParser._

  /**
   * reads the contents of a file, creating a new state
   */
  def read(path: Path): State = (new FileParser(path)).readFile
}
