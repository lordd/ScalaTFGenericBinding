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
import scala.collection.mutable.ListBuffer
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

  type ⇀[A, B] = HashMap[A, B]

  /**
   * the state to be created is shared across a file parser instance; file parser instances are used to turn a file into
   * a new state anyway.
   */
  private val σ = new State;
  private val fromReader = new ByteReader(Files.newByteChannel(path).asInstanceOf[FileChannel])

  // helper structures required to build user types

  // index(0+) → type
  private var userTypeIndexMap = new (Long ⇀ TypeDefinition)
  // name → type
  private var userTypeNameMap = σ.typeDefinitions
  // type → base type
  private var baseTypes = new (String ⇀ String)
  // type → index(1+)
  private var totalCount = new (String ⇀ Long)
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
  private var endOffsets = ListBuffer(0L)

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
      val map = σ.fieldData.get("string").getOrElse {
        val result = σ.newMap("string");
        result(0) = new HashMap[Long, Any];
        result
      }(0)

      offsets.foreach({ off ⇒
        val size = off - last
        last = off
        val s = new String(in.take(size).array, 0, size, "UTF-8")
        map.put(map.size + 1L, s)
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
      // eliminate preliminary user types
      fieldMap.values.foreach {
        fields ⇒
          fields.foreach {
            case (f, off, isNew) ⇒
              f.t = f.t match {
                case t: PreliminaryUserType ⇒ userTypeIndexMap(t.index)
                case t                      ⇒ t
              }
          }
      }

      // update fieldData
      val begin = if (endOffsets.size > 1)
        endOffsets.sorted.sliding(2).map { m ⇒ (m(1), m(0)) }.toMap
      else
        endOffsets.map { e ⇒ (e, 0L) }.toMap
      fieldMap.foreach {
        case (typeName, fields) ⇒
          val baseIndex = totalCount.get(baseTypes(typeName)).getOrElse {
            totalCount.put(baseTypes(typeName), 0L); 0L
          } + lbpsiMap(typeName) + 1L

          for (i ← 0 until fields.size) {
            fields(i) match {

              // regular case
              case (f, end, isNew) if (!isNew || 0 == totalCount(typeName)) ⇒ {
                val data = parseField(
                  f.t,
                  begin(end),
                  end,
                  countMap(typeName) + (if (isNew) totalCount(typeName) else 0L)
                )
                for (index ← 0 until data.length) {
                  val staticType = σ.fieldData(typeName)
                  val off = baseIndex + index;
                  staticType.get(off).getOrElse {
                    val result = new HashMap[Long, Any];
                    staticType(off) = result
                    result
                  }(i) = data(index)
                }
              }

              // appended field case; we have to iterate over existing instances, but we do not care for actual indices,
              // because correctness relies on their order only
              case (f, end, true) ⇒ {
                // get field data
                val data: ArrayBuffer[Any] = parseField(f.t, begin(end), end, countMap(typeName) + totalCount(typeName)).to
                implicit val ordering = new Ordering[(Long, (Long ⇀ Any))] {
                  override def compare(x: (Long, (Long ⇀ Any)), y: (Long, (Long ⇀ Any))) = x._1.compare(y._1)
                }

                // append type info to the type map, because this is the right place to do it
                val fieldIndex = userTypeNameMap(typeName).fields.size
                userTypeNameMap(typeName).fields(fieldIndex) = f

                // update all instances with the respective static type
                val staticType = σ.fieldData(typeName).toList.sorted
                val dataIterator = data.iterator
                val instances = staticType.iterator
                while (dataIterator.hasNext)
                  instances.next._2(fieldIndex) = dataIterator.next

              }
            }
          }
      }

      // update insert index map
      countMap.foreach { case (k, v) ⇒ totalCount(k) += v }

      // reset helper structures!
      countMap = new HashMap[String, Long]
      lbpsiMap = new HashMap[String, Long]
      fieldMap = new HashMap[String, ArrayBuffer[(FieldDefinition, Long, Boolean)]]
      endOffsets = ListBuffer(0L)

      // we finished a block
      blockCounter += 1
    }
    ()
  }

  private[this] def parseField(t: TypeInfo, begin: Long, end: Long, count: Long): Array[Any] = {
    def fieldParser = t match {
      case I8Info  ⇒ i8
      case I16Info ⇒ i16
      case I32Info ⇒ i32
      case I64Info ⇒ i64
      case V64Info ⇒ v64
      case AnnotationInfo ⇒ v64 ~ v64 ^^ {
        case 0L ~ _ ⇒ null
        case t ~ i  ⇒ (σ.getString(t), i)
      }
      case BoolInfo ⇒ i8 ^^ { b ⇒ b != 0 }
      case F32Info  ⇒ f32
      case F64Info  ⇒ f64
      case StringInfo ⇒ v64 ^^ {
        case 0L ⇒ null
        case i  ⇒ σ.getString(i)
      }

      //maps are rather complicated
      //      case d: MapInfo ⇒ parseMap(d.groundType)

      // user types are just references, easy pray
      case d: TypeDefinition ⇒ v64 ^^ {
        _ match {
          case 0     ⇒ null
          case index ⇒ (d.name, index)
        }

      }

      case other ⇒ throw new IllegalStateException(
        s"preliminary usertypes and constants should already have been eleminitad; found $other")
    }

    // TODO better error message
    //    assert(fromReader.position == begin, "illegal begin index; non-monotone index order???")

    val result = repN(count.toInt, fieldParser)(fromReader).get.toArray

    // TODO better error message
    //    assert(fromReader.position == end, "illegal end index")

    result
  }

  /**
   * see skill ref man §6.2
   *
   * @note restrictions are dropped anyway
   */
  private[this] def typeDeclaration = (v64 ^^ { i ⇒ σ(i) }) >> { name ⇒
    var count = 0L
    // check if we append to an existing type
    userTypeNameMap.get(name) match {
      case None ⇒ {
        (v64 >> (_ match {
          case 0L    ⇒ success(None: Option[String]) ~ success(0L)
          case index ⇒ success(Some(σ(index))) ~ v64
        })) ~ (v64 ^^ { c ⇒ count = c; c }) ~ restrictions ~ fieldDeclarations(None, count) ^^ {
          case s ~ l ~ c ~ r ~ f ⇒
            fieldMap.put(name, f.to)
            countMap.put(name, c)
            lbpsiMap.put(name, l)

            var fields = new HashMap[Long, FieldDefinition]
            val fieldIterator = f.iterator
            for (i ← 0 until f.size)
              fields.put(i.toLong, fieldIterator.next._1)

            val t = TypeDefinition(userTypeIndexMap.size, name, s, fields)
            userTypeIndexMap.put(t.index, t)
            userTypeNameMap.put(t.name, t)
            σ.newMap(t.name)
            t.superName match {
              case None    ⇒ baseTypes.put(t.name, t.name)
              case Some(s) ⇒ baseTypes.put(t.name, baseTypes(s))
            }
            ()
        }
      }
      case Some(typeDef) ⇒ (typeDef.superName match {
        case None ⇒ success(0L)
        case sn   ⇒ v64
      }) ~ (v64 ^^ { c ⇒ count = c; c }) ~ fieldDeclarations(Some(typeDef), count) ^^ {
        case l ~ c ~ f ⇒
          fieldMap.put(name, f.to)
          countMap.put(name, c)
          lbpsiMap.put(name, l)
          ()
      }
    }
  }

  private[this] def fieldDeclarations(t: Option[TypeDefinition], instanceCount: Long): Parser[List[(FieldDefinition, Long, Boolean)]] =
    v64 >> { count ⇒
      if (0 == instanceCount || t.isEmpty) {
        repN(count.toInt, fieldDeclaration)
      } else {
        val Some(typeDef) = t
        // we have to iterate manually, because there is no sorted mutable map atm
        val knownFields = for (i ← 0 until typeDef.fields.size) yield typeDef.fields(i)
        val it = knownFields.iterator
        def fieldParser = new Parser[(FieldDefinition, Long, Boolean)] {
          def apply(in: Input) = if (it.hasNext) {
            val e = in.v64
            endOffsets += e
            Success((it.next, e, false), in)
          } else {
            Success(fieldDeclaration(in).get, in)
          }
        }
        repN(count.toInt, fieldParser);
      }
    }
  private[this] def fieldDeclaration = restrictions ~ fieldTypeDeclaration ~ (v64 ^^ { i ⇒ σ(i) }) ~ v64 ^^ {
    case r ~ t ~ n ~ e ⇒
      endOffsets += e
      (FieldDefinition(t, n), e, true)
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
      case 0            ⇒ i8 ^^ { ConstantI8Info(_) }
      case 1            ⇒ i16 ^^ { ConstantI16Info(_) }
      case 2            ⇒ i32 ^^ { ConstantI32Info(_) }
      case 3            ⇒ i64 ^^ { ConstantI64Info(_) }
      case 4            ⇒ v64 ^^ { ConstantV64Info(_) }
      case 5            ⇒ success(AnnotationInfo)
      case 6            ⇒ success(BoolInfo)
      case 7            ⇒ success(I8Info)
      case 8            ⇒ success(I16Info)
      case 9            ⇒ success(I32Info)
      case 10           ⇒ success(I64Info)
      case 11           ⇒ success(V64Info)
      case 12           ⇒ success(F32Info)
      case 13           ⇒ success(F64Info)
      case 14           ⇒ success(StringInfo)
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
      case 5            ⇒ AnnotationInfo
      case 6            ⇒ BoolInfo
      case 7            ⇒ I8Info
      case 8            ⇒ I16Info
      case 9            ⇒ I32Info
      case 10           ⇒ I64Info
      case 11           ⇒ V64Info
      case 12           ⇒ F32Info
      case 13           ⇒ F64Info
      case 14           ⇒ StringInfo
      case i if i >= 32 ⇒ PreliminaryUserType(i - 32)
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
  def read(path: Path): State = {
    val p = new FileParser(path)
    try { p.readFile } catch { case e: Exception ⇒ println(p.σ.toString); throw e }
  }
}
