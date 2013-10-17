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
   * creates storage pools in type order
   */
  //  private def makeState() {
  //    def makePool(t: UserType): AbstractPool = {
  //      val result = t.name match {
  //        case "test" ⇒ new TestStoragePool(t, σ, blockCounter)
  //        case "date" ⇒ new DateStoragePool(t, σ, blockCounter)
  //
  //        case _ ⇒ new GenericPool(t, t.superName match {
  //          case Some(name) ⇒ σ.pools.get(name).ensuring(_.isDefined)
  //          case None       ⇒ None
  //        }, blockCounter)
  //      }
  //      σ.pools.put(t.name, result)
  //      t.subTypes.foreach(makePool(_))
  //      result
  //    }
  //
  //    // make base pools; the makePool function makes sub pools
  //    userTypeIndexMap.values.filter(_.superName.isEmpty).foreach(makePool(_) match {
  //      case p: KnownPool[_, _] ⇒ p.constructPool
  //      case p                  ⇒ println(s"unknown pool $p")
  //    })
  //
  //    // read eager fields
  //    val fp = new FieldParser(σ)
  //    σ.knownPools.foreach(_.readFields(fp))
  //
  //    // ensure presence of the specified types
  //    σ.finishInitialization
  //  }
  //
  /**
   * A type declaration, as it occurs during parsing of a type blocks header.
   *
   * @author Timm Felden
   */
  private class TypeDeclaration(
      val name: String,
      val superName: Option[String],
      val lbpsi: Long,
      val count: Long /*, restrictions*/ ,
      val fieldCount: Long,
      // null iff the type occurred first in this block
      var userType: TypeDefinition) {

    // ensure presence of lbpsi type start indices
    if (superName.isEmpty && !localBlockBaseTypeStartIndices.contains(name))
      localBlockBaseTypeStartIndices.put(name, 0L)

    /**
     * Field declarations obtained from the header.
     */
    var fieldDeclarations: List[FieldDefinition] = null

    /**
     * creates a user type, if non exists
     */
    def mkUserType() {
      // check for duplicate definitions
      if (userTypeNameMap.contains(name))
        throw ParseException(fromReader, blockCounter, s"Duplicate definition of type $name")

      // create a new user type
      //      userType = new UserType(
      //        userTypeIndexMap.size,
      //        name,
      //        superName,
      //        new HashMap() ++= fieldDeclarations.map { f ⇒ (f.name, f) }
      //      )

      userTypeIndexMap.put(userTypeIndexMap.size, userType)
      userTypeNameMap.put(userType.name, userType)
    }

    /**
     * Reads field declarations matching this type declaration from a stream, based on the state σ
     *
     * TODO treat restrictions
     */
    def parseFieldDeclarations: Parser[TypeDeclaration] = {
      // if we have new instances and the type existed and there are new fields, we get into special cases
      if (null != userType && count > 0) {
        val knownFields = userType.fields.size
        if (0 > fieldCount - knownFields)
          throw ParseException(fromReader, blockCounter, s"Type $name has $fieldCount fields (requires $knownFields)")

        var fieldIndex = 0

        repN(knownFields.toInt,
          v64 ^^ { end ⇒
            var result = userType.fields(fieldIndex)
            //            result.end = end

            result
          }
        ) >> { fields ⇒
            repN((fieldCount - knownFields).toInt, restrictions ~ fieldTypeDeclaration ~ v64 ~ v64 ^^ {
              case r ~ t ~ n ~ end ⇒
                val result = new FieldDefinition(t.toString, σ(n))
                // TODO maybe we have to access the block list here
                userType.fields.put(userType.fields.size, result)
                result
            }) ^^ { newFields ⇒
              fields ++ newFields
            }
          }
      } else {
        // we append only fields to the type; it is not important whether or not it existed before;
        //  all fields contain all decalrations

        repN(fieldCount.toInt,
          restrictions ~ fieldTypeDeclaration ~ v64 ~ v64 ^^ {
            case r ~ t ~ n ~ end ⇒
              val name = σ(n)
              new FieldDefinition(t.toString, name)
          })

      }
    } ^^ { r ⇒
      fieldDeclarations = r;
      this
    }

  }

  /**
   * the state to be created is shared across a file parser instance; file parser instances are used to turn a file into
   * a new state anyway.
   */
  private val σ = new State;
  private val fromReader = new ByteReader(Files.newByteChannel(path).asInstanceOf[FileChannel])

  // helper structures required to build user types
  private var userTypeIndexMap = new HashMap[Long, TypeDefinition]
  private var userTypeNameMap = σ.tau
  private var blockCounter = 0;

  /**
   * The map contains start indices of base types, which are required at the end of the type header processing phase to
   *  create absolute base pool indices from relative ones.
   */
  private var localBlockBaseTypeStartIndices = new HashMap[String, Long]

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
  private[this] def typeBlock: Parser[Unit] = (v64 >> {
    count ⇒
      repN(count.toInt, typeDeclaration)
  } ^^ {
    rawList: List[TypeDeclaration] ⇒
      val raw = rawList.toArray

      //      // create new user types for types that did not exist until now
      //      raw.filter(null == _.userType).foreach(_.mkUserType)
      //
      //      // set super and base types of new user types
      //      // note: this requires another pass, because super types may be defined after a type
      //      def mkBase(t: UserType): Unit = if (null == t.baseType) {
      //        val s = userTypeNameMap(t.superName.get)
      //        mkBase(s)
      //        t.superType = s
      //        t.baseType = s.baseType
      //        s.subTypes += t
      //      }
      //      // note rather inefficient @see issue #9
      //      raw.foreach({ d ⇒ mkBase(d.userType) })
      //
      //      raw.foreach({ t ⇒
      //        val u = t.userType
      //        // check for duplicate definition of type in this block
      //        if (u.blockInfos.contains(blockCounter))
      //          throw ParseException(σ.fromReader, blockCounter, s"Duplicate redefinition of type ${u.name}")
      //
      //        // add local block info
      //        u.addBlockInfo(new BlockInfo(t.count, localBlockBaseTypeStartIndices(u.baseType.name) + t.lbpsi), blockCounter)
      //
      //        // eliminate preliminary user types in field declarations
      //        u.fields.values.foreach(f ⇒ {
      //          if (f.t.isInstanceOf[PreliminaryUserType]) {
      //            val index = f.t.asInstanceOf[PreliminaryUserType].index
      //            if (userTypeIndexMap.contains(index))
      //              f.t = userTypeIndexMap(index)
      //            else
      //              throw ParseException(σ.fromReader, blockCounter,
      //                s"${t.name}.${f.name} refers to inexistent user type $index (user types: ${
      //                  userTypeIndexMap.mkString(", ")
      //                })"
      //              )
      //          }
      //        })
      //      })

      raw
  }) >> typeChunks

  /**
   * reads type chunks using the raw information from the type block
   */
  private def typeChunks(declarations: Array[TypeDeclaration]) = new Parser[Unit] {
    def apply(in: Input) = try {
      //
      //      var lastOffset = 0L;
      //
      //      declarations.foreach({ d ⇒
      //        d.fieldDeclarations.foreach({ f ⇒
      //          // the stream is at $lastOffset; we want to read until the fields offset
      //          // TODO even if the field had not yet existed but instances had?
      //          f.dataChunks ++= List(ChunkInfo(in.position + lastOffset, f.end - lastOffset, d.count))
      //          lastOffset = f.end
      //
      //          //invalidate end for security reasons
      //          f.end = -1
      //        })
      //      })
      //
      //      // jump over the data chunk, it might be processed in the future
      //      in.drop(lastOffset.toInt)

      Success((), in)
    } catch { case e: SkillException ⇒ throw new SkillException("type chunk parsing failed", e) }
  }

  /**
   * see skill ref man §6.2
   */
  private[this] def typeDeclaration: Parser[TypeDeclaration] = (v64 ^^ { i ⇒ σ(i) }) >> { name ⇒
    // check if we append to an existing type
    if (userTypeNameMap.contains(name)) {
      val t = userTypeNameMap(name)
      var s: Option[String] = None

      t.superName match {
        case none ⇒ v64 ~ v64 ^^ {
          case count ~ fields ⇒
            new TypeDeclaration(name, s, 0, count, fields, t)
        }
      }
      superInfo(t.superName) ~ v64 ~ v64 ^^ {
        case lbpsi ~ count ~ fields ⇒
          new TypeDeclaration(name, s, lbpsi, count, fields, t)
      }

    } else {
      (v64 >> superInfo) ~ v64 ~ restrictions ~ v64 ^^ {
        case (sup, lbpsi) ~ count ~ restrictions ~ fields ⇒
          new TypeDeclaration(name, sup, lbpsi, count, fields, null)
      }
    }
  } >> { _.parseFieldDeclarations }

  /**
   *  @return a tuple with (super name, super index)
   */
  private[this] def superInfo(index: Long) = {
    if (index != 0)
      v64 ^^ { lbpsi ⇒ (Some(σ(index)), lbpsi) }
    else
      success((None, 1L))
  }

  /**
   * return a parser parsing local base pool start index, if necessary
   */
  private[this] def superInfo(superName: Option[String]) = superName match {
    case Some(_) ⇒ v64
    case None    ⇒ success(1L)
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
