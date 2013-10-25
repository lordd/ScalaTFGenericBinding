package de.ust.skill.scala.generic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * @author Timm Felden
 */
@RunWith(classOf[JUnitRunner])
class BindingTest extends FunSuite {

  def check(src: String) {
    val σ = State.read(src)
//    println(σ.fieldData("string")(0).values.map{case s:String ⇒ s.getBytes().mkString("[","|","]")}.mkString)
    //println(σ.toString)
  }

  test("node") { check("node.sf") }
  test("four colored nodes") { check("fourColoredNodes.sf") }
  test("colored nodes") { check("coloredNodes.sf") }
}
