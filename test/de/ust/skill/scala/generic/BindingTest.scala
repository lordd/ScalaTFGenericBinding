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
    val σ = State.read("node.sf")
    println(σ.toString)
  }

  test("node") { check("node.sf") }
}
