package unit

import javafx.scene.shape.Circle

import com.agilogy.play.hierarchy.HierarchyFormat
import com.agilogy.play.hierarchy.HierarchyFormat.Discriminator
import org.scalatest.{ FunSpec, FlatSpec }
import play.api.libs.json._

trait GeometricShape extends Product with Serializable {
  val numberOfSides: Int
}

case class Triangle(side1Length: Double, side2Length: Double, side3Length: Double) extends GeometricShape {
  val numberOfSides = 3
}

case class Rectangle(side1Length: Double, side2Length: Double) extends GeometricShape {
  val numberOfSides = 4
}

class HierarchyFormatTest extends FunSpec {

  val triangle233 = Triangle(2, 3, 3)
  val rectangle12 = Rectangle(1, 2)

  val triangleFmt = Json.format[Triangle]
  val rectangleFmt = Json.format[Rectangle]

  describe("simple hierarchy format (default discriminator is class name)") {

    val fmt = HierarchyFormat.format(triangleFmt, rectangleFmt)

    it("should write to json") {
      assert(fmt.writes(triangle233) === Json.obj("$type" -> "Triangle", "side1Length" -> 2, "side2Length" -> 3, "side3Length" -> 3))
      assert(fmt.writes(rectangle12) === Json.obj("$type" -> "Rectangle", "side1Length" -> 1, "side2Length" -> 2))
    }

    it("should read from json") {
      assert(fmt.reads(fmt.writes(triangle233)) === JsSuccess(triangle233))
      assert(fmt.reads(fmt.writes(rectangle12)) === JsSuccess(rectangle12))
    }
  }

  describe("simple hierarchy format (default discriminator but choosing the attribute name for it)") {

    val fmt = HierarchyFormat.format(triangleFmt, rectangleFmt)(Discriminator.default("myType"))

    it("should write to json") {
      assert(fmt.writes(triangle233) === Json.obj("myType" -> "Triangle", "side1Length" -> 2, "side2Length" -> 3, "side3Length" -> 3))
      assert(fmt.writes(rectangle12) === Json.obj("myType" -> "Rectangle", "side1Length" -> 1, "side2Length" -> 2))
    }

    it("should read from json") {
      assert(fmt.reads(fmt.writes(triangle233)) === JsSuccess(triangle233))
      assert(fmt.reads(fmt.writes(rectangle12)) === JsSuccess(rectangle12))
    }
  }

  describe("customized (simple) discriminator hierarchy format") {

    val fmt = HierarchyFormat.format(3 -> triangleFmt, 4 -> rectangleFmt)(Discriminator.discriminator(_.numberOfSides, "numberOfSides"))

    it("should write to json") {
      assert(fmt.writes(triangle233) === Json.obj("numberOfSides" -> 3, "side1Length" -> 2, "side2Length" -> 3, "side3Length" -> 3))
      assert(fmt.writes(rectangle12) === Json.obj("numberOfSides" -> 4, "side1Length" -> 1, "side2Length" -> 2))
    }

    it("should read from json") {
      assert(fmt.reads(fmt.writes(triangle233)) === JsSuccess(triangle233))
      assert(fmt.reads(fmt.writes(rectangle12)) === JsSuccess(rectangle12))
    }

  }

  // Let's pretend Square didn't exist until this test here

  case class Square(sideLength: Double) extends GeometricShape {
    override val numberOfSides: Int = 4
  }

  val square2 = Square(2)

  val squareFmt = Json.format[Square]

  val form: GeometricShape => Option[String] = {
    case Triangle(_, _, _) => None
    case Rectangle(_, _) => Some("Rectangle")
    case Square(_) => Some("Square")
  }

  describe("customized, compound discriminator hierarchy format") {

    val fmt = HierarchyFormat.format((4 -> Option("Rectangle")) -> rectangleFmt, (4 -> Option("Square")) -> squareFmt, (3 -> Option.empty[String]) -> triangleFmt)(
      Discriminator.discriminator("numberOfSides", _.numberOfSides, "form", form)
    )

    it("should write to json") {
      assert(fmt.writes(triangle233) === Json.obj("numberOfSides" -> 3, "form" -> JsNull, "side1Length" -> 2, "side2Length" -> 3, "side3Length" -> 3))
      assert(fmt.writes(rectangle12) === Json.obj("numberOfSides" -> 4, "form" -> "Rectangle", "side1Length" -> 1, "side2Length" -> 2))
      assert(fmt.writes(square2) === Json.obj("numberOfSides" -> 4, "form" -> "Square", "sideLength" -> 2))
    }

    it("should read from json") {
      assert(fmt.reads(fmt.writes(triangle233)) === JsSuccess(triangle233))
      assert(fmt.reads(fmt.writes(rectangle12)) === JsSuccess(rectangle12))
      assert(fmt.reads(fmt.writes(square2)) === JsSuccess(square2))
    }

  }

  describe("totally customized discriminator hierarchy format") {
    import play.api.libs.functional.syntax._

    val discriminatorFmt: OFormat[(Int, Option[String])] = (
      (__ \ "sides").format[Int] and
      (__ \ "form").formatNullable[String]
    )({ case (s, f) => s -> f }, identity)

    val fmt = HierarchyFormat.format((4 -> Option("Rectangle")) -> rectangleFmt, (4 -> Option("Square")) -> squareFmt, (3 -> Option.empty[String]) -> triangleFmt)(
      Discriminator[GeometricShape, (Int, Option[String])](discriminatorFmt, s => s.numberOfSides -> form(s))
    )

    it("should write to json") {
      assert(fmt.writes(triangle233) === Json.obj("sides" -> 3, "side1Length" -> 2, "side2Length" -> 3, "side3Length" -> 3))
      assert(fmt.writes(rectangle12) === Json.obj("sides" -> 4, "form" -> "Rectangle", "side1Length" -> 1, "side2Length" -> 2))
      assert(fmt.writes(square2) === Json.obj("sides" -> 4, "form" -> "Square", "sideLength" -> 2))
    }

    it("should read from json") {
      assert(fmt.reads(fmt.writes(triangle233)) === JsSuccess(triangle233))
      assert(fmt.reads(fmt.writes(rectangle12)) === JsSuccess(rectangle12))
      assert(fmt.reads(fmt.writes(square2)) === JsSuccess(square2))
    }

  }

  //TODO: Error handling

}
