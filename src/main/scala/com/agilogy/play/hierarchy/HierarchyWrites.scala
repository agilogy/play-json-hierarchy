package com.agilogy.play.hierarchy

import play.api.libs.json._

import scala.reflect.ClassTag

object HierarchyWrites {

  class Discriminator[RT, DT](val writes: OWrites[DT], val f: RT => DT) {
    def writes(obj: RT): JsObject = writes.writes(f(obj))
  }

  object Discriminator {
    def apply[RT, DT](writes: OWrites[DT], f: RT => DT): Discriminator[RT, DT] = new Discriminator(writes, f)
    def default[RT](discriminatorAttribute: String = "$type"): Discriminator[RT, String] =
      new Discriminator[RT, String]((JsPath \ discriminatorAttribute).write[String], _.getClass.getSimpleName)
    def discriminator[RT, DT: Writes](f: RT => DT, attributeName: String = "type"): Discriminator[RT, DT] =
      new Discriminator[RT, DT]((JsPath \ attributeName).write[DT], f)
    def discriminator[RT, DT1: Writes, DT2: Writes](attributeName1: String, f1: RT => DT1, attributeName2: String, f2: RT => DT2): Discriminator[RT, (DT1, DT2)] = {
      import play.api.libs.functional.syntax._
      val w: OWrites[(DT1, DT2)] = (
        (__ \ attributeName1).write[DT1] and
        (__ \ attributeName2).write[DT2]
      )(a => a._1 -> a._2)
      new Discriminator[RT, (DT1, DT2)](w, v => f1(v) -> f2(v))
    }
  }

  def writes[RT, DT](subtypesWrites: SubclassWrites[DT, _ <: RT]*)(implicit discriminator: Discriminator[RT, DT] = Discriminator.default[RT]()): OWrites[RT] =
    new OWrites[RT] {
      override def writes(o: RT): JsObject = {
        val jsonDiscriminator = discriminator.writes(o)
        val discriminatorValue = discriminator.f(o)
        val subtypeFormat = subtypesWrites.find(_.discriminatorValue == discriminatorValue)
          .getOrElse(throw new IllegalArgumentException(s"Couldn't find a subtype writes for discriminator value $discriminatorValue"))
        val jsonObject = subtypeFormat.write(o)
        jsonObject ++ jsonDiscriminator
      }
    }

  implicit class SubclassDefaultWrites[RT](writes: Writes[RT])(implicit classTag: ClassTag[RT])
    extends SubclassWrites[String, RT](classTag.runtimeClass.getSimpleName -> writes)

  implicit class SubclassWrites[DT, RT](value: (DT, Writes[RT])) {
    val discriminatorValue = value._1
    require(value._2.isInstanceOf[OWrites[RT]])
    private val writes = value._2.asInstanceOf[OWrites[RT]]

    def write(o: Any): JsObject = writes.writes(o.asInstanceOf[RT])

  }
}
