package com.agilogy.play.hierarchy

import play.api.libs.json._

import scala.reflect.ClassTag

object HierarchyFormat {

  case class Discriminator[RT, DT](format: OFormat[DT], f: RT => DT) {
    def reads(json: JsValue): JsResult[DT] = format.reads(json)
    def writes(obj: RT): JsObject = format.writes(f(obj))
  }

  object Discriminator {
    def default[RT](discriminatorAttribute: String = "$type"): Discriminator[RT, String] =
      new Discriminator[RT, String]((JsPath \ discriminatorAttribute).format[String], _.getClass.getSimpleName)
    def discriminator[RT, DT: Format](f: RT => DT, attributeName: String = "type"): Discriminator[RT, DT] =
      new Discriminator[RT, DT]((JsPath \ attributeName).format[DT], f)
    def discriminator[RT, DT1: Format, DT2: Format](attributeName1: String, f1: RT => DT1, attributeName2: String, f2: RT => DT2): Discriminator[RT, (DT1, DT2)] = {
      import play.api.libs.functional.syntax._
      val format: OFormat[(DT1, DT2)] = (
        (__ \ attributeName1).format[DT1] and
        (__ \ attributeName2).format[DT2]
      )((s, i) => (s, i), identity)
      new Discriminator[RT, (DT1, DT2)](format, v => f1(v) -> f2(v))
    }
  }

  def format[RT, DT](subtypesFormats: SubclassFormat[DT, _ <: RT]*)(implicit discriminator: Discriminator[RT, DT] = Discriminator.default[RT]()): OFormat[RT] =
    new OFormat[RT] {
      override def reads(json: JsValue): JsResult[RT] = {
        val result = for {
          d <- discriminator.format.reads(json)
          f <- subtypesFormats.find(_.discriminatorValue == d).map(scf => JsSuccess(scf))
            .getOrElse(JsError(s"Could not find a format for discriminator value $d"))
          parsed <- f.reads(json)
        } yield parsed
        result match {
          case JsSuccess(v, _) => JsSuccess(v)
          case e => e
        }
      }

      override def writes(o: RT): JsObject = {
        val jsonDiscriminator = discriminator.writes(o)
        val discriminatorValue = discriminator.f(o)
        val subtypeFormat = subtypesFormats.find(_.discriminatorValue == discriminatorValue)
          .getOrElse(throw new IllegalArgumentException(s"Couldn't find a subtype format for discriminator value $discriminatorValue"))
        val jsonObject = subtypeFormat.write(o)
        jsonObject ++ jsonDiscriminator
      }
    }

  implicit class SubclassDefaultFormat[RT](format: Format[RT])(implicit classTag: ClassTag[RT])
    extends SubclassFormat[String, RT](classTag.runtimeClass.getSimpleName -> format)

  implicit class SubclassFormat[DT, RT](value: (DT, Format[RT])) {
    val discriminatorValue = value._1
    require(value._2.isInstanceOf[OFormat[RT]])
    private val format = value._2.asInstanceOf[OFormat[RT]]

    def reads(json: JsValue): JsResult[RT] = format.reads(json)
    def write(o: Any): JsObject = format.writes(o.asInstanceOf[RT])

  }
}
