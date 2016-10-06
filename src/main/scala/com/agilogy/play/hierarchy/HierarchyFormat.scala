package com.agilogy.play.hierarchy

import com.agilogy.play.hierarchy.HierarchyWrites.SubclassWrites
import play.api.libs.json._

import scala.reflect.ClassTag

object HierarchyFormat {

  class Discriminator[RT, DT](val format: OFormat[DT], f: RT => DT) extends HierarchyWrites.Discriminator[RT, DT](format, f) {
    def reads(json: JsValue): JsResult[DT] = format.reads(json)
  }

  object Discriminator {
    def apply[RT, DT](format: OFormat[DT], f: RT => DT): Discriminator[RT, DT] = new Discriminator[RT, DT](format, f)
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

  def format[RT, DT](subtypesFormats: SubclassFormat[DT, _ <: RT]*)(implicit discriminator: Discriminator[RT, DT] = Discriminator.default[RT]()): OFormat[RT] = {

    val writes = HierarchyWrites.writes[RT, DT](subtypesFormats: _*)

    val reads = new Reads[RT] {
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
    }

    OFormat[RT](reads, writes)
  }

  implicit class SubclassDefaultFormat[RT](format: Format[RT])(implicit classTag: ClassTag[RT])
    extends SubclassFormat[String, RT](classTag.runtimeClass.getSimpleName -> format)

  implicit class SubclassFormat[DT, RT](value: (DT, Format[RT])) extends SubclassWrites[DT, RT](value) {
    require(value._2.isInstanceOf[OFormat[RT]])
    private val format = value._2.asInstanceOf[OFormat[RT]]

    def reads(json: JsValue): JsResult[RT] = format.reads(json)

  }
}
