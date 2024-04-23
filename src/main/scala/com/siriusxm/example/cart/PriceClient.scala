package com.siriusxm.example.cart

import cats.effect.Concurrent
import io.circe.Decoder
import org.http4s.{EntityDecoder, EntityEncoder}
import io.circe._
import org.http4s.circe._
import io.circe.generic.semiauto._
import org.http4s.Method.GET
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits.http4sLiteralsSyntax


case class ItemInfo(title: String, price: BigDecimal)

object ItemInfo {
  implicit val decoder: Decoder[ItemInfo] = deriveDecoder[ItemInfo]

  implicit def entityDecoder[F[_] : Concurrent]: EntityDecoder[F, ItemInfo] = jsonOf

  implicit val encoder: Encoder.AsObject[ItemInfo] = deriveEncoder[ItemInfo]

  implicit def entityEncoder[F[_] : Concurrent]: EntityEncoder[F, ItemInfo] = jsonEncoderOf
}

trait PriceClient[F[_]] {
  def getPrice(title: String): F[Option[ItemInfo]]
}

// Not completely sure what happens on a 404 response code. I would like to map that to an option, but didn't test it
object PriceClient {
  def impl[F[_] : Concurrent](c: Client[F]): PriceClient[F] = (title: String) => {
    val dsl = new Http4sClientDsl[F] {}
    import dsl._
    val url = uri"""https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main""" / s"${title.toLowerCase}.json"
    c.expectOption[ItemInfo](GET(url))
  }
}