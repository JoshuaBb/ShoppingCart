package com.siriusxm.example.cart

import cats.Applicative
import cats.data.StateT
import cats.effect.Sync
import com.siriusxm.example.cart.ShoppingCart.ProductName

import scala.math.BigDecimal.RoundingMode

case class ShoppingCart(
                         cart: Map[ProductName, Int],
                         priceMap: Map[ProductName, BigDecimal]
                       ) {

  val rawPrice: BigDecimal = cart.foldLeft(BigDecimal(0)){ case (sum, (productName, quantity)) =>
    sum + priceMap(productName) * quantity
  }

  def taxAmount(tax: BigDecimal): BigDecimal = (tax  * rawPrice).setScale(2, RoundingMode.HALF_UP)

  def basePrice: BigDecimal = rawPrice.setScale(2, RoundingMode.HALF_UP)

  def salesPrice(tax: BigDecimal): BigDecimal = (rawPrice + taxAmount(tax)).setScale(2, RoundingMode.HALF_UP)

  def addQuantityAndPriceToCart(productName: ProductName, price: BigDecimal, quantity: Int): ShoppingCart = ShoppingCart(
    cart = cart + (productName -> (cart.getOrElse(productName, 0) + quantity)),
    priceMap = priceMap + (productName -> price),
  )
}

object ShoppingCart {
  type ProductName = String
  def Empty: ShoppingCart = ShoppingCart(Map.empty[ProductName, Int], Map.empty[ProductName, BigDecimal])
}

class ShoppingCartDb[F[_] : Applicative : Sync](priceClient: PriceClient[F]) {

  def add(title: String, quantity: Int): StateT[F, ShoppingCart, Unit] = {
    for {
      maybeItemInfo <- StateT.liftF(priceClient.getPrice(title))
      _ <- maybeItemInfo match {
        case Some(itemInfo) => StateT.modify[F, ShoppingCart](_.addQuantityAndPriceToCart(title, itemInfo.price, quantity))
        case None => getShoppingCart
      }
    } yield ()
  }

  def getShoppingCart: StateT[F ,ShoppingCart, ShoppingCart] = StateT.get

}