package com.siriusxm.example.cart

import cats.data.StateT
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, IOApp}
import org.http4s.ember.client.EmberClientBuilder

object Main extends IOApp.Simple {
  def printResult(shoppingCart: ShoppingCart, test: String): IO[Unit] = {
    val tax = BigDecimal(".125")
    for {
      _ <- IO.println("")
      _ <- IO.println(s"###### Running ${test} ######")
      _ <- IO.println(shoppingCart)
      _ <- IO.println(s"The base price is ${shoppingCart.basePrice}")
      _ <- IO.println(s"The sales price is ${shoppingCart.salesPrice(tax)}")
      _ <- IO.println(s"The tax amount is ${shoppingCart.taxAmount(tax)}")
    } yield {}
  }

  def run: IO[Unit] = {
    implicit val runtime: IORuntime = IORuntime.global
    val clientResource = EmberClientBuilder.default[IO].build
    clientResource.use[Unit] { client =>
      val priceClient = PriceClient.impl[IO](client)
      val db = new ShoppingCartDb[IO](priceClient)

      // Test One (Basic Check)
      val addCornFlakesTestOne = db.add("cornflakes", 2)
      val addWeetabixTestOne = db.add("weetabix", 1)
      (for {
          _ <- addCornFlakesTestOne
          _ <- addWeetabixTestOne
          shoppingCart <- db.getShoppingCart
          _ <- StateT.liftF(printResult(shoppingCart, "Test One"))
        } yield {}).runA(ShoppingCart.Empty).unsafeRunSync()

      // Test Two (Referential Transparency check)
      (for {
        _ <- addCornFlakesTestOne
        _ <- addCornFlakesTestOne
        _ <- addWeetabixTestOne
        shoppingCart <- db.getShoppingCart
        _ <- StateT.liftF(printResult(shoppingCart, "Test Two"))
      } yield {}).runA(ShoppingCart.Empty).unsafeRunSync()

      // Test Three (Making sure it handles None values correctly. In this case it is just a NoOp)
      (for {
        _ <- db.add("HoneyNutCheerios", 100000)
        shoppingCart <- db.getShoppingCart
        _ <- StateT.liftF(printResult(shoppingCart, "Test Three"))
      } yield {}).runA(ShoppingCart.Empty).unsafeRunSync()

      // Test Four (Checking that I can retrieve all the urls correctly)
      val allCheck = Seq("cheerios", "cornflakes", "frosties", "shreddies", "weetabix")
        .map(name => db.add(name, 1))
        .foldLeft(StateT.pure[IO, ShoppingCart, Unit](())) { case (result, next) =>
          for {
            _ <- result
            _ <- next
          } yield {}
        }

      (for {
        _ <-allCheck
        shoppingCart <- db.getShoppingCart
        _ <- StateT.liftF(printResult(shoppingCart, "Test Four"))
      } yield {}).runA(ShoppingCart.Empty).unsafeRunSync()


      IO.unit
    }
  }
}
