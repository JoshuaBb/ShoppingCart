package com.siriusxm.example.cart

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.http4s.ember.client.EmberClientBuilder
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.should.Matchers


class ShoppingCartDbSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  trait TestFixture {
    def test: IO[Assertion]

    def impl: IO[ShoppingCartDb[IO]] = {
      val clientResource = EmberClientBuilder.default[IO].build
      clientResource.use[ShoppingCartDb[IO]] { client =>
        val priceClient = PriceClient.impl[IO](client)
        val db = new ShoppingCartDb[IO](priceClient)
        IO.pure(db)
      }
    }
  }


  "Shopping Cart IO" - {
    // Maybe not the best way to handle invalid items
    "works when adding invalid items" in {
      new TestFixture {
        override def test: IO[Assertion] = {
          impl.map { db =>
            (for {
              _ <- db.add("invalid item", 100)
              state <- db.getShoppingCart
            } yield state).runA(ShoppingCart.Empty).unsafeRunSync()
          }.asserting(_ shouldBe ShoppingCart.Empty)
        }
      }.test
    }
    "works for the default test case of adding 2 cornflakes and 1 weetabix" in {
      new TestFixture {
        override def test: IO[Assertion] = {
          val expected = ShoppingCart(Map("weetabix" -> 1, "cornflakes" -> 2), Map("weetabix" -> 9.98, "cornflakes" -> 2.52))
          impl.map { db =>
            (for {
              _ <- db.add("weetabix", 1)
              _ <- db.add("cornflakes", 2)
              state <- db.getShoppingCart
            } yield state).runA(ShoppingCart.Empty).unsafeRunSync()
          }.asserting(_ shouldBe expected)
        }
      }.test
    }
    "works for the default test case of adding 2 cornflakes and 2 weetabix" in {
      new TestFixture {
        override def test: IO[Assertion] = {
          val expected = ShoppingCart(Map("weetabix" -> 2, "cornflakes" -> 2), Map("weetabix" -> 9.98, "cornflakes" -> 2.52))
          impl.map { db =>
            val addWeetabix = db.add("weetabix", 1)
            (for {
              _ <- addWeetabix
              _ <- db.add("cornflakes", 2)
              _ <- addWeetabix
              state <- db.getShoppingCart
            } yield state).runA(ShoppingCart.Empty).unsafeRunSync()
          }.asserting(_ shouldBe expected)
        }
      }.test
    }
  }
  "Shopping Cart" - {
    "Calculates all the values correctly" in {
      val tax =  BigDecimal(".125")
      val cart = ShoppingCart(Map("weetabix" -> 1, "cornflakes" -> 2), Map("weetabix" -> 9.98, "cornflakes" -> 2.52))
      cart.basePrice shouldBe BigDecimal("15.02")
      cart.taxAmount(tax) shouldBe BigDecimal("1.88")
      cart.salesPrice(tax) shouldBe BigDecimal("16.90")

    }
  }
}


