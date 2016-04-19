package io.kanaka.play

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import play.api.mvc.{Result, Results}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * @author Valentin Kasas
  */
object StepSpecification extends Properties("Step") {

  val executionContext = play.api.libs.concurrent.Execution.defaultContext

  implicit val futureMonad: shims.Monad[Future] = new shims.Monad[Future] {
    override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)(executionContext)
    override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa.flatMap(f)(executionContext)
    override def point[A](a: A): Future[A] = Future.successful(a)
  }

  implicit def arbitraryResult: Arbitrary[Result] = Arbitrary(
    Gen.oneOf(Results.NotFound, Results.NoContent, Results.Ok, Results.InternalServerError, Results.BadGateway)
  )

  implicit def arbitraryStepA[A](implicit arbA: Arbitrary[A], arbResult: Arbitrary[Result]): Arbitrary[StepT[Future, A]] = Arbitrary(
    for {
    isLeft <- arbitrary[Boolean]
    a <- arbitrary[A](arbA)
    result <- arbitrary[Result](arbResult)
  } yield {
    if (isLeft) {
      StepT[Future, A](Future.successful(Left(result)))
    } else {
      StepT[Future, A](Future.successful(Right(a)))
    }
  })


  property("left identity") = Prop.forAll{ (int: Int , f: Int => StepT[Future, String] )  =>
    val l = StepT.point[Future, Int](int) flatMap f
    val r = f(int)

    Await.result(l.run, 1.second) == Await.result(r.run, 1.second)
  }

  property("right identity") = Prop.forAll{ ( step: StepT[Future, String] )  =>
    val l = step flatMap StepT.point[Future, String]
    val r = step

    Await.result(l.run, 1.second) == Await.result(r.run, 1.second)
  }

  property("associativity") = Prop.forAll{ (step: StepT[Future, Int], f: Int => StepT[Future, String], g: String => StepT[Future, Boolean]) =>
    val l = (step flatMap f) flatMap g
    val r = step flatMap(x => f(x) flatMap g)

    Await.result(l.run, 1.second) == Await.result(r.run, 1.second)
  }
}
