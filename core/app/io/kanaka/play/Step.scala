package io.kanaka.play

import play.api.mvc.{Result, Results}

final case class StepT[F[_], A](run: F[Either[Result, A]]) {

  def map[B](f: A => B)(implicit F: shims.Functor[F]): StepT[F, B] =
    StepT[F, B](F.map(run)(_.right.map(f)))

  def flatMap[B](f: A => StepT[F, B])(implicit F: shims.Monad[F]): StepT[F, B] =
    StepT[F, B](F.flatMap(run)(_.fold(r => F.point(Left(r)), a => f(a).run)))

  def withFilter(p: A => Boolean)(implicit F: shims.Monad[F]): StepT[F, A] =
     flatMap(a =>
       if (p(a))
         StepT[F, A](F.point(Right(a)))
       else
         StepT[F, A](F.point(Left(Results.InternalServerError)))
     )
}

object StepT {

  def point[F[_], A](a: A)(implicit F: shims.Monad[F]): StepT[F, A] = StepT[F, A](F.point(Right(a)))

}

trait StepTOps[F[_], A, B] {
  def orFailWith(failureHandler: B => Result): StepT[F, A]
  def ?|(failureHandler: B => Result): StepT[F, A] = orFailWith(failureHandler)
  def ?|(failureThunk: => Result): StepT[F, A] = orFailWith(_ => failureThunk)
}
