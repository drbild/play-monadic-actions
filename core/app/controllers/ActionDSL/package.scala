/*
 * Copyright 2014
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package controllers

import io.kanaka.play.{StepT, StepTOps}
import play.api.data.Form
import play.api.data.validation.ValidationError
import play.api.libs.json.{JsPath, JsResult}
import play.api.mvc.{Result, Results}
import shims.{Functor, Monad}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * @author Valentin Kasas
 */
package object ActionDSL {

  type JsErrorContent = Seq[(JsPath, Seq[ValidationError])]

  private [ActionDSL] def fromFuture[A](onFailure: Throwable => Result)(future: Future[A])(implicit ec: ExecutionContext): StepT[Future, A] =
    StepT[Future, A](future.map(Right[Result, A](_)).recover{case t: Throwable => Left[Result, A](onFailure(t))})

  private [ActionDSL] def fromFOption[F[_], A](onNone: => Result)(fOption: F[Option[A]])(implicit F: shims.Functor[F]): StepT[F, A] =
    StepT[F, A](F.map(fOption)(_.fold[Either[Result, A]](Left(onNone))(Right(_))))

  private [ActionDSL] def fromFEither[F[_],A,B](onLeft: B => Result)(fEither: F[Either[B,A]])(implicit F: shims.Functor[F]): StepT[F, A] =
    StepT[F, A](F.map(fEither)(_.left.map(onLeft)))

  private [ActionDSL] def fromOption[F[_], A](onNone: => Result)(option: Option[A])(implicit F: shims.Monad[F]): StepT[F, A] =
    StepT[F, A](F.point(option.fold[Either[Result,A]](Left(onNone))(Right(_))))

  private [ActionDSL] def fromEither[F[_],A,B](onLeft: B => Result)(either: Either[B,A])(implicit F: shims.Monad[F]): StepT[F, A] =
    StepT[F, A](F.point(either.left.map(onLeft)))

  private [ActionDSL] def fromJsResult[F[_], A](onJsError: JsErrorContent => Result)(jsResult: JsResult[A])(implicit F: shims.Monad[F]): StepT[F, A] =
    StepT[F, A](F.point(jsResult.fold(err => Left(onJsError(err)), Right(_))))

  private [ActionDSL] def fromForm[F[_], A](onError: Form[A] => Result)(form: Form[A])(implicit F: shims.Monad[F]): StepT[F, A] =
    StepT[F, A](F.point(form.fold(err => Left(onError(err)), Right(_))))

  private [ActionDSL] def fromBoolean[F[_]](onFalse: => Result)(boolean: Boolean)(implicit F: shims.Monad[F]): StepT[F, Unit] =
    StepT[F, Unit](F.point(if(boolean) Right(()) else Left(onFalse)))

  private [ActionDSL] def fromTry[F[_], A](onFailure: Throwable => Result)(tryValue: Try[A])(implicit F: shims.Monad[F]):StepT[F, A] =
    StepT[F, A](F.point(tryValue match {
        case Failure(t) => Left(onFailure(t))
        case Success(v) => Right(v)
      }))

  trait MonadicActions0 {

    import scala.language.implicitConversions

    val executionContext: ExecutionContext = play.api.libs.concurrent.Execution.defaultContext

    implicit val futureMonad: shims.Monad[Future] = new Monad[Future] {

      override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)(executionContext)

      override def flatMap[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa.flatMap(f)(executionContext)

      override def point[A](a: A): Future[A] = Future.successful(a)
    }

    implicit class BaseOps[F[_], A](base: F[A])(implicit F: shims.Functor[F]) {
      def -| : StepT[F, A] = StepT[F, A](F.map(base)(Right(_)))
    }

    implicit def futureToStepOps[A](future: Future[A]): StepTOps[Future, A, Throwable] = new StepTOps[Future, A, Throwable] {
      override def orFailWith(failureHandler: (Throwable) => Result) = fromFuture(failureHandler)(future)(executionContext)
    }
  }

  trait MonadicActions extends MonadicActions0 {

    implicit def fOptionToStepOps[F[+_], A](fOption: F[Option[A]])(implicit F: shims.Monad[F]): StepTOps[F, A,Unit] = new StepTOps[F, A, Unit]{
      override def orFailWith(failureHandler: Unit => Result) = fromFOption(failureHandler(()))(fOption)
    }

    implicit def fEitherToStepOps[F[_], A, B](fEither: F[Either[B,A]])(implicit F: shims.Functor[F]): StepTOps[F,A,B] = new StepTOps[F,A,B] {
      override def orFailWith(failureHandler: (B) => Result) = fromFEither(failureHandler)(fEither)
    }

    implicit def optionToStepOps[F[_], A](option: Option[A])(implicit F: shims.Monad[F]): StepTOps[F, A, Unit] = new StepTOps[F, A, Unit] {
      override def orFailWith(failureHandler: (Unit) => Result) = fromOption(failureHandler(()))(option)
    }

    implicit def eitherToStepOps[F[_], A, B](either: Either[B,A])(implicit F: shims.Monad[F]): StepTOps[F,A,B] = new StepTOps[F,A,B] {
      override def orFailWith(failureHandler: (B) => Result) = fromEither(failureHandler)(either)
    }

    implicit def jsResultToStepOps[F[_], A](jsResult: JsResult[A])(implicit F: shims.Monad[F]): StepTOps[F, A, JsErrorContent] = new StepTOps[F, A, JsErrorContent] {
      override def orFailWith(failureHandler: (JsErrorContent) => Result) = fromJsResult(failureHandler)(jsResult)
    }

    implicit def formToStepOps[F[_], A](form: Form[A])(implicit F: shims.Monad[F]): StepTOps[F, A, Form[A]] = new StepTOps[F, A, Form[A]] {
      override def orFailWith(failureHandler: (Form[A]) => Result) = fromForm(failureHandler)(form)
    }

    implicit def booleanToStepOps[F[_]](boolean: Boolean)(implicit F: shims.Monad[F]): StepTOps[F, Unit, Unit] = new StepTOps[F, Unit, Unit] {
      override def orFailWith(failureHandler: (Unit) => Result) = fromBoolean(failureHandler(()))(boolean)
    }

    implicit def tryToStepOps[F[_], A](tryValue: Try[A])(implicit F: shims.Monad[F]): StepTOps[F, A, Throwable] = new StepTOps[F, A, Throwable] {
      override def orFailWith(failureHandler: (Throwable) => Result) = fromTry(failureHandler)(tryValue)
    }

    implicit def stepToResult[F[_], R <: Result](step: StepT[F, R])(implicit F: shims.Functor[F]): F[Result] = F.map(step.run)(_.merge)

    implicit def stepToEither[F[_], A](step: StepT[F, A]): F[Either[Result, A]] = step.run
  }
}
