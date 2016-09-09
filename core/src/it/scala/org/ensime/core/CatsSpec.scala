// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.api, api.{ BasicTypeInfo => _, _ }
import org.ensime.fixture._
import org.ensime.model.BasicTypeInfo
import org.ensime.util.EnsimeSpec
import DeclaredAs.{ Nil => _, _ }

class CatsSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.FqnsTestProject

  it should "calculate the TypeInfo at point" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "import scala.concurrent.ExecutionContext.Implicits.global",
      "import scala.concurrent.Future",
      "import ca@cats@ts.data.EitherT",
      "import cats.instances.future._",
      "import cats.instances.list._",
      "import cats.syntax.cartesian._",
      "import cats.syntax.traverse._",
      "class Thing {",
      "    val val@value@ue: EitherT[Future, Int, String] = EitherT.right(Future(\"Ok\"))",
      "    val res@result1@ult1 = List(value, value).sequenceU",
      "    val res@result2@ult2 = (value |@| value |@| value) map {",
      "        case (a, b, c) => (a, b, c)",
      "    }",
      "}"
    ) { (p, label, cc) =>
        withClue(label) {
          cc.askTypeInfoAt(p).getOrElse { fail } shouldBe {
            label match {
              case "cats" =>
                BasicTypeInfo("cats", Object, "cats")
              case "value" =>
                api.BasicTypeInfo(
                  "EitherT[Future, Int, String]",
                  Class,
                  "cats.data.EitherT[scala.concurrent.Future, scala.Int, java.lang.String]",
                  List(
                    BasicTypeInfo("Future", Trait, "scala.concurrent.Future"),
                    BasicTypeInfo("Int", Class, "scala.Int"),
                    BasicTypeInfo("String", Class, "java.lang.String")
                  ),
                  List.empty, None, List.empty
                )
              case "result1" =>
                api.BasicTypeInfo(
                  "EitherT[Future, Int, List[String]]",
                  Class,
                  "cats.data.EitherT[scala.concurrent.Future, scala.Int, scala.collection.immutable.List[java.lang.String]]",
                  List(
                    BasicTypeInfo("Future", Trait, "scala.concurrent.Future"),
                    BasicTypeInfo("Int", Class, "scala.Int"),
                    api.BasicTypeInfo(
                      "List[String]",
                      Class,
                      "scala.collection.immutable.List[java.lang.String]",
                      List(
                        BasicTypeInfo("String", Class, "java.lang.String")
                      ),
                      List.empty, None, List.empty
                    )
                  ),
                  List.empty, None, List.empty
                )
              case "result2" =>
                api.BasicTypeInfo(
                  "EitherT[Future, Int, (String, String, String)]",
                  Class,
                  "cats.data.EitherT[scala.concurrent.Future, scala.Int, (java.lang.String, java.lang.String, java.lang.String)]",
                  List(
                    BasicTypeInfo("Future", Trait, "scala.concurrent.Future"),
                    BasicTypeInfo("Int", Class, "scala.Int"),
                    api.BasicTypeInfo(
                      "(String, String, String)",
                      Class,
                      "(java.lang.String, java.lang.String, java.lang.String)",
                      List(
                        BasicTypeInfo("String", Class, "java.lang.String"),
                        BasicTypeInfo("String", Class, "java.lang.String"),
                        BasicTypeInfo("String", Class, "java.lang.String")
                      ),
                      List.empty, None, List.empty
                    )
                  ),
                  List.empty, None, List.empty
                )
            }
          }
        }
      }
  }
}
