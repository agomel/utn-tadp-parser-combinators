package practico

import MusicParser._
import Musica._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Success, Try}

class PracticoIndividualTest extends FreeSpec with Matchers {

  def assertParserSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe Success(expectedResult)
  }

  "Practico individual" - {

    "melodia" - {

      "debería ser una lista con los sonidos Sonido(Tono(4,C),Negra), Sonido(Tono(4,C),Negra), Sonido(Tono(4,D),Blanca), Sonido(Tono(4,C),Negra)" in {
        assertParserSucceededWithResult(melodiaParser("4C1/4 4C1/4 4D1/2 4C1/4"), (List(Sonido(Tono(4,C),Negra), Sonido(Tono(4,C),Negra), Sonido(Tono(4,D),Blanca), Sonido(Tono(4,C),Negra)), ""))
      }
      "deberia parsear el tocable correcto si esta seguido por uno con un typo" in {
        assertParserSucceededWithResult(melodiaParser("4C1/4 4R1/4"), (List(Sonido(Tono(4, C), Negra)), " 4R1/4"))
      }
      "si no puede parsear ni un tocable, List()" in {
        assertParserSucceededWithResult(melodiaParser("test"), (List(), "test"))
      }

      "debería ser una lista con tres veces 4C1/4" in {
        assertParserSucceededWithResult(melodiaParser("x3(4C1/4)"), (List(Sonido(Tono(4,C),Negra), Sonido(Tono(4,C),Negra), Sonido(Tono(4,C),Negra)), ""))
      }

    }

  }

}