import MusicParser._
import Musica._
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Success, Try}


class MusicParserTest extends FreeSpec with Matchers {

  def assertParserSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe Success(expectedResult)
  }

  def assertEmptyStringException[T](actualResult: Try[T]): Unit = {
    assertThrows[EmptyStringException](actualResult.get)
  }

  def assertNotASilenceException[T](actualResult: Try[T]): Unit = {
    assertThrows[NotASilenceException](actualResult.get)
  }

  def assertNotANoteException[T](actualResult: Try[T]): Unit = {
    assertThrows[NotANoteException](actualResult.get)
  }

  def assertNotAnInteger[T](actualResult: Try[T]): Unit = {
    assertThrows[NotAnIntegerException](actualResult.get)
  }

  def assertNotAFigureException[T](actualResult: Try[T]): Unit = {
    assertThrows[NotAFigureException](actualResult.get)
  }

  def assertCharacterNotFoundException[T](actualResult: Try[T]): Unit = {
    assertThrows[CharacterNotFoundException](actualResult.get)
  }

  "Music Parsers" - {

    "silencio" - {
      "deberia devolver un silencio de Blanca y nada en lo no consumido" in {
        assertParserSucceededWithResult(silencioParser("_"), (Silencio(Blanca), ""))
      }
      "deberia devolver un silencio de Blanca" in {
        assertParserSucceededWithResult(silencioParser("_ el resto no parseado"), (Silencio(Blanca), " el resto no parseado"))
      }
      "deberia devolver un silencio de Negra" in {
        assertParserSucceededWithResult(silencioParser("- el resto no parseado"), (Silencio(Negra), " el resto no parseado"))
      }
      "deberia devolver un silencio de corchea" in {
        assertParserSucceededWithResult(silencioParser("~ el resto no parseado"), (Silencio(Corchea), " el resto no parseado"))
      }
      "deberia Fallar porque no puede parsear ningun silencio" in {
        assertNotASilenceException(silencioParser("test"))
      }
      "deberia fallar con un string vacio" in {
        assertNotASilenceException(silencioParser(""))
      }
    }

    "nota" - {
      "deberia devolver una nota A y nada en lo no consumido" in {
        assertParserSucceededWithResult(notaParser("A"), (A, ""))
      }
      "deberia devolver una nota A" in {
        assertParserSucceededWithResult(notaParser("A el resto no parseado"), (A, " el resto no parseado"))
      }
      "deberia devolver una nota B" in {
        assertParserSucceededWithResult(notaParser("B el resto no parseado"), (B, " el resto no parseado"))
      }
      "deberia devolver una nota C" in {
        assertParserSucceededWithResult(notaParser("C el resto no parseado"), (C, " el resto no parseado"))
      }
      "deberia devolver una nota C sostenido" in {
        assertParserSucceededWithResult(notaParser("C# el resto no parseado"), (Cs, " el resto no parseado"))
      }
      "deberia devolver una nota G bemol" in {
        assertParserSucceededWithResult(notaParser("Gb el resto no parseado"), (Fs, " el resto no parseado"))
      }
      "deberia sólo parsear el nombre de la nota cuando el mismo esta seguido por un modificador que no es ni # ni b" in {
        assertParserSucceededWithResult(notaParser("Ap"), (A, "p"))
      }
      "deberia fallar porque no puede parsear ninguna Nota" in {
        assertNotANoteException(notaParser("test"))
      }
      "deberia fallar con un string vacio" in {
        assertNotANoteException(notaParser(""))
      }
    }

    "tono" - {
      "deberia devolver un tono con octava 4 y nota A, y nada en lo no consumido" in {
        assertParserSucceededWithResult(tonoParser("4A"), (Tono(4, A), ""))
      }
      "deberia devolver un tono con octava 9 y nota F" in {
        assertParserSucceededWithResult(tonoParser("9F el resto no parseado"), (Tono(9, F), " el resto no parseado"))
      }
      "deberia devolver un tono con octava 148 y nota E" in {
        assertParserSucceededWithResult(tonoParser("148E el resto no parseado"), (Tono(148, E), " el resto no parseado"))
      }
      "deberia parsear un tono con octava 2 y nota C#" in {
        assertParserSucceededWithResult(tonoParser("2C#"), (Tono(2, Cs), ""))
      }
      "deberia Fallar porque no puede parsear ningun Tono" in {
        assertNotAnInteger(tonoParser("test"))
      }
    }

    "figura" - {
      "deberia devolver una figura Redonda y nada en lo no consumido" in {
        assertParserSucceededWithResult(figuraParser("1/1"), (Redonda, ""))
      }
      "deberia devolver una figura Redonda" in {
        assertParserSucceededWithResult(figuraParser("1/1 el resto no parseado"), (Redonda, " el resto no parseado"))
      }
      "deberia devolver una figura Blanca" in {
        assertParserSucceededWithResult(figuraParser("1/2 el resto no parseado"), (Blanca, " el resto no parseado"))
      }
      "deberia devolver una figura Negra" in {
        assertParserSucceededWithResult(figuraParser("1/4 el resto no parseado"), (Negra, " el resto no parseado"))
      }
      "deberia devolver una figura Corchea" in {
        assertParserSucceededWithResult(figuraParser("1/8 el resto no parseado"), (Corchea, " el resto no parseado"))
      }
      "deberia devolver una figura SemiCorchea" in {
        assertParserSucceededWithResult(figuraParser("1/16 el resto no parseado"), (SemiCorchea, " el resto no parseado"))
      }
      "deberia Fallar porque no puede parsear ninguna figura" in {
        assertNotAFigureException(figuraParser("test"))
      }
    }

    "sonido" - {
      "deberia devolver un sonido con tono(octava 4 y nota A) y figura redonda, y nada en lo no consumido" in {
        assertParserSucceededWithResult(sonidoParser("4A1/1"), (Sonido(Tono(4, A), Redonda), ""))
      }
      "deberia devolver un sonido con tono(octava 4 y nota A) y figura redonda" in {
        assertParserSucceededWithResult(sonidoParser("4A1/1 el resto no parseado"), (Sonido(Tono(4, A), Redonda), " el resto no parseado"))
      }
      "deberia devolver un sonido con tono(octava 99 y nota D) y figura semicorchea" in {
        assertParserSucceededWithResult(sonidoParser("99D1/16 el resto no parseado"), (Sonido(Tono(99, D), SemiCorchea), " el resto no parseado"))
      }
      "debería ser un sonido para el cual la figura es Negra y el tono está compuesto por la octava 6 y la nota A sostenido" in {
        assertParserSucceededWithResult(sonidoParser("6A#1/4"), (Sonido(Tono(6, As), Negra), ""))
      }
      "deberia fallar porque no puede parsear la octava del tono" in {
        assertNotAnInteger(sonidoParser("test"))
      }
      "deberia fallar cuando el sonido tiene una nota desconocida" in {
        assertNotANoteException(sonidoParser("4P1/1"))
      }
      "deberia fallar cuando el sonido tiene una nota erronea" in {
        assertNotAFigureException(sonidoParser("4Ap1/1"))
      }
    }

    "acorde" - {
      "acorde Explicito" - {
        "debería ser un acorde con los tonos 6A, 6C#, 6G y con la duración de una Corchea" in {
          assertParserSucceededWithResult(acordeExplicitoParser("6A+6C#+6G1/8"), (Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, G)), Corchea), ""))
        }
        "deberia fallar porque no puede parsear un acorde implicito (al intentar parsear la figura)" in {
          assertNotAFigureException(acordeExplicitoParser("6AM1/2"))
        }
        "deberia fallar porque no puede parsear un tono que no empieza con la octava" in {
          assertNotAFigureException(acordeExplicitoParser("test"))
        }
      }

      "acorde MenorMayor" - {
        "debería ser el acorde 6 A mayor, que dura como una Blanca." in {
          assertParserSucceededWithResult(acordeMenorMayorParser("6AM1/2"), (Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, E)), Blanca), ""))
        }
        "deberia fallar porque no puede parsear un acorde explicito (al intentar el m o M)" in {
          assertCharacterNotFoundException(acordeMenorMayorParser("6A+6C#+6G1/8"))
        }
        "deberia fallar porque no puede parsear un tono que no empieza con la octava" in {
          assertNotAnInteger(acordeMenorMayorParser("test"))
        }
      }

      "acordeParser" - {
        "debería ser un acorde con los tonos 6A, 6C#, 6G y con la duración de una Corchea" in {
          assertParserSucceededWithResult(acordeParser("6A+6C#+6G1/8"), (Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, G)), Corchea), ""))
        }
        "debería ser el acorde 6 A mayor, que dura como una Blanca" in {
          assertParserSucceededWithResult(acordeParser("6AM1/2"), (Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, E)), Blanca), ""))
        }
        "deberia fallar porque no puede parsear un acorder explicito ni menorMayor" in {
          assertNotAnInteger(acordeParser("test"))
        }
      }
    }

    "tocable" - {
      "deberia devolver un silencio de Blanca" in {
        assertParserSucceededWithResult(tocableParser("_ el resto no parseado"), (Silencio(Blanca), " el resto no parseado"))
      }
      "debería ser un sonido para el cual la figura es Negra y el tono está compuesto por la octava 6 y la nota A sostenido" in {
        assertParserSucceededWithResult(tocableParser("6A#1/4"), (Sonido(Tono(6, As), Negra), ""))
      }
      "debería ser un acorde con los tonos 6A, 6C#, 6G y con la duración de una Corchea" in {
        assertParserSucceededWithResult(tocableParser("6A+6C#+6G1/8"), (Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, G)), Corchea), ""))
      }
      "debería ser el acorde 6 A mayor, que dura como una Blanca" in {
        assertParserSucceededWithResult(tocableParser("6AM1/2"), (Acorde(List(Tono(6, A), Tono(6, Cs), Tono(6, E)), Blanca), ""))
      }
      "deberia fallar porque no puede parsear la octava del tono de un acorde mayor de un acorde de un tocable" in {
        assertNotAnInteger(tocableParser("test"))
      }
    }

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
    }

  }

}