import org.scalatest.{FreeSpec, Matchers}
import parsers._

import scala.util.{Failure, Success}

class ParserTest extends FreeSpec with Matchers {

  def assertParserSucceededWithResult[T](actualResult: T, expectedResult: T): Unit = {
    actualResult shouldBe Success(expectedResult)
  }

  // TODO ver de pasar la excepcion por parametro
  def assertEmptyString[T](actualResult: ⇒ T): Unit = {
    assertThrows[EmptyStringException](actualResult)
  }

  def assertNotFoundCharacter[T](actualResult: ⇒ T): Unit = {
    assertThrows[CharacterNotFoundException](actualResult)
  }

  def assertNotALetter[T](actualResult: ⇒ T): Unit = {
    assertThrows[NotALetterException](actualResult)
  }

  def assertNotADigit[T](actualResult: ⇒ T): Unit = {
    assertThrows[NotADigitException](actualResult)
  }

  def assertNotAnAlphaNum[T](actualResult: ⇒ T): Unit = {
    assertThrows[NotAnAlphaNumException](actualResult)
  }
  def assertNotTheRightStringException[T](actualResult: ⇒ T): Unit = {
    assertThrows[NotTheRightStringException](actualResult)
  }

  def assertNotSatisfiesException[T](actualResult: ⇒ T)= {
    assertThrows[NotSatisfiesException[T]](actualResult)
  }
  def assertNotAnInteger[T](actualResult: ⇒ T)= {
    assertThrows[NotAnIntegerException](actualResult)
  }

  "Parsers" - {
    "Basic parsers" - {
      "anyChar" - {
        "deberia devolver un success de ParserResult(h, ola) cuando el string es hola" in {
          assertParserSucceededWithResult(anyChar("hola"), ('h', "ola"))
        }

        "deberia fallar cuando el string es vacio" in {
          assertEmptyString(anyChar("").get)
        }

        "deberia devolver un success cuando el input tiene un unico caracter" in {
          assertParserSucceededWithResult(anyChar("h"), ('h', ""))
        }
      }

      "char" - {
        "deberia devolver un success de ParserResult(c, hau) cuando el string es chau y el caracter es c" in {
          assertParserSucceededWithResult(char('c')("chau"), ('c', "hau"))
        }

        "deberia fallar cuando el string es hola y el caracter es c" in {
          assertNotFoundCharacter(char('c')("hola").get)
        }

        "deberia fallar cuando el string es vacio" in {
          assertEmptyString(char('c')("").get)
        }
      }

      "void" - {
        "deberia devolver success con ParserResult(null, ola) cuando el string es hola" in {
          assertParserSucceededWithResult(void("hola"), ((), "ola"))
        }

        "deberia fallar cuando el string es vacio" in {
          assertEmptyString(void("").get)
        }
      }

      "letter" - {
        "deberia devolver success con ParserResult(t, otal) cuando el string es total" in {
          assertParserSucceededWithResult(letter("total"), ('t', "otal"))
        }

        "deberia devolver success con ParserResult(T, ) cuando el string es T" in {
          assertParserSucceededWithResult(letter("T"), ('T', ""))
        }

        "deberia fallar cuando el string 123abc" in {
          assertNotALetter(letter("123abc").get)
        }

        "deberia fallar cuando el string es vacio" in {
          assertEmptyString(letter("").get)
        }
      }

      "digit" - {
        "deberia devolver success con ParserResult(1, 23abc) cuando el string es 123abc" in {
          assertParserSucceededWithResult(digit("123abc"), ('1', "23abc"))
        }

        "deberia fallar cuando el string abc123" in {
          assertNotADigit(digit("abc123").get)
        }

        "deberia fallar cuando el string es vacio" in {
          assertEmptyString(digit("").get)
        }
      }

      "alphaNum" - {
        "deberia devolver success con ParserResult(t, otal) cuando el string es total" in {
          assertParserSucceededWithResult(alphaNum("total"), ('t', "otal"))
        }

        "deberia devolver success con ParserResult(1, 23abc) cuando el string es 123abc" in {
          assertParserSucceededWithResult(alphaNum("123abc"), ('1', "23abc"))
        }

        "deberia fallar cuando el string es (5 + 4)" in {
          assertNotAnAlphaNum(alphaNum("(5 + 4)").get)
        }

        "deberia fallar cuando el string es vacio" in {
          assertEmptyString(alphaNum("").get)
        }
      }

      "string" - {
        "deberia devolver success con ParserResult(hola, mundo!) cuando el input es hola mundo!" in {
          assertParserSucceededWithResult(string("hola")("hola mundo!"), ("hola", " mundo!"))
        }

        "deberia devolver success con ParserResult(hola, ) cuando el input es hola" in {
          assertParserSucceededWithResult(string("hola")("hola"), ("hola", ""))
        }

        "deberia fallar cuando el input es hol" in {
          assertNotTheRightStringException(string("hola")("hol").get)
        }

        "deberia fallar cuando el input es holgado" in {
          assertNotTheRightStringException(string("hola")("holgado").get)
        }

        "deberia fallar cuando el input es vacio" in {
          assertEmptyString(string("probando")("").get)
        }

        "deberia fallar cuando el input es vacio y el string es tambien vacio" in {
          assertEmptyString(string("")("").get)
        }
      }
    }

    "Combinators" - {

      "<|>" - {
        "con dos char parser's deberia devolver lo que el primero, si este puede parsear" in {
          assertParserSucceededWithResult((char('t') <|> char('c'))("test"), ('t', "est"))
        }

        "con dos char parser's deberia devolver lo que el segundo, si el primero no puede parsear" in {
          assertParserSucceededWithResult((char('c') <|> char('h'))("helado"), ('h', "elado"))
        }
        
        "se puede aplicar con dos parser's de distinto tipo" in {
          assertParserSucceededWithResult((anyChar <|> void)("input"), ('i', "nput"))
        }
        
        "con dos char parser's deberia fallar si ninguno encuentra el char" in {
          assertNotFoundCharacter((char('c') <|> char('h'))("messi").get)
        }
        
        "Concatenación de <|>" - {
          "cuando se concatenan dos <|> con anyChar con input hola el resultado es (h, ola)" in {
            assertParserSucceededWithResult((anyChar <|> anyChar <|> anyChar) ("hola"), ('h', "ola"))
          }

          "al concatenar dos <|> con anycChar(c), digit y char(h) con input hola devuelve (h, ola)" in {
            assertParserSucceededWithResult((char('c') <|> digit <|> char('h')) ("hola"), ('h', "ola"))
          }

          "al concatenar dos <|> con un exito en el primero de los parsers, ese es el resultado (precedencia de izquiera a derecha)" in {
            assertParserSucceededWithResult((char('h') <|> digit <|> char('c')) ("hola"), ('h', "ola"))
          }

          "al concatenar dos <|> con tres parsers que no parsean el input hola falla" in {
            assertNotFoundCharacter((char('c') <|> digit <|> char('s')) ("hola").get)
          }
          
          "al concatenar dos <|> con tres parsers de distintos tipos, " in {
            assertParserSucceededWithResult((anyChar <|> void <|> string("hola"))("hola"), ('h', "ola"))
          }
          
          "al concatenar dos <|> con un exito en el segundo de los parsers, ese es el resultado (precedencia de izquiera a derecha)" in {
            assertParserSucceededWithResult((char('h') <|> digit <|> char('c')) ("1hola"), ('1', "hola"))
          }
        }
      }

      "<>" - {
        "deberia fallar cuando el string es vacio" in {
          assertEmptyString((char('t') <> char('e'))("").get)
        }

        "con dos char parser's deberia devolver una tupla con los valores del primero y el segundo y lo no parseado, si ambos pueden parsear" in {
          assertParserSucceededWithResult((char('t') <> char('e'))("test"), (('t', 'e'), "st"))
        }

        "con dos parser's deberia devolver una tupla con los elementos parseados y el resto no parseado, si ambos pueden parsear" in {
          assertParserSucceededWithResult((char('t') <> digit)("t3st"), (('t', '3'), "st"))
        }

        "si falla el primer parser deberia devolver el error del primero" in {
          assertNotFoundCharacter((char('z') <> digit)("test").get)
        }

        "si falla el segundo parser deberia devolver el error del primero" in {
          assertNotADigit((char('t') <> digit)("test").get)
        }

        "si consumo todos los caracteres falla con un anyChar segundo" in {
          assertEmptyString((string("test") <> anyChar)("test").get)
        }

        "puede parsear con 3 parsers" in {
          assertParserSucceededWithResult((char('t') <> char('e') <> char('s'))("test"), ((('t', 'e'), 's'), "t"))
        }
      }

      "~>" - {
        "deberia fallar cuando el string es vacio" in {
          assertEmptyString((char('t') ~> char('e'))("").get)
        }

        "con dos parser's deberia devolver el resultado esperado para el segundo parser, si ambos pueden parsear" in {
          assertParserSucceededWithResult((char('t') ~> digit)("t3st"), ('3', "st"))
        }

        "si falla el primer parser deberia devolver el error del primero" in {
          assertNotFoundCharacter((char('z') ~> digit)("test").get)
        }

        "si falla el segundo parser deberia devolver el error del segundo" in {
          assertNotADigit((char('t') ~> digit)("test").get)
        }
      }

      "<~" - {
        "deberia fallar cuando el string es vacio" in {
          assertEmptyString((char('t') <~ char('e'))("").get)
        }

        "con dos parser's deberia devolver el elemento parseado por el primer parser y lo no consumido por el segundo, si ambos pueden parsear" in {
          assertParserSucceededWithResult((char('t') <~ digit)("t3st"), ('t', "st"))
        }

        "si falla el primer parser deberia devolver el error del primero" in {
          assertNotFoundCharacter((char('z') <~ digit)("test").get)
        }

        "si falla el segundo parser deberia devolver el error del segundo" in {
          assertNotADigit((char('t') <~ digit)("test").get)
        }
      }
    }

    "Parsers2" - {
      "satisfies" - {
        "deberia fallar cuando no se cumple la condicion, si es que lo puede parsear" in {
          assertNotSatisfiesException( char('t').satisfies(parsedElement => parsedElement.equals('a'))("test").get )
        }
        
        "parsea correctamente si el resultado del char parser cumple la condicion" in {
          val satisfiesChar = char('p').satisfies(p => p.equals('p'))
          assertParserSucceededWithResult(satisfiesChar("poroto"), ('p', "oroto"))
        }

        "parsea correctamente si el resultado del strin cumple la condicion" in {
          val parser = string("pelota").satisfies(string => string == "pelota")
          assertParserSucceededWithResult(parser("pelotadefutbol"), ("pelota", "defutbol"))
        }
        
        "deberia funcionar si se cumple la condicion y se puede parsear" in {
          assertParserSucceededWithResult( char('a').satisfies(parsedElement => parsedElement.equals('a'))("asd"), ('a',"sd"))
        }
      }
      
      "opt" - {
        "precedencia parsea exitosamente las palabras infija y fija" in {
          val talVezIn = string("in").opt
          val precedencia = talVezIn <> string("fija")
          assertParserSucceededWithResult(precedencia("fija"), ((None, "fija"), ""))
          assertParserSucceededWithResult(precedencia("infija"), ((Some("in"), "fija"), ""))
        }
        
        "no puede parsear un string vacio" in {
          // Este estaba en fix-kleen
          assertParserSucceededWithResult(anyChar.opt(""), (None, ""))
        }
        
        "si un parser opt falla, no consume caracteres" in {
          // Este estaba en parser-combinators
          assertParserSucceededWithResult(char('a').opt("test"),(None,"test"))
        }
      }
      
      "*" - {
        "El resultado debería ser una lista vacia ya que no pudo parsear ni una sola vez" in {
          assertParserSucceededWithResult(char('a')*("hola"), (List(), "hola"))
        }
        "El resultado debería ser una lista que contiene todos los valores que hayan sido parseados" in {
          assertParserSucceededWithResult(char('a')*("aabb"), (List('a', 'a'), "bb"))
        }
        "El resultado debería ser una lista que contiene todos los valores que hayan sido parseados y nada en el sobrante" in {
          assertParserSucceededWithResult(char('a')*("aa"), (List('a', 'a'), ""))
        }

        "al pasar un string vacio, deberia parsear una lista vacia ya que no puede parsear" in {
          assertParserSucceededWithResult(char('a')*(""), (List(), ""))
        }
        "Al parsear el string 1234 devuelve List('1','2','3','4')" in {
          assertParserSucceededWithResult(digit*("1234"), (List('1','2','3','4'), ""))
        }
      }
      
      "+" - {
        "El resultado debería ser Failure(CharacterNotFound) ya que no pudo parcear ni una sola vez" in {
          assertNotFoundCharacter( (char('a')+)("hola").get )
        }
        "El resultado debería ser una lista que contiene todos los valores que hayan sido parseados" in {
          assertParserSucceededWithResult((char('a')+)("aabb"), (List('a', 'a'), "bb"))
        }
        "El resultado debería ser una lista que contiene todos los valores que hayan sido parseados y nada en el sobrante" in {
          assertParserSucceededWithResult(char('a')+("aa"), (List('a', 'a'), ""))
        }
      }

      "const" - {
        "El resultado debería ser el valor constante que se le pasó a const, en vez de lo que devolvería el parser base, si es que lo puede parsear." in {
          val trueParser = string("true").const(true)
          assertParserSucceededWithResult(trueParser("truetest"), (true, "test"))
        }

        "Al parsear el string 1234 con el parser integer y la constante sea el int 1234 debería devolver (1234, )" in {
          assertParserSucceededWithResult(integer.const(1234)("1234"), (1234, ""))
        }

        "Al parsear el string 123456 con el parser integer y la constante sea el int 1234 debería devolver (1234, )" in {
          assertParserSucceededWithResult(integer.const(1234)("123456"), (1234, ""))
        }

        "Al parsear el string cccqli con el parser char(c)* y la constante sea el int 1234 debería devolver (1234, qli)" in {
          assertParserSucceededWithResult((char('c')*).const(1234)("cccqli"), (1234, "qli"))
        }

        "El resultado debería ser Failure(NotTheRightStringException), si es que no lo puedo parsear." in {
          val trueParser = string("true").const(true)
          assertNotTheRightStringException( trueParser("test").get )
        }

      }

      "map" - {
        "si el parser original devuelve un string 'capo', el map devuelve un 'capo, groso'" in {
          val mapParser = string("capo").map(output => output + ", groso")
          assertParserSucceededWithResult(mapParser("capohola"), ("capo, groso", "hola"))
        }

        "si el parser original devuelve un char, el map devuelve un string a partir del char" in {
          val mapParser = char('c').map(output => s"$output, groso")
          assertParserSucceededWithResult(mapParser("copa"), ("c, groso", "opa"))
        }

        "si el parser original devuelve una tupla, el map devuelve una tupla de aridad 3" in {
          val mapParser = (string("probando") <> char('m')).map{
            case (parsedElement, secondParsedElement) => (parsedElement, secondParsedElement, "i amor por ti")
          }

          assertParserSucceededWithResult(mapParser("probandomi amor por ti"), (("probando", 'm', "i amor por ti"), "i amor por ti"))
        }
      }

      "integer" - {
        "al parsear el string 1234 devuelve el Integer 1234" in {
          assertParserSucceededWithResult(integer("1234"), (1234, ""))
        }
        "al parsear el string 9 devuelve el Integer 9" in {
          assertParserSucceededWithResult(integer("9"), (9, ""))
        }
        "al parsear el string 12A34 parsea los digitos que puede y devuelve el resto" in {
          assertParserSucceededWithResult(integer("12A34"), (12, "A34"))
        }
        "al parsear el string 4232x parsea el numero 4232 y no consume la x" in {
          assertParserSucceededWithResult(integer("4232x"), (4232, "x"))
        }
        "al parsear el string vacío falla" in {
          assertNotAnInteger(integer("").get)
        }
        "al parsear el string x4232 falla" in {
          assertNotAnInteger(integer("x4232").get)
        }
      }

      "sepBy" - {
        "al aplicar un parser de contenido anyChar y uno separador char(-), deberia devolver los caracteres que no son el separador" in {
          assertParserSucceededWithResult(anyChar.sepBy(char('-'))("h-o-l-a"), (List('h', 'o', 'l', 'a'), ""))
        }

        "si llega un punto en que termina la secuencia con el separador, devuelve lo ultimo antes del separador faltante" in {
          assertParserSucceededWithResult(char('a').sepBy(char('-'))("a-a-a-aaa"), (List('a', 'a', 'a', 'a'), "aa"))
        }

        "si llega un punto en que falla la secuencia con el separador, devuelve lo ultimo antes del separador faltante" in {
          assertParserSucceededWithResult(char('a').sepBy(char('-'))("a-a-a-a-bbbb"), (List('a', 'a', 'a', 'a'), "-bbbb"))
        }

        "si la cadena termina con un separador, devuelve lo parseado hasta y no consume el ultimo separador" in {
          assertParserSucceededWithResult(anyChar.sepBy(char('-'))("a-a-a-a-"), (List('a', 'a', 'a', 'a'), "-"))
        }

        "al aplicar un parser de contenido string('hola') y uno separador string('chau'), deberia devolver una lista con muchos 'test'" in {
          assertParserSucceededWithResult(string("hola").sepBy(string("chau"))("holachauholachauhola"), (List("hola", "hola", "hola"), ""))
        }

        "Si encuentra el separador parsea correctamente" in {
          val numeroDeTelefono = integer.sepBy(char('-'))
          assertParserSucceededWithResult(numeroDeTelefono("1234-5678"), (List(1234, 5678), ""))
        }

        "Si no encuentra ningun separador pero puede parsear arranca a parsearo y devuelve lo que resta" in {
          val numeroDeTelefono = integer.sepBy(char('-'))
          assertParserSucceededWithResult(numeroDeTelefono("1234 5678"), (List(1234), " 5678"))
        }

        "Si no puede parsear con el parser devuelve lista vacia" in {
          val numeroDeTelefono = integer.sepBy(char('-'))
          assertParserSucceededWithResult(numeroDeTelefono("a1234-5678"), (List(), "a1234-5678"))
        }

        "si solo podria parsear el separador, devuelve un Success(List(), '-')" in {
          assertParserSucceededWithResult((char('a').sepBy(char('-')))("-"), (List(), "-"))
        }

        "con un input vacio devuelve un Success(List(), '')" in {
          assertParserSucceededWithResult((char('a').sepBy(anyChar))(""), (List(), ""))
        }
      }

    }
  }
}
