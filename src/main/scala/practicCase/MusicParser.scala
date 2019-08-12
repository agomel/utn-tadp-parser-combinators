import Musica._
import parsers._

package object MusicParser {
  case object silencioParser extends Parser[Silencio](input =>
    (char('_').const(Silencio(Blanca))
        <|> char('-').const(Silencio(Negra))
        <|> char('~').const(Silencio(Corchea)))(input)
      .orElse(throw new NotASilenceException(input))
  )

  case object notaParser extends Parser[Nota](input =>
    anyChar
      .map(charNota => Nota.notas.find(_.toString == charNota.toString).get)
      .<>((char('#') <|> char('b')).opt)
      .map {
        case (nota, Some('#')) => nota.sostenido
        case (nota, Some('b')) => nota.bemol
        case (nota, None) => nota
      }(input)
      .orElse(throw new NotANoteException(input))
  )

  case object tonoParser extends Parser[Tono](
    (integer <> notaParser).map { case (octava, nota) => Tono(octava, nota) }(_)
  )

  case object figuraParser extends Parser[Figura](input =>
    (string("1/16").const(SemiCorchea)
      <|> string("1/2").const(Blanca)
      <|> string("1/4").const(Negra)
      <|> string("1/8").const(Corchea)
      <|> string("1/1").const(Redonda))(input)
      .orElse(throw new NotAFigureException(input))
  )

  case object sonidoParser extends Parser[Sonido](
    (tonoParser <> figuraParser).map { case (tono, figura) => Sonido(tono, figura) }(_)
  )

  case object acordeExplicitoParser extends Parser[Acorde](
    (tonoParser.sepBy(char('+')) <> figuraParser).map { case (tonos, figura) => Acorde(tonos, figura) }(_)
  )

  case object acordeMenorMayorParser extends Parser[Acorde](
    ((tonoParser <> (char('m') <|> char('M'))) <> figuraParser)
      .map {
        case ((Tono(octava, nota), 'm'), figura) => nota.acordeMenor(octava, figura)
        case ((Tono(octava, nota), 'M'), figura) => nota.acordeMayor(octava, figura)
      }(_)
  )

  case object acordeParser extends Parser[Acorde]((acordeExplicitoParser <|> acordeMenorMayorParser)(_))

  case object tocableParser extends Parser[Tocable]((silencioParser <|> sonidoParser <|> acordeParser)(_))

  case object melodiaParser extends Parser[Melodia](tocableParser.sepBy(char(' '))(_))

}
