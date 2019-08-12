import scala.util.{Failure, Success, Try}

object ParserTypes {
  //(parsedElement, notConsumed)
  type ParserOutput[+T] = (T, String)
  type ParserResult[T] = Try[ParserOutput[T]]
  type ParserType[T] = String => ParserResult[T]
  type ParserCondition[T] = T => Boolean
}

import ParserTypes._

import parsers._

class Parser[+T](criterion: String => ParserResult[T]) {
  def apply(input: String): ParserResult[T] = criterion(input)

  def <|>[U >: T](anotherParser: Parser[U]) : Parser[U] = new Parser[U](input => this(input).orElse(anotherParser(input)))
  
  def <>[U](anotherParser: Parser[U]): Parser[(T, U)] = new Parser[(T,U)](
    this(_).flatMap { case (parsedElement, notConsumed) => anotherParser(notConsumed).map(parserOutput => ((parsedElement, parserOutput._1), parserOutput._2)) }
  )

  def ~>[U](anotherParser: Parser[U]): Parser[U] = new Parser[U](  
    (this <> anotherParser)(_).map { case ((_, anotherParserParsedElement), notConsumed) => (anotherParserParsedElement, notConsumed) }
  )

  def <~[U](anotherParser: Parser[U]): Parser[T] = new Parser[T](
    (this <> anotherParser)(_).map { case ((parsedElement, _), notConsumed) => (parsedElement, notConsumed)}
  )
  
  def satisfies(condition: ParserCondition[T]) = new Parser[T](input =>
    this(input).filter { case (parsedElement, _) => condition(parsedElement) }
      .orElse(Failure(new NotSatisfiesException(condition, input)))
  )

  def opt: Parser[Option[T]] = new Parser[Option[T]](
    input => this(input).map { case (parsedElement, notConsumed) => (Some(parsedElement), notConsumed) }.orElse(Success(None, input))
  )

  def * : Parser[List[T]] = new Parser[List[T]](input =>
    this(input).transform(
      {
        case (parsedElement, notConsumed) =>
          this.*(notConsumed).map { case (parsed, stillNotConsumed) => (parsedElement :: parsed, stillNotConsumed) }
      },
      _ => Success(List(), input)
    )
  )

  def + = new Parser[List[T]](
    this(_).flatMap{ case (parsedElement, notConsumed) => this.*(notConsumed).map { case (parsed, stillNotConsumed) => (parsedElement :: parsed, stillNotConsumed) } }
  )

  def sepBy[U](separator: Parser[U]): Parser[List[T]] = new Parser(input =>
    (this <> (separator ~> this).*).map { case(firstElement, parsedElements) => firstElement :: parsedElements }(input)
      .orElse(Try((List(), input)))
  )

  def const[U](constantValue: U) = new Parser[U](this.map(_ => constantValue)(_))

  def map[U](mapper: T => U) = new Parser[U](
    this(_).map{ case (parsedElement, notConsumed) => (mapper(parsedElement), notConsumed) }
  )
}

class NonEmptyInputParser[T](criterion: String => ParserResult[T]) extends Parser[T](criterion) {
  override def apply(input: String): ParserResult[T] =
    if (input.isEmpty) Failure(new EmptyStringException) else super.apply(input)
}

object parsers {
  val anyChar = new NonEmptyInputParser[Char](input => Success(input.head, input.tail))

  val anyCharWithCondition = (condition : ParserCondition[Char], exception: String => Throwable) =>
    new NonEmptyInputParser[Char](input => anyChar.satisfies(condition)(input).orElse(Failure(exception(input))))

  var char = (char: Char) => anyCharWithCondition(_ == char, new CharacterNotFoundException(char, _))

  val void = new NonEmptyInputParser[Unit](input => Success((), input.tail))

  val letter = anyCharWithCondition(_.isLetter, new NotALetterException(_))

  val digit = anyCharWithCondition(_.isDigit, new NotADigitException(_))

  val alphaNum = new NonEmptyInputParser[Char](input =>
    (letter <|> digit)(input).orElse(Failure(new NotAnAlphaNumException(input))))

  val integer = new Parser[Int](input =>
    digit.+.map (parsedList => parsedList.mkString("").toInt)(input)
      .orElse(Failure(new NotAnIntegerException(input))))

  val string = (string : String) => new NonEmptyInputParser[String](input =>
      if (input.startsWith(string))
      Success(string, input.slice(string.length, input.length))
      else
      Failure(new NotTheRightStringException(string, input))
    )
}