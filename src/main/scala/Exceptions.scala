import ParserTypes._

class EmptyStringException extends Exception
class CharacterNotFoundException(char: Char, input: String) extends Exception//(s"The character '$char' was not found in $input")
class NotALetterException(input: String) extends Exception
class NotADigitException(input: String) extends Exception
class NotAnAlphaNumException(input: String) extends Exception
class NotTheRightStringException(expectedString : String, notConsumed: String) extends Exception// (s"Expected $expectedString... but got $currentString")
class NotSatisfiesException[T](condition: ParserCondition[T], notConsumed: String) extends Exception
class NotAnIntegerException(input: String) extends Exception