package pp202002.hw2
import pp202002.hw2.Data._
import scala.annotation.tailrec
import scala.util.control.TailCalls._

object Main {

  /* Problem 1: Structural Sub Type
   Complete the definition of MyClass
   DO NOT USE "Any" in anywhere in your code.
   */
  object Problem1{
    class MyClass[A,B,C,D,E,F]() {
      type Func1 = { val a: A } => { val b: B }
      type Func2 = { val b: B } => { val a: A }
      type Func3 = { val c: C } => { val a: A }
      type Func4 = { val f: F } => { val d: D }

      type Ty1 = {
        def apply: { val func: Func1 ; val c: C } => { val b: B ; val d: D }
        def function1: { val func: Func3 } => { val c: C }
        val a: A
        val b: B
        val d: D
      }

      type Ty2 = {
        def apply: { val func: Func2 ; val e: E } => { val b: B ; val f: F ; val c: C }
        def function1: { val func: Func4 } => { val c: C; val e: E }
        val a: A
        val c: C
        val d: D
      }

      /*
       Find suitable common supertype of Ty1 and Ty2,
       and replace "Any" with that type.
       */
      type CommonTy = { val a: A; val d: D }
    }
  }

  /*
    Exercise 2: Regular Expressions

    Regular expression is a sequence of character that defines a pattern.
    In this exercise, we will cover two problems:
    (1) Converts a string to regular expression defined below.
    (2) Check if a regular expression matches given string.

    For more information on regular expressions, refer to the link below.
    https://en.wikipedia.org/wiki/Regular_expression

    (*) Syntax of Exp (see Data.scala)
    E -> a      (single alphanumeric character)
       | (EE)   (concatenation)
       | (E|E)  (or)
       | (E)*   (repetition / kleene star)
   */

  /*
    Exercise 2-1: Regex Converter
    Implement a function that converts a list of characters into a regular expression (Exp).
    The result of the conversion should be a regular expression (Exp) that means the same pattern as the input.

    To solve this problem, implement the two functions described below:
    (1) Function that convert list of character to list of token (lexer)
    (2) Function that convert list of token to regular expression (parser)

    (Tip) About Lexer
    Lexer is a function that divides a string into appropriate units, and the unit is called token.
    Token is defined in Data.scala, but there is no content yet.
    It is recommended to start by defining an appropriate Token to divide the String into.
    Information about lexers and tokens can be obtained from the link below.
    https://en.wikipedia.org/wiki/Lexical_analysis

    ex)
      converter({'(', '(', '2', '3', ')', ')', '*'})
        => EStar( EConcat( EChar('2') , EChar('3') ) )
      converter({'(', '1', '(', '(', '(', '2', '3', ')', ')', '*', '4', ')', ')'})
        => EConcat( EChar('1') , EConcat( EStar( EConcat( EChar('2') , EChar('3') ) ) , EChar('4') ) ) )

    (caution)
    1. Only '0' ~ '9', 'a' ~ 'z', 'A' ~ 'Z', '|', '*', '(', and ')' will be given as an input.
    2. Converter must be able to handle gramatical errors of input.
       Note that the result of converting an erroneous character list should be Error
       itself, rather than an expression containing Error.
       ex)
       (ab)* => EError    [((ab))* is right expression]
       (((34) => EError

    (*) "List" is an algebraic datatype which is defined as below.
      sealed abstract class List[A]
      case object Nil extends List[A]
      case class Cons(hd: A, tl: List[A]) extends List[A]

    Scala provides a special syntax for List.
    - "case object" can drop out "()" after the name of case class (like "Nil()")
    - "hd :: tl" means "Cons(hd, tl)".
    For usage, see below "drop_two".
      def drop_two(l: List[Int]): List[Int] =
        l match {
          case hd1::tl1 =>
            tl1 match {
              case hd2::tl2 => tl2
              case Nil => Nil
            }
          case _ => Nil

   ref) Technically, Scala can use special symbols as a valid identifier, so it defines "case class ::" instead "case class Cons".
        Therefore, you can't match List like "case Cons(hd, tl) => ..."
   */

    def lexer(l: List[Char]): List[Token] = {
        @tailrec
        def lexerCont(xs: List[Char], cont: List[Token]=>TailRec[List[Token]]): List[Token] = {
            xs match {
                case Nil => cont(Nil).result
                case hd::tl => {
                    if (hd == '(') lexerCont(tl, (r) => tailcall(cont(Open::r)))
                    else if (hd == ')') lexerCont(tl, (r) => tailcall(cont(Close::r)))
                    else if (hd == '*') lexerCont(tl, (r) => tailcall(cont(Star::r)))
                    else if (hd == '|') lexerCont(tl, (r) => tailcall(cont(Bar::r)))
                    else if (hd >= '0' && hd <= '9') lexerCont(tl, (r) => tailcall(cont(Letter(hd)::r)))
                    else if (hd >= 'a' && hd <= 'z') lexerCont(tl, (r) => tailcall(cont(Letter(hd)::r)))
                    else if (hd >= 'A' && hd <= 'Z') lexerCont(tl, (r) => tailcall(cont(Letter(hd)::r)))
                    else lexerCont(tl, (r) => tailcall(cont(Other::r)))
                }
            }
        }
        lexerCont(l, (r)=>done(r))   
    }
    
    def parser(l: List[Token]): Exp = {
        def isNotOp(e: Exp): Boolean = {
            e match {
                case EChar('|') => false
                case EChar('(') => false
                case EError => false
                case _ => true
            }
        }

        @tailrec
        def parserIter(xs: List[Token], stack: List[Exp]): Exp = {
            xs match {
                case hd::tl => hd match {
                    // (single alphanumeric character)
                    case Letter(c) => parserIter(tl, EChar(c)::stack)
                    case Bar => parserIter(tl, EChar('|')::stack)
                    case Open => parserIter(tl, EChar('(')::stack)
                    case Close => tl match {
                        // (repetition / kleene star) & tl updated
                        case Star::tl => {
                            stack match {
                                case hd1::tl1 if isNotOp(hd1) => tl1 match {
                                    case hd2::tl2 if hd2 == EChar('(') => {
                                        parserIter(tl, EStar(hd1)::tl2)
                                    }
                                    case _ => EError
                                }
                                case _ => EError
                            }
                        }
                        case _ => {
                            stack match {
                                case hd1::tl1 if isNotOp(hd1) => tl1 match {
                                    // (or)
                                    case EChar('|')::tl2 => tl2 match {
                                        case hd3::tl3 if isNotOp(hd3) => tl3 match {
                                            case hd4::tl4 if hd4 == EChar('(') => {
                                                parserIter(tl, EOr(hd3, hd1)::tl4)
                                            }
                                            case _ => EError
                                        }
                                        case _ => EError
                                    }
                                    // (concatenation)                  
                                    case hd2::tl2 if isNotOp(hd2) => tl2 match {
                                        case hd3::tl3 if hd3 == EChar('(') => {
                                            parserIter(tl, EConcat(hd2, hd1)::tl3)
                                        }
                                        case _ => EError
                                    }
                                    case _ => EError
                                }
                                case _ => EError
                            }
                        }
                    }
                    case _ => EError
                }                                                 
                case _ => stack match {
                    case Nil => EEpsilon
                    case hd::tl if tl == Nil => hd
                    case _ => EError
                }
            }
        }
        parserIter(l, Nil)
    }

    def converter(l: List[Char]): Exp = {
        parser(lexer(l))
    }

  /*
    Exercise 2-2: Regex Matcher by Brzozowski derivative
    Implement a function that checks if a given regular expression matches a string.
    Since regular expression means a string pattern, you can determine whether a string belongs to the pattern.
    Use the "Brzozowski derivative" shown in the link below to implement a function
    that determines whether a string is included in the pattern of a regular expression.
    e.g.)
      matcher(converter("((23))*".toList), "2323232323") = true
      matcher(converter("((23))*".toList), "d") = false
      matcher(converter("(1(((23))*4))".toList), "12323234") = true

    https://en.wikipedia.org/wiki/Brzozowski_derivative
    https://www.ccs.neu.edu/home/turon/re-deriv.pdf
   */

    def matcher(e: Exp, s: String): Boolean = {
        def helper(x: Exp): Boolean = {
            x match {
                case EError => false
                case EEpsilon => true
                case EStar(_) => true
                case EChar(_) => false
                case EConcat(e1, e2) => helper(e1) & helper(e2)
                case EOr(e1, e2) => helper(e1) | helper(e2)
            }
        }

        def derivative(x: Exp, c: Char): Exp = {
            x match {
                case EEpsilon => EError
                case EChar(c1) if c1 == c => EEpsilon
                case EChar(_) => EError
                case EStar(e) => EConcat(derivative(e, c), x)
                case EConcat(e1, e2) => {
                    if (helper(e1)) EOr(EConcat(derivative(e1, c), e2), derivative(e2, c))
                    else EConcat(derivative(e1, c), e2)
                }
                case EOr(e1, e2) => EOr(derivative(e1, c), derivative(e2, c))
                case _ => EError
            }
        }

        @tailrec
        def matcherIter(x: Exp, i: Int): Boolean = {
            if (i >= s.length) helper(x)
            else matcherIter(derivative(x, s(i)), i+1)
        }

        e match {
            case EError => false
            case _ => matcherIter(e, 0)
        }
    }
}
