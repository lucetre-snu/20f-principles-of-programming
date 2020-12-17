package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ExprInterpreter {
  class InterpreterException(val reason: String) extends Exception {
    override def getMessage: String = reason
  }

  implicit def exprInterpreter[Env](implicit
      envOps: EnvOps[Env, Value[Env]]
  ): Interpreter[Expr, Value[Env]] = new Interpreter[Expr, Value[Env]] {

    val TRUE = VRight(VInt(0))
    val FALSE = VLeft(VInt(0))

    // get an integer
    def getInt(v: Value[Env]) = v match {
      case VInt(n) => n
      case _ => throw new InterpreterException("Not an Integer Type")
    }

    def interp(expr: Expr): Try[Value[Env]] = {

      def interpIter(expr: Expr, env: Env): Value[Env] = {

        // performs integer operation
        def intOp(left: Expr, right: Expr, op: (Int, Int) => Value[Env]): Value[Env] = {
          val lInt = getInt(interpIter(left, env))
          val rInt = getInt(interpIter(right, env))
          op(lInt, rInt)
        }

        // get value by evaluation
        def getValue(x: String): Value[Env] = envOps.findItem(env, x) match {
          case None => throw new InterpreterException("None")
          case Some(lazyVal) => lazyVal match {
            case LVVal(v) => v
            case lvLazy@LVLazy(expr, env, evaluated) => evaluated match {
              case Some(value) => value
              case None => {
                val value = interpIter(expr, env)
                lvLazy.evaluated = Some(value)
                value
              }
            }
          }
        }

        // get a function
        def getFunction(expr: Expr) = expr match {
          case EName(x) => getValue(x) match {
            case vFunc@VFunc(_, _, _, _) => vFunc
            case _ => throw new InterpreterException("Not a Function Type")
          }
          case _ => throw new InterpreterException("Not a Name Type")
        }

        // interpret expressions
        expr match {
          case EInt(n)  => VInt(n)
          case EName(x) => getValue(x)
          case EInL(e)  => VLeft (interpIter(e, env))
          case EInR(e)  => VRight(interpIter(e, env))
          case EMatch(value, lvName, lvCase, rvName, rvCase) => {
            interpIter(value, env) match {
              case VLeft(v)  => interpIter(lvCase, envOps.setItem(envOps.pushEmptyFrame(env), lvName, LVVal(v)))
              case VRight(v) => interpIter(rvCase, envOps.setItem(envOps.pushEmptyFrame(env), rvName, LVVal(v)))
              case _ => throw new InterpreterException("Not a Sum Type")
            }
          }
          case ENil => VNil
          case ECons(head, tail) => VCons(interpIter(head, env), interpIter(tail, env))
          case EFst(e) => interpIter(e, env) match {
            case VCons(head, _) => head
            case _ => throw new InterpreterException("Not a Product Type")
          }
          case ESnd(e) => interpIter(e, env) match {
            case VCons(_, tail) => tail
            case _ => throw new InterpreterException("Not a Product Type")
          }
          case EApp(f, args) => {
            val func = getFunction(f)
            if (func.params.size != args.size) throw new InterpreterException("Not Matching # of Params")
            val newEnv = func.params.zip(args).foldLeft(envOps.pushEmptyFrame(env)) { case (curEnv, (arg, exp)) =>
              arg match {
                case AVName(x) => envOps.setItem(curEnv, x, LVVal(interpIter(exp, env)))
                case ANName(x) => envOps.setItem(curEnv, x, LVLazy(exp, env, None))
              }
            }
            interpIter(func.body, newEnv)
          }
          case ELet(bindings, e) => {
            val newEnv = bindings.foldLeft(envOps.pushEmptyFrame(env)) { case (curEnv, bind) =>
              bind match {
                case BDef(f, params, body) => envOps.setItem(curEnv, f, LVVal(VFunc(f, params, body, curEnv)))
                case BVal(x, e)            => envOps.setItem(curEnv, x, LVVal(interpIter(e, curEnv)))
                case BLVal(x, e)           => envOps.setItem(curEnv, x, LVLazy(e, curEnv, None))
              }
            }
            interpIter(e, newEnv)
          }
          case ENilP(e) => interpIter(e, env) match {
            case VNil => TRUE
            case _ => FALSE
          }
          case EIntP(e) => interpIter(e, env) match {
            case VInt(_) => TRUE
            case _ => FALSE
          }
          case ESumP(e) => interpIter(e, env) match {
            case VLeft(_) | VRight(_) => TRUE
            case _ => FALSE
          }
          case EProdP(e) => interpIter(e, env) match {
            case VCons(_, _) => TRUE
            case _ => FALSE
          }
          case EPlus(left, right)   => intOp(left, right, (x,y) => VInt(x+y))
          case EMinus(left, right)  => intOp(left, right, (x,y) => VInt(x-y))
          case EMul(left, right)    => intOp(left, right, (x,y) => VInt(x*y))
          case EDiv(left, right)    => intOp(left, right, (x,y) => VInt(x/y))
          case EMod(left, right)    => intOp(left, right, (x,y) => VInt(x%y))
          case EEq(left, right)     => intOp(left, right, (x,y) => if (x == y) TRUE else FALSE)
          case ELt(left, right)     => intOp(left, right, (x,y) => if (x <  y) TRUE else FALSE)
          case EGt(left, right)     => intOp(left, right, (x,y) => if (x >  y) TRUE else FALSE)
        }
      }

      try {
        Success(interpIter(expr, envOps.emptyEnv()))
      }
      catch {
        case e: Exception => Failure(e)
      }
    }
  }
}

