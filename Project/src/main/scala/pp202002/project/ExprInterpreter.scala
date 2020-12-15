package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._

import scala.util.{Failure, Success, Try}

object ExprInterpreter {
  class InterpreterException(val reason: String) extends Exception {
    override def getMessage: String = reason
  }

  implicit def exprInterpreter[Env](implicit
      envOps: EnvOps[Env, Value[Env]]
  ): Interpreter[Expr, Value[Env]] = new Interpreter[Expr, Value[Env]] {

    def interp(expr: Expr): Try[Value[Env]] = {

      def interpItr(expr: Expr, env: Env): Value[Env] = {
        expr match {
          case ELet(bindings, e) => {
            val builtEnv = bindings.foldLeft(envOps.pushEmptyFrame(env)) { (currentEnv, bind) =>
              bind match {
                case BDef(f, params, body) => envOps.setItem(currentEnv, f, LVVal(VFunc(f, params, body, currentEnv)))
                case BVal(x, e) => envOps.setItem(currentEnv, x, LVVal(interpItr(e, currentEnv)))
                case BLVal(x, e) => envOps.setItem(currentEnv, x, LVLazy(e, currentEnv, None))
              }
            }

            interpItr(e, builtEnv)
          }
          case EName(x) => findItemAfterEvaluation(x, env)
          case EApp(f, args) => {
            val fVal = f match {
              case EName(fName) => findItemAfterEvaluation(fName, env) match {
                case vFunc @ VFunc(_, _, _, _) => vFunc
                case _ => throw new InterpreterException(s"$f is not function type")
              }
            }

            if (fVal.params.size != args.size) {
              throw new InterpreterException(s"The number of arguments is not equal to required value of $fVal")
            }

            val functionEnv = fVal.params.zip(args).foldLeft(envOps.pushEmptyFrame(env)) { case (currentEnv: Env, (arg: Arg, e: Expr)) =>
              arg match {
                case AVName(x) => envOps.setItem(currentEnv, x, LVVal(interpItr(e, env)))
                case ANName(x) => envOps.setItem(currentEnv, x, LVLazy(e, env, None))
              }
            }

            interpItr(fVal.body, functionEnv)
          }
          case EMatch(value, lvName, lvCase, rvName, rvCase) => {
            interpItr(value, env) match {
              case VLeft(v) => interpItr(lvCase, envOps.setItem(envOps.pushEmptyFrame(env), lvName, LVVal(v)))
              case VRight(v) => interpItr(rvCase, envOps.setItem(envOps.pushEmptyFrame(env), rvName, LVVal(v)))
              case _ => throw new InterpreterException(s"Evaluated value of match should be sum type")
            }
          }
          case EInt(n) => VInt(n)
          case EInL(e) => VLeft(interpItr(e, env))
          case EInR(e) => VRight(interpItr(e, env))
          case ENil => VNil
          case ECons(head, tail) => VCons(interpItr(head, env), interpItr(tail, env))
          case EFst(e) => interpItr(e, env) match {
            case VCons(head, _) => head
            case _ => throw new InterpreterException(s"$e is not product type")
          }
          case ESnd(e) => interpItr(e, env) match {
            case VCons(_, tail) => tail
            case _ => throw new InterpreterException(s"$e is not product type")
          }
          case ENilP(e) => interpItr(e, env) match {
            case VNil => TRUE
            case _ => FALSE
          }
          case EIntP(e) => interpItr(e, env) match {
            case VInt(_) => TRUE
            case _ => FALSE
          }
          case ESumP(e) => interpItr(e, env) match {
            case VLeft(_) | VRight(_) => TRUE
            case _ => FALSE
          }
          case EProdP(e) => interpItr(e, env) match {
            case VCons(_, _) => TRUE
            case _ => FALSE
          }
          case EPlus(left, right) => integerOp(left, right, env, (l, r) => VInt(l + r))
          case EMinus(left, right) => integerOp(left, right, env, (l, r) => VInt(l - r))
          case EMul(left, right) => integerOp(left, right, env, (l, r) => VInt(l * r))
          case EDiv(left, right) => integerOp(left, right, env, (l, r) => VInt(l / r))
          case EMod(left, right) => integerOp(left, right, env, (l, r) => VInt(l % r))
          case EEq(left, right) => integerOp(left, right, env, (l, r) => if (l == r) TRUE else FALSE)
          case ELt(left, right) => integerOp(left, right, env, (l, r) => if (l < r) TRUE else FALSE)
          case EGt(left, right) => integerOp(left, right, env, (l, r) => if (l > r) TRUE else FALSE)
        }
      }

      def findItemAfterEvaluation(x: String, env: Env): Value[Env] = envOps.findItem(env, x) match {
        case None => throw new InterpreterException(s"No item was found for name=$x")
        case Some(lazyVal) => lazyVal match {
          case LVVal(v) => v
          case lvLazy @ LVLazy(expr, env, evaluated) => evaluated match {
            case Some(value) => value
            case None => {
              val evaluatedValue = interpItr(expr, env)
              lvLazy.evaluated = Some(evaluatedValue)
              evaluatedValue
            }
          }
        }
      }

      def integerOp(left: Expr, right: Expr, env: Env, op: (Int, Int) => Value[Env]): Value[Env] = {
        val l = interpItr(left, env) match {
          case VInt(v) => v
          case _ =>   throw new InterpreterException(s"$left is not integer type")
        }

        val r = interpItr(right, env) match {
          case VInt(v) => v
          case _ =>   throw new InterpreterException(s"$right is not integer type")
        }

        op(l, r)
      }

      try {
        Success(interpItr(expr, envOps.emptyEnv()))
      } catch {
        case ex: Exception => Failure(ex)
      }
    }

  }

  private val TRUE = VRight(VInt(0))
  private val FALSE = VLeft(VInt(0))
}
