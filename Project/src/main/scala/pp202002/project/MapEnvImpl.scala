package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._
import pp202002.project.impl.ExprInterpreter.InterpreterException

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object MapEnvImpl {
  implicit val mapEnvImpl: EnvOps[MapEnv, Value[MapEnv]] =
    new EnvOps[MapEnv, Value[MapEnv]] {
      def emptyEnv(): MapEnv = new MapEnv(Nil)

      def pushEmptyFrame(env: MapEnv): MapEnv = new MapEnv(new HashMap[String, EnvVal] :: env.frames)

      def popFrame(env: MapEnv): MapEnv = env.frames match {
        case Nil => throw new InterpreterException("No frames to popFrame")
        case _ :: tl => new MapEnv(tl)
      }

      def setItem(
          env: MapEnv,
          name: String,
          item: EnvVal
      ): MapEnv = env.frames match {
        case Nil => throw new InterpreterException("No frames to setItem")
        case hd :: tl => new MapEnv((hd + (name -> item)) :: tl)
      }

      def findItem(
          env: MapEnv,
          name: String
      ): Option[EnvVal] = {

        @tailrec
        def findItemItr(frames: List[Frame[EnvVal]]): Option[EnvVal] = {
          frames match {
            case Nil => None
            case hd :: tl => {
              val f = hd.get(name)
              f match {
                case None => findItemItr(tl)
                case _ => f
              }
            }
          }
        }

        findItemItr(env.frames)
      }
    }
}
