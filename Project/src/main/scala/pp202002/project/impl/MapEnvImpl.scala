package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._
import pp202002.project.impl.ExprInterpreter.InterpreterException

import scala.annotation.tailrec

object MapEnvImpl {
  implicit val mapEnvImpl: EnvOps[MapEnv, Value[MapEnv]] = {
    new EnvOps[MapEnv, Value[MapEnv]] {
      def emptyEnv(): MapEnv = new MapEnv(Nil)

      def pushEmptyFrame(env: MapEnv): MapEnv = new MapEnv(Map[String, EnvVal]() :: env.frames)

      def popFrame(env: MapEnv): MapEnv = env.frames match {
        case Nil => throw new InterpreterException("No Available Frames")
        case _::tl => new MapEnv(tl)
      }

      def setItem(
          env: MapEnv,
          name: String,
          item: EnvVal
      ): MapEnv = env.frames match {
        case Nil => throw new InterpreterException("No Available Frames")
        case hd::tl => new MapEnv((hd + (name->item)) :: tl)
      }

      def findItem(
          env: MapEnv,
          name: String
      ): Option[EnvVal] = {
        @tailrec
        def findItemIter(frames: List[Frame[EnvVal]]): Option[EnvVal] = frames match {
          case Nil => None
          case hd::tl => {
            hd.get(name) match {
              case None => findItemIter(tl)
              case _ => hd.get(name)
            }
          }
        }
        findItemIter(env.frames)
      }
    }
  }
}
