package pp202002.hw3
import scala.annotation.tailrec
import scala.util.control.TailCalls._
/*
  Exercise 3: Enigma machine

  Implement Enigma machine described below.

  Enigma machine is encryption system used by Nazi Germany during World War II
  Alan Turing, the father of Computer Science, is famous for his development of Enigma-deciphering machine.

  Enigma machine encrypts an alphabet by the connections and arrangements of internal optical wires.
  The connection of the optical wire is composed of 3 'Rotors' and a single 'Reflector'.
  The overall encoding path is shown as below.
                  +++++++++++         +++++++++++         +++++++++++        ===
  input:  'A' --> |         | --'C'-> |         | --'D'-> |         | --'F'----+\
                  | Rotor 1 |         | Rotor 2 |         | Rotor 3 |          | |  Reflector
  output: 'B' <-- |         | <-'E'-- |         | <-'S'-- |         | <-'S'----+/
                  +++++++++++         +++++++++++         +++++++++++        ===
  Each Rotor and Reflector has internal optical 'Wire's. The wire maps each alphabet to another alphabet, e.g., 'A' to 'F'.
  At first, the input alphabet goes forward through the wires of three rotors, and 'reflected' by the reflector.
  Reflector maps an alphabet to another alphabet, and vice versa, e.g. 'A' to 'F', and 'F' to 'A'

  Because of the reflector, decoding path of Enigma is just reversal of the encoding path.
                  +++++++++++         +++++++++++         +++++++++++        ===
  output: 'A' <-- |         | <-'C'-- |         | <-'D'-- |         | <-'F'----+\
                  | Rotor 1 |         | Rotor 2 |         | Rotor 3 |          | |  Reflector
  input:  'B' --> |         | --'E'-> |         | --'S'-> |         | --'S'----+/
                  +++++++++++         +++++++++++         +++++++++++        ===

  Before the first input enters, the first rotor 'rotates' one tick, that means, the internal wire is entirely rotated by 1/26 round counterclockwise.
  The effect of rotations can be decomposed into two events, 1) rotating the whole connection, 2) and shifting left by one alphabet.
  If the first rotor has turned full circle, the seconds rotor rotates one tick, and so on.

  e.g.) Suppose that the first rotor initially maps 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' to 'QWERTYUIOPASDFGHJKLZXCVBNM'
        When we type first letter, the first rotor maps 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' to 'VDQSXTHNOZRCEFGIJKYWBUAMLP',
        so if we typed 'B', it would be encrypted into 'D'.
        (rotation of wire shifts the connections into 'WERTYUIOPASDFGHJKLZXCVBNMQ', then its output is shifted left,
         that makes the map to 'VDQSXTHNOZRCEFGIJKYWBUAMLP')

  Our Enigma machine uses 1 to 5 rotors and a single reflector.
  (We will not use a plugboard)

  EnigmaSettings contains a list of initial rotor settings and reflector settings.
  You should implement the backward path of wires, and the full encryption and decryption paths of Enigma machine.

  The input will be restricted to upper cased alphabets. ('A'-'Z')

  reference links:
  https://en.wikipedia.org/wiki/Enigma_rotor_details
  https://www.theguardian.com/technology/2014/nov/14/how-did-enigma-machine-work-imitation-game
  https://hackaday.com/2017/08/22/the-enigma-enigma-how-the-enigma-machine-worked/
  https://www.youtube.com/watch?v=QwQVMqfoB2E

  P.S.) As we ignore turnover notch positions and plugboard, our Enigma machine behaves 
        slightly differently than most of the web implementations of Enigma machine.
 */

/** The initial setting of Enigma machine
 *
 * We guarantee that the connection of reflectorState is involutive,
 * that means, forall x, reflectorState.forward(reflectorState.forward(x)) == x
 *
 * @param rotorState     The internal wire connections of each Rotor. The first object of the list should be the first rotor.
 * @param reflectorState The internal wire connection of Reflector.
 */
case class EnigmaSettings(rotorState: List[Wire], reflectorState: Wire)

object Enigma extends CipherGen[EnigmaSettings] {
    def buildEncryptor(initSetting: EnigmaSettings): Enigma = {        
        @tailrec
        def buildRotorsCont(rotors: List[Wire], cont: List[Rotor]=>TailRec[List[Rotor]]): List[Rotor] = {
            rotors match { 
                case Nil => cont(Nil).result
                case wire::tl => buildRotorsCont(tl, (r)=>tailcall(cont(Rotor(wire, 'A')::r)))
            }
        }
        val rotors = buildRotorsCont(initSetting.rotorState, (r)=>done(r))
        val reflector = Reflector(initSetting.reflectorState)
        new Enigma(rotors, reflector)
    }
    def buildDecryptor(initSetting: EnigmaSettings): Enigma = buildEncryptor(initSetting)
}

class Enigma(rotors: List[Rotor], reflector: Reflector) extends Encryptor with Decryptor {
    def encrypt(c: Char): (Char, Enigma) = {        
        @tailrec
        def buildNewRotorsCont(ls: List[Rotor], cont: List[Rotor]=>TailRec[List[Rotor]]): List[Rotor] = {
            ls match {
                case Nil => cont(Nil).result
                case hd::tl => {
                    if (hd.state == 'Z') buildNewRotorsCont(tl, (r)=>tailcall(cont(hd.tick::r)))
                    else cont(hd.tick::tl).result
                }
            }
        }
        val newRotors = buildNewRotorsCont(rotors, (r)=>done(r))
        
        @tailrec
        def rotorsForward(i: Int, c: Char): Char = {
            if (i >= newRotors.length) c
            else rotorsForward(i+1, newRotors(i).forward(c))
        }
        @tailrec
        def rotorsBackward(i: Int, c: Char): Char = {
            if (i < 0) c
            else rotorsBackward(i-1, newRotors(i).backward(c))
        }
        
        val c1 = rotorsForward(0, c)
        val c2 = reflector.forward(c1)
        val c3 = rotorsBackward(newRotors.length-1, c2)
        (c3, new Enigma(newRotors, reflector))
    }

    // Decryption of Enigma machine is same to the Encryption
    def decrypt(c: Char): (Char, Enigma) = encrypt(c)
}

sealed abstract class EnigmaParts {
    def forward(c: Char): Char
    def backward(c: Char): Char
}

case class Wire(connection: String) extends EnigmaParts {
    def forward(c: Char): Char = connection(c - 'A')
    def backward(c: Char): Char = (connection.indexOf(c) + 'A').toChar
}

case class Rotor(wire: Wire, state: Char) extends EnigmaParts {
    def forward(c: Char): Char = wire.forward(c)
    def backward(c: Char): Char = wire.backward(c)
    
    def caesarInc(c: Char): Char = Caesar.buildEncryptor(1).encrypt(c)._1
    def caesarDec(c: Char): Char = Caesar.buildEncryptor(25).encrypt(c)._1
    
    def tick: Rotor = {
        @tailrec
        def tickIter(i: Int, connection: String): String = {
            if (i >= wire.connection.length) connection + caesarDec(wire.connection(0)).toString
            else tickIter(i+1, connection + caesarDec(wire.connection(i)).toString)
        }
        Rotor(Wire(tickIter(1, "")), caesarInc(state))
    }
}

case class Reflector(wire: Wire) extends EnigmaParts {
    def forward(c: Char): Char = wire.forward(c)
    def backward(c: Char): Char = wire.backward(c)
}