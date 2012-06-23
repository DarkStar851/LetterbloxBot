import scala.io.Source.{fromFile}
import java.awt.{Robot}
import wordtree._

object bot {
  def main(args: Array[String]): Unit = {
    val wordtree = storeWordList("/usr/share/dict/words")
    val robot = new Robot
    val continueMsg = "Would you like to continue? Y/n: "
    var letters = ""
    
    //robot.delay(100)
    println(continueMsg)
    while (userWantsMore(readChar)) {
      letters = readLine("Enter your six letters: ")
      println("Click into the game screen now.")
      Thread.sleep(3000) // Give the user time to click game scr.
      for { num   <- 3 to 6 // Take only 3-6 letter words.
            combo <- letters.combinations(num)
            perm  <- combo.permutations
            if (wordtree.findWord(perm) != null)
      } enterWord(perm, robot)
      println(continueMsg)
    }
  }

  def enterWord(word: String, robot: Robot): Unit = {
    val enterKey = 10
    word.toUpperCase.foreach { char =>
      var letter = char.asInstanceOf[Int]
      robot.keyPress(letter)
      robot.keyRelease(letter)
    }
    robot.keyPress(enterKey)
    robot.keyRelease(enterKey)
  }

  def userWantsMore(answer: Char) = answer == 'y' || answer == 'Y'

  def storeWordList(filename: String) = {
    val wordtree = new WordTree
    fromFile(filename).mkString("").split("\n").foreach(word => {
      try { wordtree.addWord(word.toLowerCase()) }
      catch { // Handles the case where an unknown letter is found.
        case e: IndexOutOfBoundsException => println("Couldn't add " + word)
      }
    })
    wordtree
  }
}
