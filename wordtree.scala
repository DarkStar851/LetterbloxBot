package wordtree

class Node(val letter: Char, var endsWord: Boolean, var connections: List[Node] = List()) {
  def getLink(letter: Char): Node = {
    for (connection <- connections) {
      if (connection.letter == letter)
        return connection
    }
    null.asInstanceOf[Node]
  }

  def addLink(letter: Char, endsWord: Boolean) = 
    connections = (new Node(letter, endsWord)) :: connections

  def printable: String = 
    "(" + letter.toString() + "->" + connections.map(_.printable).mkString(", ") + ")"
}

class WordTree() {
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  var base = alphabet.map(c => new Node(c.asInstanceOf[Char], false))

  def addWord(word: String, n: Node = null): Node = {
    var node = n
    var link = n

    if (word.length == 0) return null.asInstanceOf[Node]
    if (n == null) node = base(alphabet.indexOf(word(0)))
    if (word.length == 1) {
      node.endsWord = true
      return node
    }
    link = node.getLink(word(1).asInstanceOf[Char])
    if (link != null) return addWord(word.slice(1, word.length), link)
    node.addLink(word(1).asInstanceOf[Char], false)
    link = node.getLink(word(1).asInstanceOf[Char])
    addWord(word.slice(1, word.length), link)
  }

  def findWord(word: String, n: Node = null): Node = {
    var node = n
    var link = n

    if (n == null) node = base(alphabet.indexOf(word(0)))
    if (word.length == 1) 
      return if (node.endsWord) node else null.asInstanceOf[Node]
    link = node.getLink(word(1).asInstanceOf[Char])
    if (link != null) return findWord(word.slice(1, word.length), link)
    return null.asInstanceOf[Node]
  }
}
