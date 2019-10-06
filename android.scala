import collection.mutable.Buffer
import collection.mutable.Map
import io.StdIn._

class Being() {
  val brain = new Map[String, Assoc]()
  var feelings: Assoc

  val memLength = 5
  val memory = new Array[String](memLength)

  def updateMemory(assoc: Assoc) = {
    memory[memLength - 1] = null
    memory += assoc.action
    if(!brain.contains(assoc.action))
      brain += (assoc.action, assoc)
    for(action <- memory, if action != null)
      brain(action).addAssoc(assoc)
    feelings = brain(assoc.action) //subject to change: acts purely based on opposition
  }
  def chooseAction() = {
    val emotionChosen = feelings.maxEmotion
    var maxAssoc: Assoc = brain.head
    for((action, assoc) <- brain) {
      if(assoc.emotions(emotionChosen) > maxAssoc(emotionChosen))
        maxAssoc = assoc
    }
    println(maxAssoc.action)
    memory += assoc.action
  }
}

class Assoc(val action: String) {
  val emotions = Map[String, Double] = (
    "anger" -> 0.0,
    "sadness" -> 0.0,
    "pleasure" -> 0.0,
    "peace" -> 0.0)
  def addAssoc(assoc: Assoc) = {
    emotions("anger") += assoc.emotions("anger")
    emotions("sadness") += assoc.emotions("sadness")
    emotions("pleasure") += assoc.emotions("pleasure")
    emotions("peace") += assoc.emotions("peace")
  }
  def maxEmotion() = emotions.max()
}
