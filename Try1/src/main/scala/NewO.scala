object NewO {
  import java.io.BufferedReader
  import java.io.FileReader
  import java.util.HashMap
  import java.util.StringTokenizer

  class Interpretor {
    intVariables = new util.HashMap[String, Integer]
    boolVariables = new util.HashMap[String, Boolean]
    variables = new util.HashSet[String]
    var intVariables: util.Map[String, Integer] = null // map to store int variables names and values
    var boolVariables: util.Map[String, Boolean] = null
    var variables: util.Set[String] = null //store names of variables whose type has not been determined
    private[interpreter] def lineparser = { //parses code file to create a list of all the commands.
      val codeLines = new util.ArrayList[String]
      try {
        val br = new BufferedReader(new FileReader("interpreter/language.txt"))
        //buffered reader to read code file
        var line = null
        var s = null
        while ((s = br.readLine) != null) { //iterate through all lines
          if (s.contains(";")) {
            line = s.substring(0, s.indexOf(";")) //splits string from start point till semicolon
            codeLines.add(line)
            line = s.substring(s.indexOf(";") + 1, s.length - 1) //copy rest of string into line
            if (!line.isEmpty) { //if there is remaining string then add to line
              codeLines.add(line)
            }
          } else {
            line = br.readLine
            codeLines.add(line)
          }
        }
      } catch {
        case e: Exception => e.printStackTrace()
      }
      codeLines
    }

    private[interpreter] def languageParser(codeLines: util.List[String]): Unit = { //goes through the list of lines and executes language accordingly
      val key = null
      val value = 0
      for (s <- codeLines) {
        val parts = s.split("[\\s;]+")
        if (parts(0).equalsIgnoreCase("let")) { //parts[1] is variable name
          if (parts(2) == "=") { //= at i+2
            // check for boolean true
            if (parts(3) == "tt") this.boolVariables.put(parts(1), true) else if (parts(3) == "ff") { //check bool false
              this.boolVariables.put(parts(1), false)
            } else if (parts(3).matches("\\d+")) { //check if int
              this.intVariables.put(parts(1), parts(3).toInt)
            }
          } else variables.add(parts(1))
        } else if (variables.contains(parts(0)) || intVariables.containsKey(parts(0)) || boolVariables.containsKey(parts(0))) if (parts(1) == "=") if (parts(2) == "tt") if (variables.contains(parts(0))) {
          boolVariables.put(parts(0), true)
          variables.remove(parts(0))
        } else if (boolVariables.containsKey(parts(0))) boolVariables.replace(parts(0), true) else if (parts(2) == "ff") if (variables.contains(parts(0))) {
          boolVariables.put(parts(0), false)
          variables.remove(parts(0))
        } else if (boolVariables.containsKey(parts(0))) boolVariables.replace(parts(0), false) else if (parts(2).matches("\\d+")) if (variables.contains(parts(0))) {
          intVariables.put(parts(0), parts(2).toInt)
          variables.remove(parts(0))
        } else if (parts(3) == "+" || parts(3) == "-") if (parts(3) == "+") if (parts(4).matches("\\d+")) {
          var num = intVariables.get(parts(0))
          num = num + parts(4).toInt
          intVariables.replace(parts(0), num)
        } else {
          var num = intVariables.get(parts(0))
          val n2 = intVariables.get(parts(4))
          num = num + n2
          intVariables.replace(parts(0), num)
        } else if (parts(3) == "-") if (parts(4).matches("\\d+")) {
          var num = intVariables.get(parts(0))
          num = num - parts(4).toInt
          intVariables.replace(parts(0), num)
        } else {
          var num = intVariables.get(parts(0))
          val n2 = intVariables.get(parts(4))
          num = num - n2
          intVariables.replace(parts(0), num)
        } else if (parts(3) == "*") if (parts(4).matches("\\d+")) {
          var num = intVariables.get(parts(0))
          num = num * parts(4).toInt
          intVariables.replace(parts(0), num)
        } else {
          var num = intVariables.get(parts(0))
          val n2 = intVariables.get(parts(4))
          num = num * n2
          intVariables.replace(parts(0), num)
        }
      }
    }
  }


}