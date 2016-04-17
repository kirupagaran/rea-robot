object Robot {
  def main(args:Array[String]){
    val lines = io.Source.fromFile("data/data.txt").getLines();
    val tup = (1,1,"N")
    val flag = (0,1,"flag");
    val commands = lines.toTraversable.toArray
    //println(commands(1))
    //for(line<-lines){
      println(calculate(commands,tup,flag))  
    //}
    
  }
  def calculate(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String)):String={
    println(commands(flag._1))
    commands(flag._1) match {
      case "place"  => place(commands,tup,flag)
      case "move"  => move(commands,tup,flag)
      //case "left"  => return place(commands,tup,flag)
      //case "right"  => return place(commands,tup,flag)
      case "report"  => return report(commands,tup,flag)
    }
  }
  def place(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String))={
    calculate(commands:Array[String],(tup._1,tup._2,"N"),(flag._1+1,flag._2,flag._3))
  }
  def move(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String))={
    calculate(commands:Array[String],(tup._1+1,tup._2+2,"N"),(flag._1+1,flag._2,flag._3))
  }
  def report(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String)):String={
    return tup._1+","+tup._2+","+tup._3;
  }
}