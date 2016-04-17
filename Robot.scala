object Robot{
  def main(args:Array[String]){
    val lines = io.Source.fromFile("data/data.txt").getLines();
    
    val flag = (0,1,"flag");
    val commands = lines.toTraversable.toArray
    
    //println(commands(1))
    //for(line<-lines){
    if(commands(0).split(" ")(0).toLowerCase() == "place"){
      val coordinates = commands(0).split(" ")(1).split(",")
      val tup = (coordinates(0).toInt,coordinates(1).toInt,coordinates(2))
      //println(tup)
      println(calculate(commands,tup,flag))
    }
    else
       println("Not a valid command. Please place the robot on table first");
        
    //}
    
  }
  def calculate(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String)):String={
    //println(commands(flag._1))
    val a =1;
    commands(flag._1).toLowerCase().split(" ")(0) match {
      case "place" => place(commands,(commands(flag._1).split(" ")(1).split(",")(0).toInt,commands(flag._1).split(" ")(1).split(",")(1).toInt,commands(flag._1).split(" ")(1).split(",")(2)),flag)
      case "move"  => move(commands,tup,flag)
      case "right"  => return right(commands,tup,flag)
      case "left"  => return left(commands,tup,flag)
      case "report"  => return report(commands,tup,flag)
    }
  }
  def place(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String))={
    calculate(commands:Array[String],(tup._1,tup._2,tup._3),(flag._1+1,flag._2,flag._3))
  }
  def move(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String))={
    if(tup._3.toLowerCase() == "north"){
     calculate(commands:Array[String],(tup._1,tup._2+1,tup._3),(flag._1+1,flag._2,flag._3))
    }
    else if(tup._3.toLowerCase() == "south"){
      calculate(commands:Array[String],(tup._1,tup._2-1,tup._3),(flag._1+1,flag._2,flag._3))
    }
    else if(tup._3.toLowerCase() == "east"){
      calculate(commands:Array[String],(tup._1+1,tup._2,tup._3),(flag._1+1,flag._2,flag._3))
    }
    else{
      calculate(commands:Array[String],(tup._1-1,tup._2,tup._3),(flag._1+1,flag._2,flag._3))
    }
  }
  def right(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String))={
    if(tup._3.toLowerCase() == "north"){
     calculate(commands:Array[String],(tup._1,tup._2,"east"),(flag._1+1,flag._2,flag._3))
    }
    else if(tup._3.toLowerCase() == "south"){
      calculate(commands:Array[String],(tup._1,tup._2,"west"),(flag._1+1,flag._2,flag._3))
    }
    else if(tup._3.toLowerCase() == "east"){
      calculate(commands:Array[String],(tup._1,tup._2,"south"),(flag._1+1,flag._2,flag._3))
    }
    else{
      calculate(commands:Array[String],(tup._1,tup._2,"north"),(flag._1+1,flag._2,flag._3))
    }
  }
  
  def left(commands:Array[String],tup:(Int,Int,String),flag:(Int,Int,String))={
    if(tup._3.toLowerCase() == "north"){
     calculate(commands:Array[String],(tup._1,tup._2,"west"),(flag._1+1,flag._2,flag._3))
    }
    else if(tup._3.toLowerCase() == "south"){
      calculate(commands:Array[String],(tup._1,tup._2,"east"),(flag._1+1,flag._2,flag._3))
    }
    else if(tup._3.toLowerCase() == "east"){
      calculate(commands:Array[String],(tup._1,tup._2,"north"),(flag._1+1,flag._2,flag._3))
    }
    else{
      calculate(commands:Array[String],(tup._1,tup._2,"south"),(flag._1+1,flag._2,flag._3))
	}
  }
 }