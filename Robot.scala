object Robot {
  def main(args:Array[String]){
    val lines = io.Source.fromFile("data/data.txt").getLines();
    
    val tracker_flag = (0,1,"tracker_flag");
    val commands = lines.toTraversable.toArray
    
    //println(commands(1))
    //for(line<-lines){
    if(commands(0).split(" ")(0).toLowerCase() == "place"){
      val axis_data = commands(0).split(" ")(1).split(",")
      val coordinates = (axis_data(0).toInt,axis_data(1).toInt,axis_data(2))
      //println(coordinates)
      println(calculate(commands,coordinates,tracker_flag))
    }
    else
       println("Not a valid command. Robot is not placed on the table. Use 'PLACE' command as first argument");
        
    //}
    
  }
  def calculate(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String)):String={
    //println(tracker_flag._1+" "+commands.length)
    if(tracker_flag._1 >=  commands.length){
      //println("Enter")
      System.exit(0)
    }
    //val a =1;
    commands(tracker_flag._1).toLowerCase().split(" ")(0) match {
      case "place" => place(commands,(commands(tracker_flag._1).split(" ")(1).split(",")(0).toInt,commands(tracker_flag._1).split(" ")(1).split(",")(1).toInt,commands(tracker_flag._1).split(" ")(1).split(",")(2)),tracker_flag)
      case "move"  => move(commands,coordinates,tracker_flag)
      case "right"  => right(commands,coordinates,tracker_flag)
      case "left"  => left(commands,coordinates,tracker_flag)
      case "report"  => report(commands,coordinates,tracker_flag)
    }
  }
  def place(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    //if(on_table(coordinates))
      calculate(commands:Array[String],(coordinates._1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    //else
     // return 
  }
  def move(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    if(coordinates._3.toLowerCase() == "north"){
     calculate(commands:Array[String],(coordinates._1,coordinates._2+1,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else if(coordinates._3.toLowerCase() == "south"){
      calculate(commands:Array[String],(coordinates._1,coordinates._2-1,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else if(coordinates._3.toLowerCase() == "east"){
      calculate(commands:Array[String],(coordinates._1+1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else{
      calculate(commands:Array[String],(coordinates._1-1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
  }
  def right(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    if(coordinates._3.toLowerCase() == "north"){
     calculate(commands:Array[String],(coordinates._1,coordinates._2,"east"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else if(coordinates._3.toLowerCase() == "south"){
      calculate(commands:Array[String],(coordinates._1,coordinates._2,"west"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else if(coordinates._3.toLowerCase() == "east"){
      calculate(commands:Array[String],(coordinates._1,coordinates._2,"south"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else{
      calculate(commands:Array[String],(coordinates._1,coordinates._2,"north"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
  }
  
  def left(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    if(coordinates._3.toLowerCase() == "north"){
     calculate(commands:Array[String],(coordinates._1,coordinates._2,"west"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else if(coordinates._3.toLowerCase() == "south"){
      calculate(commands:Array[String],(coordinates._1,coordinates._2,"east"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else if(coordinates._3.toLowerCase() == "east"){
      calculate(commands:Array[String],(coordinates._1,coordinates._2,"north"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    else{
      calculate(commands:Array[String],(coordinates._1,coordinates._2,"south"),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
  }
  def report(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    /*if(commands.length != tracker_flag._1){
      calculate(commands,coordinates,tracker_flag)
    }*/
    println(coordinates._1+","+coordinates._2+","+coordinates._3)
    calculate(commands:Array[String],(coordinates._1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
  }
  def on_table(coordinates:(Int,Int,String)):Boolean={
    val matrix = 5;
    if(coordinates._1+1 <= 0 && coordinates._2 >= 0 && coordinates._1 <=matrix && coordinates._2 <=matrix){
      return true
    }
    else
      return false;
  }
}