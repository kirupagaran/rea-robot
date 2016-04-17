object Robot1 {
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
       println("Not a valid command. Robot is not placed on the table. Use 'PLACE' as first coomand to robot");
        
    //}
    
  }
  def calculate(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String)):String={
    //println(tracker_flag._1+" "+commands.length)
    if(tracker_flag._1 >=  commands.length){
      //println("Enter")
      System.exit(0)
    }
   
    commands(tracker_flag._1).toLowerCase().split(" ")(0) match {
      case "place" => place(commands,(commands(tracker_flag._1).split(" ")(1).split(",")(0).toInt,commands(tracker_flag._1).split(" ")(1).split(",")(1).toInt,commands(tracker_flag._1).split(" ")(1).split(",")(2)),tracker_flag)
      case "move"  => move(commands,coordinates,tracker_flag)
      case "right"  => right(commands,coordinates,tracker_flag)
      case "left"  => left(commands,coordinates,tracker_flag)
      case "report"  => report(commands,coordinates,tracker_flag)
    }
  }
  def place(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    if(!on_table("place",coordinates)){
      println("Cannot place the robot. Issue with Coordinates("+coordinates._1+","+coordinates._2+")")
      System.exit(0)
    }
      
    calculate(commands:Array[String],(coordinates._1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    //else
     // return 
  }
  def move(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    if(!on_table("move",coordinates)){
      println("Cannot move the Robot on "+coordinates._3.toUpperCase+" after the coordinates ("+coordinates._1+","+coordinates._2+")")
      System.exit(0)
  def on_table(command_name:String,coordinates:(Int,Int,String)):Boolean={
    //specify the matrix on which the robot is going to move
    val matrix = 5;
    if(command_name.toLowerCase == "place" && coordinates._1 >=0 && coordinates._1 < matrix && coordinates._2 >= 0 && coordinates._2 < matrix){
      return true;
    }
    else if(command_name.toLowerCase == "move"){
      coordinates._3.toLowerCase match {
        case "north" => if(coordinates._2+1 < matrix) return true else false
        case "south" => if(coordinates._2-1 >= 0 ) return true else false
        case "east" => if(coordinates._1+1 < matrix ) return true else false
        case "west" => if(coordinates._1-1 >= 0 ) return true else false
      }
    }
    else
      return false;
  }
}