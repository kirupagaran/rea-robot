/*
 * Project: REA ROBOT
 * Author: Kirupa Devarajan
 * Date: 17/04/2016
 * Input Source: File
 * Input location: data/data.txt 
 */
object Robot {
  def main(args:Array[String]){
    val lines = io.Source.fromFile("data/data.txt").getLines();
    val tracker_flag = (0,1,"tracker_flag");
    val commands = lines.toTraversable.toArray
    
    //Check whether the first command is PLACE
    if(commands(0).split(" ")(0).toLowerCase() == "place"){
      val axis_data = commands(0).split(" ")(1).split(",")
      val coordinates = (axis_data(0).toInt,axis_data(1).toInt,axis_data(2))
      println(calculate(commands,coordinates,tracker_flag))
    }
    else
       println("Not a valid command. Robot is not placed on the tabletop. Use 'PLACE' as first coomand to robot");
  }
  
  /*
   * Method: calculate();
   * Purpose: Recursive method to process each of the commands
   */
  def calculate(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String)):String={
    
    //Condition to break the recursion
    if(tracker_flag._1 >=  commands.length){
      System.exit(0)
    }
    //Process each command
    commands(tracker_flag._1).toLowerCase().split(" ")(0) match {
      case "place" => place(commands,(commands(tracker_flag._1).split(" ")(1).split(",")(0).toInt,commands(tracker_flag._1).split(" ")(1).split(",")(1).toInt,commands(tracker_flag._1).split(" ")(1).split(",")(2)),tracker_flag)
      case "move"  => move(commands,coordinates,tracker_flag)
      case "right"  => right(commands,coordinates,tracker_flag)
      case "left"  => left(commands,coordinates,tracker_flag)
      case "report"  => report(commands,coordinates,tracker_flag)
    }
  }
  
   /*
   * Method: place();
   * Purpose: Method to process the command PLACE
   */
  def place(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    //Check whether the robot is on table. If not, do not process other command and exit
    if(!on_table("place",coordinates)){
      println("Cannot place the robot. Placement out of table. Issue with Coordinates("+coordinates._1+","+coordinates._2+")")
      System.exit(0)
    }
    //If Robot is on the table, process other commands
    calculate(commands:Array[String],(coordinates._1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))

  }
  
  
   /*
   * Method: move();
   * Purpose: Method to process MOVE command
   */
  def move(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    //Check whether the robot is on table. If not, ignore the MOVE command and process the next command
    if(!on_table("move",coordinates)){
      calculate(commands:Array[String],(coordinates._1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
    }
    
    //Move robot based on the directions
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
  
   /*
   * Method: right();
   * Purpose: Method to process RIGHT command
   */
  def right(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    
    //Turn the robot 90 degrees clockwise based on current face of the robot
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
  
  
   /*
   * Method: right();
   * Purpose: Method to process LEFT command
   */
  def left(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    //Turn the robot 90 degrees anti-clockwise based on current face of the robot
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
  
   /*
   * Method: report();
   * Purpose: Method to process REPORT command
   */
  def report(commands:Array[String],coordinates:(Int,Int,String),tracker_flag:(Int,Int,String))={
    //Print the correct coordinates and process the next command
    println(coordinates._1+","+coordinates._2+","+coordinates._3.toUpperCase)
    calculate(commands:Array[String],(coordinates._1,coordinates._2,coordinates._3),(tracker_flag._1+1,tracker_flag._2,tracker_flag._3))
  }
  
   /*
   * Method: on_table();
   * Purpose: Method to check robot is not going away from table
   */
  def on_table(command_name:String,coordinates:(Int,Int,String)):Boolean={
    //specify the matrix on which the robot is going to move
    val matrix = 5;
    //check whether the robot is on table during PLACE command
    if(command_name.toLowerCase == "place" && coordinates._1 >=0 && coordinates._1 < matrix && coordinates._2 >= 0 && coordinates._2 < matrix){
      return true;
    }
    ////check whether the robot is on table during MOVE command
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