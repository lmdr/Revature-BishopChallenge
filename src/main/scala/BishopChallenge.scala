/**
 *
 *                Chess Board
 *
 *      a   b   c   d   e   f   g   h
 *  8   W   B   W   B   W   B   W   B   8
 *  7   B   W   B   W   B   W   B   W   7
 *  6   W   B   W   B   W   B   W   B   6
 *  5   B   W   B   W   B   W   B   W   5
 *  4   W   B   W   B   W   B   W   B   4
 *  3   B   W   B   W   B   W   B   W   3
 *  2   W   B   W   B   W   B   W   B   2
 *  1   B   W   B   W   B   W   B   W   1
 *      a   b   c   d   e   f   g   h
 *
 *  Board:
 *  1. Empty beside bishop.
 *  2. Edges do not cycle.
 *  3. Alphabetical letters are horizontal location component.
 *  4. Numerics are vertical location component.
 *
 *  Bishop:
 *  1. Can move any number of squares diagonally.
 *  2. Cannot move off starting color.
 *  3. Can reach all 32 squares of the same color within 2 moves.
 *  4. Moves, starting and ending locations provided by user.
 *
 */


object BishopChallenge {
  def convert_to_tuple(location: String): (Int, Int) = {
    val location_pattern = "^([a-h])([1-8])$".r
    val location_pattern(horizontal, vertical) = location
    // " " is a filler in alphabet mapping
    val alphabet_map = Array(" ", "a", "b", "c", "d", "e", "f", "g", "h")
    (alphabet_map.indexOf(horizontal), vertical.toInt)
  }

  def find_adjacent_locations(location: (Int, Int)): Set[(Int, Int)] = {
    def find_adjacent_locations_direction(location: (Int, Int), f_horizontal: (Int, Int) => Int, f_vertical: (Int, Int) => Int): Set[(Int, Int)] = {
      val adjacent_locations_generator = for (i <- 0 to 8 if
          0 < f_horizontal(location._1, i) && f_horizontal(location._1, i) < 9 &&
          0 < f_vertical(location._2, i) && f_vertical(location._2, i) < 9)
        yield (f_horizontal(location._1, i), f_vertical(location._2, i))
      Set.from(adjacent_locations_generator)
    }

    Set.concat(
      find_adjacent_locations_direction(location, _ + _, _ + _),
      find_adjacent_locations_direction(location, _ + _, _ - _),
      find_adjacent_locations_direction(location, _ - _, _ + _),
      find_adjacent_locations_direction(location, _ - _, _ - _))
  }

  def build_moves_map(location: (Int, Int)): scala.collection.mutable.Map[Int, Set[(Int, Int)]] = {
    val moves_map = scala.collection.mutable.Map(0 -> Set(location))
    for (i <- 1 to 2) {
      moves_map(i) = Set()
      for (l <- moves_map(i - 1))
        moves_map(i) = moves_map(i).concat(find_adjacent_locations(l))
    }
    moves_map
  }

  def main(args: Array[String]): Unit = {
    println("Bishop Challenge: Can the Bishop move to a location on the board in given moves?")
    println("Enter 0 to exit. Enter start location, end location, and number of available moves.")
    println("Example: e4 g6 2")

    var input = scala.io.StdIn.readLine("start end moves: ")

    while (input != "0") {
      val f_input = input.toLowerCase.split(" ")
      val start = convert_to_tuple(f_input(0))
      val end = convert_to_tuple(f_input(1))
      val moves = f_input(2).toInt
      val moves_map = build_moves_map(start)

      // Test moves_map if end locations is reachable within 2 moves or less
      // Each key is number of moves taken to reach the end location
      // Each value is a set of all ending locations reachable
      if (moves_map(scala.math.min(moves, 2)).contains(end))
        println(s"${f_input(1)} is reachable from ${f_input(0)} within ${f_input(2)} move(s).")
      else
        println(s"Cannot reach ${f_input(1)} within ${f_input(2)} move(s).")

      input = scala.io.StdIn.readLine("start end moves: ")
    }

    println("Exiting...")
  }
}
