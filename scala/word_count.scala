import scalax.io._
import scalax.file._
import scalax.file.defaultfs._
import scalax.file.LinkOption.NoFollowLinks
import scalax.file.PathMatcher.IsDirectory

object Counts {
	
	type Counter = Map[String, Int]
	
	def main(args: Array[String]) {
		val files = if (args.length < 2) getFiles(args(0)) else getFiles(args(0), args(1))
		if (files.isEmpty) println("nothing found in folder " + args(0))
		else {
			val counts = (files.par map (countWords)) reduce (countsMerge)
			val topTen = counts.toSeq.sortBy[Int](countSort)(Ordering.Int.reverse).take(10)
			topTen.foreach(println)
		}
	}

	/**
	 * Count the words contained in a file
	 */
	def countWords(path: Path): Counter = {
		def parseLine(words: Counter, line: Seq[String]): Counter = 
			if (line.isEmpty) words
			else {
				val h = line.head
				val wordsUpd = incrementCount(words, h)
				parseLine(wordsUpd, line.tail)
			}
		val r = Resource.fromFile(path.path)
		(r.lines() foldLeft Map[String, Int]()) ((words, line) => parseLine(words, line.split(' ')))
	}

    // a function value extracting the count from the (word, count)	 pair for count sorting
	val countSort: ((String, Int)) => Int = _._2

	/**
	 * Takes two Counters and merges the mapping, resulting in a 
	 * single map with all word counts
	 */
	def countsMerge(c1: Counter, c2: Counter): Counter = (c1 foldLeft c2) (updateCount)
	
	/**
	 * Extracts the paths to files contained in the directory.
	 * An optional filename filter is available
	 */
	def getFiles(dir: String, filter: String = "*.*"): Seq[Path] = {
		Path(dir) * filter
	}.toSeq
	
	/**
	 * Increments the counter for the specified string of 1 unit
	 * Returns the updated Counter
	 */
	def incrementCount(c: Counter, word: String): Counter = updateCount(c, (word, 1))

		
	/**
	 * Increments the counter for the specified string of an arbitrary number
	 * Returns the updated Counter
	 */
	def updateCount(c: Counter, update: (String, Int)): Counter = {
		val (word, incr) = update
		c + (word -> (c.getOrElse(word, 0) + incr))
	}
	
}
