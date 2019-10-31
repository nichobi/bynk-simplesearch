package test

import java.io.File
import scala.util.Try

object SimpleSearch extends App {
  //This is a workaround to allow the (failures, success) tuple decomposition in the for statement,
  //neccessary because of this bug in the scala compiler: https://github.com/scala/bug/issues/5589
  implicit class FilterableEither[E, T](x: Either[E, T]) {
    def withFilter(p: T => Boolean): Either[E, T] = x
  }
  import Program._
  (for {
    directory           <- readFile(args)
    (failed, succeeded) <- Right(index(directory))
    _ <- Right(failed.foreach {
          case (file, error) => println(s"Skipped file $file due to error: $error")
        })
    _ <- Right(iterate(succeeded.toMap))
  } yield ()).fold(
    err => println(s"SimpleSearch exited with an error: $err"),
    _ => println("Exiting SimpleSearch")
  )
}
object Program {

  import scala.io.StdIn.readLine
  sealed trait ReadFileError
  final case object MissingPathArg              extends ReadFileError
  final case class NotDirectory(file: File)     extends ReadFileError
  final case class FileDoesNotExist(file: File) extends ReadFileError
  final case class InvalidPath(t: Throwable)    extends ReadFileError
  final case class FileUnreadable(file: File)   extends ReadFileError

  def readFile(args: Array[String]): Either[ReadFileError, File] =
    for {
      path <- args.headOption.toRight(MissingPathArg)
      file <- Try(new File(path)).fold(
               throwable => Left(InvalidPath(throwable)),
               file =>
                 if (!file.exists) Left(FileDoesNotExist(file))
                 else if (!file.isDirectory) Left(NotDirectory(file))
                 else Right(file)
             )
    } yield file

  /**
    * Attempts to read all the files in the supplied directory, and returns two lists,
    * one with the files that coudln't be read and the relevant error;
    * and one with the successfully read files along with their contents as a list of unprocessed words.
    */
  def index(directory: File): (Seq[(File, Throwable)], Seq[(File, Set[String])]) = {
    val files     = directory.listFiles.toSeq
    val readFiles = files.map(file => file -> tryToReadFile(file).map(buildWordSets))
    readFiles.partitionMap {
      case (file, Left(err))      => Left(file  -> err)
      case (file, Right(wordSet)) => Right(file -> wordSet)
    }
  }

  /**
    * Tries to read a file and returns either a Throwable if the file can't be read,
    * or a sequence of the words it contains (split by " ")
    */
  def tryToReadFile(file: File): Either[Throwable, Seq[String]] =
    (Try {
      val source   = scala.io.Source.fromFile(file)
      val rawWords = source.mkString.split(" ")
      source.close()
      rawWords.toIndexedSeq
    }).fold(err => Left(err), rawWords => Right(rawWords))

  /**
    * Takes a sequence of Strings and builds a set of each word, converted to lowercase
    */
  def buildWordSets(rawInput: Seq[String]): Set[String] =
    rawInput.filter(isWord).map(_.toLowerCase).toSet

  def isWord(string: String): Boolean = {
    val OnlyLetters = "([A-Za-z]+)".r
    string match {
      case OnlyLetters(_) => true
      case _              => false
    }
  }

  /**
    * Prompts the user for a string, and if it isn't an exit keyword, searches the indexed files for it
    * and displays the results, before looping
    */
  def iterate(indexedFiles: Map[File, Set[String]]): Unit = {
    val searchString = readLine("Query> ")
    if (searchString != ":quit" && searchString != ":q") {
      val inputWords = searchString.split(" ").map(_.toLowerCase).toIndexedSeq
      findResults(inputWords, indexedFiles).fold(
        searchError => println(searchError),
        println
      )
      iterate(indexedFiles)
    }
  }

  sealed trait SearchError
  case class NoValidWords(ignoredWords: Seq[String]) extends SearchError
  case object NoWordsGiven                           extends SearchError
  case class NoMatchesFound(words: Seq[String])      extends SearchError

  case class Result(file: File, score: Int) {
    override def toString = s"$score% - $file"
  }

  case class SearchResult(results: Seq[Result], invalidWords: Seq[String]) {
    val ignoredString =
      if (invalidWords.isEmpty) "" else s"Ignored invalid words: ${invalidWords.mkString(", ")}\n"
    override def toString =
      s"${ignoredString}Matches:\n${results.sortBy(-_.score).take(10).mkString("\n")}"
  }

  /**
    * Parses the search words, checks which ones are valid words, and then makes sure we have at
    * least one valid word. If so, return a SearchResult with the files containing at leat one
    * word. Returns a SearchError if the search is invalid
    */
  def findResults(
      inputWords: Seq[String],
      indexedFiles: Map[File, Set[String]]
  ): Either[SearchError, SearchResult] =
    if (inputWords.isEmpty) Left(NoWordsGiven)
    else {
      val (validWords, invalidWords) = inputWords.partition(isWord)
      if (validWords.isEmpty) Left(NoValidWords(invalidWords))
      else {
        val results      = score(inputWords, indexedFiles)
        val passedScores = results.filter(_.score > 0)
        if (passedScores.isEmpty) Left(NoMatchesFound(validWords))
        else Right(SearchResult(passedScores, invalidWords))
      }
    }

  /**
    * Calculates the percentage of input words contained in each file, and returns a sequence of
    * Results, containing a file and a score
    */
  def score(
      inputWords: Seq[String],
      indexedFiles: Map[File, Set[String]]
  ): Seq[Result] = {
    def percentage(a: Int, b: Int): Int = (a * 100.0 / b + 0.5).toInt

    val results = for {
      (file, wordSet) <- indexedFiles
    } yield {
      val matchCount = inputWords.count(wordSet.contains)
      val score      = percentage(matchCount, inputWords.size)
      Result(file, score)
    }
    results.toSeq
  }

}
