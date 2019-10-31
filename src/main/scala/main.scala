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
    attemptedReads      <- Right(index(directory))
    (failed, succeeded) <- Right(partitionTupleEither(attemptedReads))
    _                   <- Right(failed.foreach(x => println(x._2)))
    _                   <- Right(iterate(succeeded.toMap))
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
      file <- Try(new java.io.File(path)).fold(
               throwable => Left(InvalidPath(throwable)),
               file =>
                 if (file.exists) Left(FileDoesNotExist(file))
                 else if (!file.isDirectory) Left(NotDirectory(file))
                 else Right(file)
             )
    } yield file

  def partitionTupleEither[A, B, E](list: Seq[(A, Either[E, B])]): (Seq[(A, E)], Seq[(A, B)]) =
    list.partitionMap {
      case (a, Left(e))  => Left(a  -> e)
      case (a, Right(b)) => Right(a -> b)
    }

  def index(directory: File): Seq[(File, Either[FileUnreadable, Set[String]])] = {
    val files = directory.listFiles.sortBy(_.toString)
    files.map(file => file -> tryToReadFile(file).map(buildWordSets)).toSeq
  }

  def tryToReadFile(file: File): Either[FileUnreadable, Seq[String]] =
    (Try {
      val source   = scala.io.Source.fromFile(file)
      val rawWords = source.mkString.split(" ")
      source.close()
      rawWords.toIndexedSeq
    }).fold(_ => Left(FileUnreadable(file)), rawWords => Right(rawWords))

  def buildWordSets(rawInput: Seq[String]): Set[String] =
    rawInput.filter(isWord).map(_.toLowerCase).toSet

  def isWord(string: String): Boolean = {
    val OnlyLetters = "([A-Za-z]+)".r
    string match {
      case OnlyLetters(_) => true
      case _              => false
    }
  }

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
