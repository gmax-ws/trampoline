package scalable.solutions

import java.io.{File, FileInputStream, FileOutputStream, InputStreamReader}
import java.nio.charset.{Charset, StandardCharsets}
import scala.io.Source
import scala.util.{Try, Using}

object Test extends App {

  /**
    * Create dictionary loaded from a file.
    * Key, Value separator is by default ':'
    *
    * @param file File name containing dictionary data.
    * @return Dictionary map.
    */
  def init(file: String, separator: Char = ':'): Try[Map[String, String]] = {
    Using(Source.fromFile(file)) { source =>
      val dict = scala.collection.mutable.HashMap[String, String]()
      source.getLines.foreach { line =>
        val kv = line.split(separator)
        if (kv.length == 2)
          dict.put(kv(0), kv(1))
      }
      dict.toMap
    }
  }

  /**
    * Parse input file character by character, extract words and
    * replace them if they are found in dictionary and write to an output file.
    *
    * @param dict Dictionary object
    * @param inFile input file name
    * @param outFile output file name
    * @param charSet character set (default UTF-8)
    */
  def parse(
      dict: Map[String, String],
      inFile: String,
      outFile: String,
      charSet: Charset = StandardCharsets.UTF_8
  ): Unit =
    Using.resources(
      new InputStreamReader(new FileInputStream(new File(inFile)), charSet),
      new FileOutputStream(new File(outFile))
    ) { (in, out) =>
      val wordBuffer = new StringBuilder()
      var byte = in.read()
      while (byte != -1) {
        val ch = byte.toChar
        // FIXME at this moment let assert a word contains only successive letters and digits
        if (ch.isLetterOrDigit) {
          wordBuffer.append(ch)
        } else {
          if (wordBuffer.nonEmpty) {
            val word = wordBuffer.toString
            dict.get(word) match {
              case Some(v) =>
                out.write(v.getBytes)
                println(s"replaced [$word] with [$v]")
              case None => out.write(word.getBytes)
            }
            wordBuffer.clear()
          }
          out.write(byte)
        }
        byte = in.read()
      }
    }

  val path = "/home/mag/Desktop"
  init(s"$path/dict.txt").toEither match {
    case Right(dict) =>
      println(dict)
      parse(dict, s"$path/input.txt", s"$path/output.txt")
    case Left(e) => e.printStackTrace()
  }
}
