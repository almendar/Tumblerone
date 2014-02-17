package pl.tk.tumblerone

import java.net.{HttpURLConnection,URL}
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.nio.file.{Paths, Path}
import scala.util.Try


trait Logger {
  def log(msg:String)
  def info(msg:String) = log(s"[INFO]: $msg")
  def error(msg:String) = log(s"[ERROR]: $msg")
}


trait ConsoleLogger extends Logger {
  override def log(msg:String) = println(msg)
}

class FileDownloader(saveToLocation : Path) {
  self : Logger =>

  def download(url:URL) {

    val fileName = extractFileNameFromUrl(url)

    val byteContent = Try {
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      val in = connection.getInputStream
      val byteArray = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
      byteArray
    }

    val outStream = Try {
      if(!saveToLocation.toFile.exists()) saveToLocation.toFile.mkdirs()
      val file = saveToLocation.resolve(fileName).toFile
      if(file.exists) {log(s"File already exists, skipping ${fileName}") ; return}
      val out = new BufferedOutputStream(new FileOutputStream(file))
      out
    }

    for {
      b <- byteContent
      os <- outStream
    } {
      os.write(b)
      os.close()
    }
    info(s"Writting file ${fileName}")
  }

  private def extractFileNameFromUrl(url:URL) : String = {
    val strUrl = url.toString
    strUrl.substring( strUrl.lastIndexOf('/')+1, strUrl.length() )
  }

}

object Tumblerone {

  def main(args:Array[String]) {
    val blogName = "nba"
    val listOfUrl = generateTumblrBlogPages(blogName).take(100).toList.par.flatMap { downloadHtml(_) }.flatMap(extractImageUrl(_))
      val resultDir = Paths.get(".").resolve(blogName)
      val downloader = new FileDownloader(resultDir) with ConsoleLogger
      listOfUrl.foreach(downloader.download(_))
  }

}
