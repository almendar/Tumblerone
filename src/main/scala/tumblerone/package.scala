package pl.tk

import java.net.URL
import scala.util.{Failure, Try, Success}

/**
 * Created by tomaszk on 17.02.14.
 */
package object tumblerone {

   def downloadHtml(url:URL) : Option[String] = {
     val cl = new ConsoleLogger {}
     Try(scala.io.Source.fromURL(url).getLines().mkString("\n")) match {
       case Success(value) =>
         cl.info(s"HTML content download successfull of $url")
         Some(value)
       case Failure(exception) =>
         cl.error(exception.getMessage)
         None
     }
  }

  def extractImageUrl(content:String) : List[URL] = {
    val reg = """(src=")(http://(24|25).+?\.(jpg|png|gif))(")""".r
    val f = reg.findAllMatchIn(content).toList.map(_.group(2))
    f.flatMap {strUrl =>
       Try(new URL(strUrl)) match {
         case Success(value) => Some(value)
         case Failure(exception) => None
       }
    }
  }


  /**
   * Will generate links
   * @param blogName
   * @return
   */
  def generateTumblrBlogPages(blogName : String) : Iterator[URL] = {
    new Iterator[URL] {
      private var index = 0;
      override def next(): URL = {index+=1 ; new URL(s"http://${blogName}.tumblr.com" + s"""/page/${index}""")}
      override def hasNext: Boolean = true
    }
  }

}
