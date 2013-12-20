package controllers
import play.api._
import utils._
import play.api.mvc._
import play.libs.Akka
import scala.concurrent.{Future, future, ExecutionContext}
import ExecutionContext.Implicits.global
import scala.io.Source
import java.io._
import java.net.URL
import java.io.FileOutputStream
import java.net.HttpURLConnection
import play.api.libs.iteratee.Enumerator
import play.api.libs.concurrent.Promise
import java.util.concurrent.TimeUnit
import akka._
import akka.actor._
import akka.pattern._
import akka.util.Timeout
import scala.util._
import java.io.InputStreamReader
import org.apache.http.client.HttpClient
import org.apache.http.impl.client.HttpClients
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.client.methods.HttpGet
import java.net.URI
import play.api.libs.json._
object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
//  Akka.system().actorOf(props[])
  
  def getFile(path: String) = Action{ r =>
    Ok.sendFile(
      content = new File(path), 
      fileName = thatFile => {
        val path = thatFile.getAbsolutePath()
        path.drop(path.lastIndexOf('/'))
      }
    )
  }
  
  def plzDex(url: String) = Action.async{ r =>
    implicit def timeout = Timeout.intToTimeout(60000)
    (actor ? url).map{
      case DexRes(files) => 
        val filesJson = files.map{ _.toString |> JsString.apply } |> JsArray.apply
        JsObject(Seq("files" -> filesJson)) |> Ok.apply[JsValue]
      case that => BadRequest(that.toString)
    }
  }
 
  val actor = Akka.system().actorOf(Props[MyAct])
}

case class MyAct() extends Actor {
  val httpClient = HttpClients.createDefault()
  def tmpFile = File.createTempFile("musteri", "")
  def receive = {
    case url: String =>
      Try{
        val leFile = tmpFile
        val get = new HttpGet(url)
        get.setHeader("User-Agent", "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36")
        val resp = httpClient.execute(get)
        val inputStream = resp.getEntity().getContent()
//        val connection = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
//        val stream = connection.getInputStream
        copyStream(inputStream, new FileOutputStream(leFile))
        val rawjars = utils.ZipSplitter.helper(leFile)
        utils.ZipSplitter.doDex(rawjars)
      } match {
        case Success(that) => sender ! that
        case Failure(that) => sender ! that
      }
  }

  type RC = { def read(): Int; def close(): Unit }
  type WC = { def write(i: Int): Unit; def close(): Unit }
  def copyStream(i: RC, o: WC) = {
    var read = 0
    def condition = {
      read = i.read
      read >= 0
    }
    while(condition) o.write(read)
    i.close
    o.close
  }
}