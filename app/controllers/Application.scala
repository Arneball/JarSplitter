package controllers

import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import akka.actor.Actor
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import play.api.libs.concurrent.Promise
import play.api.libs.iteratee.Enumerator
import play.api.mvc.Action
import play.api.mvc.Controller
import play.libs.Akka
import utils.DexRes
import play.api.libs.iteratee.Iteratee
import play.api.libs.Comet
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString
import play.api.mvc.WebSocket
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import java.util.concurrent.TimeUnit._
import play.api.libs.ws.WS
import play.api.libs.json.JsArray
object Application extends Controller {
  import Utils._
  def index = Action {
    Ok(views.html.index())
  }
  private def futs[T](futs: Future[T]*): Enumerator[T] = {
    var rem = futs
    def take(): Future[Option[T]] = rem match {
      case Seq(head, tail @ _*) =>
        rem = tail
        head.map{ Option.apply }
      case _ => future{ None }
    }
    Enumerator generateM take
  }
  private def musta[T](futs: (T, Int)*): Enumerator[T] = {
    var rem = futs
    var cnt = 0
    def take(): Future[Option[T]] = rem match {
      case Seq((t, duration), tail @ _*)=>
        cnt += 1
        println(s"Done: $cnt")
        rem = tail
        Promise.timeout(Some(t), duration, MILLISECONDS)
      case _ => future{ None } 
    }
    
    Enumerator generateM take
  }
  private def repeat[T](f: =>T, times: Int): Enumerator[T] = {
    var counter = times
    def mk = if(counter > 0) {
      counter -= 1
      println(s"Counter=$counter")
      Some(f)
    } else None
    Enumerator.generateM{
      Promise.timeout(mk, 50, TimeUnit.MILLISECONDS)
    }
  }
  implicit val timeout = Timeout.intToTimeout(60000)
  def asyncWs = Action{ r =>
    val hello = future{ 
      JsObject(Seq("message" -> JsString("getting stuff"))): JsValue 
    }
    val futJsArray = WS.url("http://www.aftonbladet.se/").get().map{ resp => 
      JsObject(Seq("message"->JsString(resp.cookies.map{ _.toString}.mkString))): JsValue 
    }
    val enum = futs[JsValue](hello, futJsArray) &> Comet[JsValue](callback="parent.crazy")
    Ok.chunked(enum)
  }
  def async = Action { r =>
//    val after = repeat({
//      JsObject(Seq("color" -> JsString("#" + (0 to 2).map{ _ => util.Random.nextInt(255).toHexString}.mkString))): JsValue
//    }, 1000)
    val futures: Stream[(JsValue, Int)] = Stream.tabulate(250){ _ => 
      val obj = JsObject(Seq("color" -> JsString("#" + (0 to 2).map{ _ => util.Random.nextInt(255).toHexString}.mkString))): JsValue
      obj -> util.Random.nextInt(250)
    }
    val after = musta(futures: _*)
    val enum = after &> Comet[JsValue](callback = "parent.pelle")
    Ok.chunked(enum)
  }
  
  def plzDex(url: String) = Action.async { r =>
    def tmpFile() = File.createTempFile("musteri", "")
    def downloadJar(): Future[File] = future {
      val get = new HttpGet(url){
        setHeader("User-Agent", "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1667.0 Safari/537.36")
      }
      val resp = httpClient.execute(get)
      val inputStream = resp.getEntity().getContent()
      val leFile = tmpFile()
      inputStream copyTo new FileOutputStream(leFile)
      leFile
    }
    val tmp = for {
      downloaded <- downloadJar()
      DexRes(zipFile) <- actor ? downloaded
    } yield Ok.sendFile(content = zipFile, fileName = _ => "thatFile.zip", onClose = () => {
      downloaded.delete()
      zipFile.delete()
    })

    tmp.recover {
      case that => BadRequest(that.toString)
    }
  }

  val actor = Akka.system().actorOf(Props[MyAct])
  val httpClient = HttpClients.createDefault()
}

case class MyAct() extends Actor {
  def receive = {
    case leFile: File => Try {
      val rawjars = utils.ZipSplitter.helper(leFile)
      val tmp = utils.ZipSplitter.doDex(rawjars)
      rawjars.foreach { _.delete }
      tmp
    } match {
      case Success(that) => sender ! that
      case Failure(that) =>
        that.printStackTrace()
        sender ! that
    }
  }
}

object Utils {
  implicit class InputStreamW(val i: InputStream) extends AnyVal {
    def copyTo(o: OutputStream) = {
      var read = 0
      def condition = {
        read = i.read
        read >= 0
      }
      while (condition) o.write(read)
      i.close
      o.close
    }
  }
  //  type RC = { def read(): Int; def close(): Unit }
  //  type WC = { def write(i: Int): Unit; def close(): Unit }
}