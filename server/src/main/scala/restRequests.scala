import cats.effect._
import cats.implicits._
import cats.instances._
import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s.client.blaze._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object restRequests extends App {

  case class Response(id: Int, userId: Option[Int] = None, title: Option[String] = None, body: Option[String] = None, error: Option[String] = None)

  implicit val userDecoder = jsonOf[IO, Response]
  implicit val list = jsonOf[IO, List[Response]]

  val httpClient = Http1Client[IO]().unsafeRunSync()



  def elapsedTimeMillis(start: Long): Long = {
    val nanos = System.nanoTime() - start nanos;
    nanos.toMillis
  }

  def requestSingleTimed(id: Int): IO[(Response, Long)] = {
    val start = System.nanoTime()
    httpClient.expect[Response](s"https://jsonplaceholder.typicode.com/posts/$id")
      .map { response => (response, elapsedTimeMillis(start))}
      .timeout(400 millis)
      .recover {
        case _: Throwable => (Response(-1), elapsedTimeMillis(start))
      }
  }

  val result = for {
    postListResponse <- httpClient.expect[List[Response]]("https://jsonplaceholder.typicode.com/posts")
    postList = postListResponse.map(_.id)
    postListIndividual <- postList.parTraverse(id => requestSingleTimed(id))
  } yield (postListResponse.map(_.id), postListIndividual.map {
    case (response, time) => (response.id, time)
  })

  (1 to 100).foreach { _ =>
    val start = System.nanoTime()
    val (combined, individual) = result.unsafeRunSync()
    println(combined)
    println(individual)
    println(s"Total time: ${elapsedTimeMillis(start)}")
    Thread.sleep(500)
  }



  //  val ids = postList.map(a => a.id)
  //
  //  println(ids.unsafeRunSync())
}
