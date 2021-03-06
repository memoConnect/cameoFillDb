package controllers

import play.api.mvc._
import play.api.libs.json.{JsString, JsObject, Json}
import play.api.libs.ws.WS
import scala.concurrent.{Await, ExecutionContext, Future}
import play.api.Logger
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.io.FileWriter


case class User(token: String, identity: String, login: String)

object Application extends Controller {

  val password = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8"

//  val password = "password"
  // sha256 of "password"
    val url = "https://dev.cameo.io/api/v1"
//  val url = "http://localhost:9000/api/v1"
  val messagesPerConversation = 1000
  val maxMessageSize = 500
  val numberOfRepetitions = 200
  val numberOfContacts = 100

  def index = Action {
//    addUser()
        Future(multipleBatches(numberOfRepetitions))
    Ok("running - see console for details")
  }

  def multipleBatches(repetitions: Int) = {

    Seq.range(0, repetitions).seq.map {
      n => {
        Logger.debug("Starting repetition number: " + n + " Total: " + numberOfRepetitions)
        Await.result(oneBatch(n), 10 minutes)
      }

    }

  }

  def oneBatch(repetition: Int): Future[Boolean] = {

    val start = System.currentTimeMillis()

    // create Users
    Logger.info("STARTING: Creating Users ")
    val mainUser = addUser()
    val otherUsers = Seq.fill(4) {
      addUser()
    }

    mainUser.flatMap {
      user => {
        // add Users to address book
        Logger.info("STARTING: Adding Contacts")
        val conRes: Seq[Future[Boolean]] = otherUsers.zipWithIndex.map {
          case (fu, i) => fu.flatMap {
            u =>
              val groups = i match {
                case 0 => Seq("group1")
                case 1 => Seq("group1", "group2")
                case 2 => Seq("group2")
                case 3 => Seq()
              }
              addContact(user.token, Some(u.identity), groups, None, None)
          }
        }
        Future.sequence(conRes).map {
          s => if (s.forall(b => b)) {
            Logger.info("FINISHED: Adding Contacts ")
          }
        }

        // add 100 random contacts
        Logger.info("STARTING: Adding random Contacts ")
        val ranConRes: Future[Boolean] = Future(Seq.range(0, numberOfContacts).seq.map {
          case n if n < 20 => Await.result(addContact(user.token, None, Seq("group1"), Some("12341234"), Some("asdfasdfsaf")), 1 minute)
          case n if n < 40 => Await.result(addContact(user.token, None, Seq("group2"), Some("12341234"), Some("asdfasdfsaf")), 1 minute)
          case n => Await.result(addContact(user.token, None, Seq(), Some("12341234"), Some("asdfasdfsaf")), 1 minute)
        }.forall(b => b))
        //        Future.sequence(ranConRes).map {
        //          s => if (s.forall(b => b)) {
        //            Logger.info("FINISHED: Adding random Contacts ")
        //          }
        //        }
        ranConRes.map {
          s => if (s) {
            Logger.info("FINISHED: Adding random Contacts ")
          } else {
            Logger.error("ERROR")
          }
        }

        // create conversations
        Logger.info("STARTING: Creating conversations with " + messagesPerConversation + " messages each")
        def cFinish(n: Int) = Logger.info("FINISHED: Creating conversation:" + n)
        Future.sequence(otherUsers).flatMap {
          others => {
            val seq = Seq.range(0, 10).map {
              case n if n <= 2 => createConversation(Seq(user, others(1)), messagesPerConversation, n).map(b => if (b) {
                cFinish(n)
              })
              case n if n == 3 => createConversation(Seq(user, others(0), others(1)), messagesPerConversation, n).map(b => if (b) {
                cFinish(n)
              })
              case n if n == 4 => createConversation(Seq(user, others(2), others(3)), messagesPerConversation, n).map(b => if (b) {
                cFinish(n)
              })
              case n if n == 5 => createConversation(Seq(user, others(0), others(2)), messagesPerConversation, n).map(b => if (b) {
                cFinish(n)
              })
              case n if n <= 7 => createConversation(Seq(user, others(3)), messagesPerConversation, n).map(b => if (b) {
                cFinish(n)
              })
              case n => createConversation(Seq(user, others(0), others(1), others(2), others(3)), messagesPerConversation, n).map(b => if (b) {
                cFinish(n)
              })
            }
            Future.sequence(seq)
          }
        }.map(r => {
          val total = (System.currentTimeMillis() - start) / 1000
          val secs = total % 60
          val minutes = total / 60
          val msg = "Total time: " + minutes + "minutes " + secs + " seconds\n" + "Main User: " + user.login + " token: " + user.token + "\n"
          Logger.info(msg)

          val fw = new FileWriter("db_fill_users.txt", true)
          try {
            fw.write(user.login + ";password;" + user.identity + ";" + user.token + "\n")
          }
          finally fw.close()

          val fw2 = new FileWriter("db_fill_times.txt", true)
          try {
            fw2.write(repetition + ";" + total + "\n")
          }
          finally fw2.close()

          true
        })
      }
    }

  }

  def addUser(): Future[User] = {

    val login = randomString(8)

    // get reservation secret
    postRequest("/account/check", Json.obj("loginName" -> login), "").flatMap {
      case None => Future(new User("fail", "fail", "fail"))
      case Some(res) => {
        val reservationSecret = (res \ "data" \ "reservationSecret").as[String]
        val user = Json.obj(
          "loginName" -> login,
          "password" -> password,
          "phoneNumber" -> "+491744560277",
          "email" -> "dbfill@bjrm.de",
          "reservationSecret" -> reservationSecret
        )

        postRequest("/account", user, "").flatMap {
          case None => Future(new User("fail", "fail", login))
          case Some(res) => {
            val identityJs = (res \ "data" \ "identities")(0).as[JsObject]
            val identity = (identityJs \ "id").as[String]

            // get token
            val auth = new sun.misc.BASE64Encoder().encode((login + ":" + password).getBytes).replace("\n","")
              val authPath = url + "/token"
            WS.url(authPath).withHeaders(("Authorization", auth)).get().map {
              res => {
                if (res.status != 200) {
                  Logger.debug("error:" + res.body)
                  new User("fail", "fail", "fail")
                } else {
                  val token = (Json.parse(res.body) \ "data" \ "token").as[String]
                  new User(token, identity, login)
                }
              }
            }
          }
        }
      }
    }
  }

  def addContact(token: String, id: Option[String], groups: Seq[String], tel: Option[String], email: Option[String]): Future[Boolean] = {
    val identity: JsObject = id match {
      case Some(id2) => Json.obj("identityId" -> id2)
      case None => Json.obj(
        "identity" -> Json.obj(
          "displayName" -> ("generic Display Name " + randomString(3)),
          "email" -> JsString(email.getOrElse("mailmailmailmail")),
          "phoneNumber" -> JsString(tel.getOrElse("23456712341234"))
        ))
    }
    val groupsJs = Json.obj("groups" -> groups)

    postRequest("/contact", identity ++ groupsJs, token).map(_.isDefined)
  }

  def createConversation(users: Seq[User], messageNum: Int, num: Int): Future[Boolean] = {

    val con = Json.obj("subject" -> ("some 1337 subject " + randomString(5)))

    // create and add recipients
    val c: Future[Option[String]] = users.headOption match {
      case None => Future(None)
      case Some(u: User) =>
        postRequest("/conversation", con, u.token).map {
          case None => None
          case Some(js) =>
            val cid = (js \ "data" \ "id").as[String]
            // add others as recipients to conversation
            val re = Json.obj("recipients" -> users.tail.map{_.identity})
            postRequest("/conversation/" + cid + "/recipient", re, u.token)
            Some(cid)
        }
    }

    val tokens = users.map{_.token}
    val tokens2: Seq[String] = tokens

    // add messages to conversation sequentially
    c.map {
      case None => Logger.error("ConversationID expected"); false
      case Some(cid) => {
        // how many messages per user?
        val n = messageNum / tokens2.size
        val seq: Seq[Boolean] = Seq.range(0, n).seq.flatMap {
          i =>
            if (i % 50 == 0) {
              Logger.info(i * tokens2.size + " Messages in Conversation " + num)
            }

            tokens2.map {
              t =>
                val message = Json.obj("messageBody" -> (i + " :" + randomMessageBody))
                Await.result(postRequest("/conversation/" + cid + "/message", message, t), 1 minutes).isDefined
            }
        }
        // converge all results to a single boolean
        seq.forall(b => b)
      }
    }
  }

  def postRequest(path: String, js: JsObject, token: String): Future[Option[JsObject]] = {
    WS.url(url + path).withQueryString(("token", token)).post(js).map {
      response =>
        if (response.status == 200) {
          Some(Json.parse(response.body).as[JsObject])
        } else {
          Logger.error("Request failed. Path: " + path + " Response: [" + response.status + "] " + response.body)
          None
        }
    }
  }

  val random = new scala.util.Random

  def randomString(n: Int): String = {
    def alphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  }

  def randomStringWithSpace(n: Int): String = {
    def alphabet: String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789   "
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString
  }

  def randomMessageBody = {
    randomStringWithSpace(random.nextInt(maxMessageSize))
  }

  //  Future.sequence(otherUsers).map {
  //    others => {
  //      Await.result(createConversation(Seq(user.token, others(1).token), messagesPerConversation), 5 minutes)
  //      cFinish(1)
  //      Await.result(createConversation(Seq(user.token, others(1).token), messagesPerConversation), 5 minutes)
  //      cFinish(2)
  //      Await.result(createConversation(Seq(user.token, others(1).token), messagesPerConversation), 5 minutes)
  //      cFinish(3)
  //      Await.result(createConversation(Seq(user.token, others(0).token, others(1).token), messagesPerConversation), 5 minutes)
  //      cFinish(4)
  //      Await.result(createConversation(Seq(user.token, others(2).token, others(3).token), messagesPerConversation), 5 minutes)
  //      cFinish(5)
  //      Await.result(createConversation(Seq(user.token, others(0).token, others(2).token), messagesPerConversation), 5 minutes)
  //      cFinish(6)
  //      Await.result(createConversation(Seq(user.token, others(3).token), messagesPerConversation), 5 minutes)
  //      cFinish(7)
  //      Await.result(createConversation(Seq(user.token, others(1).token), messagesPerConversation), 5 minutes)
  //      cFinish(8)
  //      Await.result(createConversation(Seq(user.token, others(0).token, others(1).token, others(2).token, others(3).token), messagesPerConversation), 5 minutes)
  //      cFinish(1)
  //      Await.result(createConversation(Seq(user.token, others(0).token, others(1).token, others(2).token, others(3).token), messagesPerConversation), 5 minutes)
  //      cFinish(1)
  //    }
  //  }

}