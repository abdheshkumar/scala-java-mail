package mailer

import java.util.Properties
import java.util.concurrent.TimeUnit

import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import com.typesafe.config.Config
import javax.mail.{Authenticator, PasswordAuthentication, Transport}
import mailer.models._
import pureconfig.error.ConfigReaderFailures

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

class Mailer private (session: Session, autoClose: Boolean) {

  private lazy val mailer = new Mailer.Synchronous(session, autoClose)

  /**
    * Send single any type of email
    * @param email: Email
    * @param ec: ExecutionContext
    * @return
    */
  def sendEmail(email: Email)(implicit ec: ExecutionContext): Future[Unit] =
    Future(mailer.sendEmail(email)).flatMap {
      case Failure(t) => Future.failed(t)
      case Success(u) => Future.successful(u)
    }

  /**
    * Send multiple emails
    * @param emails
    * @param ec
    * @return
    */
  def sendEmails(
      emails: List[Email]
  )(implicit ec: ExecutionContext): Future[List[Unit]] =
    Future(mailer.sendEmails(emails)).flatMap {
      case Failure(t) => Future.failed(t)
      case Success(u) => Future.successful(u)
    }
}

object Mailer {
  import pureconfig._

  /**
    * Create settings from the config
    * @param conf
    * @return
    */
  private def mailerSettings(
      conf: Config
  ): Either[ConfigReaderFailures, MailerSettings] = {
    for {
      protocol <-
        ConfigSource.fromConfig(conf).at("transport.protocol").load[String]
      host <- ConfigSource.fromConfig(conf).at("host").load[String]
      port <- ConfigSource.fromConfig(conf).at("port").load[String]
      failTo <- ConfigSource.fromConfig(conf).at("failTo").load[String]
      auth <- ConfigSource.fromConfig(conf).at("auth").load[String]
      username <- ConfigSource.fromConfig(conf).at("username").load[String]
      password <- ConfigSource.fromConfig(conf).at("password").load[String]
      connectionTimeout <-
        ConfigSource.fromConfig(conf).at("connectiontimeout").load[Option[Int]]
      timeout <- ConfigSource.fromConfig(conf).at("timeout").load[Option[Int]]
    } yield MailerSettings(
      protocol = Some(protocol),
      host,
      port,
      failTo,
      Some(auth.toBoolean),
      username,
      password,
      timeout.map(FiniteDuration(_, TimeUnit.SECONDS)),
      connectionTimeout.map(FiniteDuration(_, TimeUnit.SECONDS))
    )
  }

  /**
    * Create mailer instance
    * @param config
    * @param autoClose
    * @return
    */
  def createMailer(
      config: Config,
      autoClose: Boolean = false
  ): Either[ConfigReaderFailures, Mailer] =
    mailerSettings(config).map(settings =>
      new Mailer(createSessionBySetting(settings), autoClose)
    )

  /**
    * Create mailer instance
    * @param settings:MailerSettings
    * @param autoClose
    * @return
    */
  def createMailerBySettings(
      settings: MailerSettings,
      autoClose: Boolean = false
  ): Mailer =
    new Mailer(createSessionBySetting(settings), autoClose)

  def createSessionBySetting(setting: MailerSettings): Session = {
    val protocol = setting.protocol.getOrElse("smtps")
    val auth = setting.auth.getOrElse(true)

    val properties = new Properties()
    properties.put(s"mail.transport.protocol", protocol)
    properties.put(s"mail.$protocol.quitwait", "false")
    properties.put(s"mail.$protocol.host", setting.host)
    properties.put(s"mail.$protocol.port", setting.port)
    properties.put(s"mail.$protocol.from", setting.failTo)
    properties.put(s"mail.$protocol.auth", auth.toString)
    setting.connectionTimeout.foreach(t =>
      properties
        .put(s"mail.$protocol.connectiontimeout", t.toMillis: java.lang.Long)
    )
    setting.timeout.foreach(t =>
      properties.put(s"mail.$protocol.timeout", t.toMillis: java.lang.Long)
    )

    val authenticator =
      if (auth) {
        new Authenticator {
          override def getPasswordAuthentication =
            new PasswordAuthentication(setting.username, setting.password)
        }
      } else null
    javax.mail.Session.getInstance(properties, authenticator)
  }

  /** Send email using
    *
    * @param session: Session
    * @param autoClose: Boolean if it is true then it transport connection will be close
    */
  private class Synchronous(session: Session, autoClose: Boolean) {

    /**
      * Create one transport connection for an application
      */
    private lazy val transport: Try[Transport] = Try {
      val t = session.getTransport
      t.addConnectionListener(new DefaultConnectionListener())
      t
    }

    def sendEmail(email: Email): Try[Unit] =
      tryWithTransport { implicit transport =>
        send(email)
      }(autoClose).flatten

    def sendEmails(emails: List[Email]): Try[List[Unit]] =
      tryWithTransport { implicit transport =>
        emails.map(send).sequence
      }(autoClose).flatten

    /**
      * It will execute function by passing transport connection and also it connect transport if it is not connected
      * @param function
      * @param autoClose: Boolean It will close connection if the value is true
      * @tparam T
      * @return
      */
    private def tryWithTransport[T](
        function: Transport => T
    )(autoClose: Boolean): Try[T] =
      for {
        _transport <- transport
        _ <- Try(if (!_transport.isConnected) _transport.connect())
        possibleResult = Try(function(_transport))
        _ <- Try(if (autoClose) _transport.close())
        result <- possibleResult
      } yield result

    private def send(email: Email)(implicit transport: Transport): Try[Unit] =
      Try {
        val message = email.createMessage(session)
        transport.sendMessage(message, message.getAllRecipients)
      }.recoverWith {
        case cause => Failure(SendEmailException(email, cause))
      }
  }

}
