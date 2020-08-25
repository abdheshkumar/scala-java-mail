import javax.mail.internet.MimeMultipart

import scala.concurrent.duration.FiniteDuration

package object mailer {
  private type Root = MimeMultipart
  private type Related = MimeMultipart
  private type Alternative = MimeMultipart

  type Session = javax.mail.Session

  case class MailerSettings(
      protocol: Option[String],
      host: String,
      port: String,
      failTo: String,
      auth: Option[Boolean],
      username: String,
      password: String,
      timeout: Option[FiniteDuration],
      connectionTimeout: Option[FiniteDuration]
  )

}
