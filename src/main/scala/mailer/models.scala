package mailer

import java.util.Date

import javax.activation.DataHandler
import javax.mail.{Message, Session}
import javax.mail.Message.RecipientType
import javax.mail.internet.{
  InternetAddress,
  MimeBodyPart,
  MimeMessage,
  MimeMultipart
}

object models {
  final case class Recipient(
      tpe: RecipientType,
      emailAddress: Option[EmailAddress] = None
  )

  final case class EmailAddress(
      name: String,
      address: String
  )
  final case class Attachment(
      name: String = "",
      data: Array[Byte] = Array.emptyByteArray,
      mimeType: String = "",
      disposition: Disposition = Disposition.Inline
  )
  final case class Email(
      subject: String = "",
      from: Option[EmailAddress] = None,
      text: String = "",
      htmlText: Option[String] = None,
      replyTo: Option[EmailAddress] = None,
      recipients: Seq[Recipient] = Seq.empty,
      attachments: Seq[Attachment] = Seq.empty
  ) {

    private type Root = MimeMultipart
    private type Related = MimeMultipart
    private type Alternative = MimeMultipart

    def to(name: String, address: String): Email =
      copy(recipients =
        recipients :+ Recipient(
          RecipientType.TO,
          Some(EmailAddress(name, address))
        )
      )

    def cc(name: String, address: String): Email =
      copy(recipients =
        recipients :+ Recipient(
          RecipientType.CC,
          Some(EmailAddress(name, address))
        )
      )

    def bcc(name: String, address: String): Email =
      copy(recipients =
        recipients :+ Recipient(
          RecipientType.BCC,
          Some(EmailAddress(name, address))
        )
      )

    def replyTo(name: String, address: String): Email =
      copy(replyTo = Some(EmailAddress(name, address)))

    def sendAttachments(__vs: Attachment*): Email =
      copy(attachments = attachments ++ __vs)

    def createMessage(session: Session): Message = {

      val (root: Root, related: Related, alternative: Alternative) =
        messageStructure

      val message = createMimeMessage(session, root)
      addRecipients(message)
      addTextPart(alternative)
      htmlText.foreach(addHtmlPart(alternative))
      addAttachments(root, related)

      message.saveChanges()

      message
    }

    private def messageStructure: (Root, Related, Alternative) = {
      val root: Root = new MimeMultipart("mixed")
      val relatedPart = new MimeBodyPart
      val related: Related = new MimeMultipart("related")

      root.addBodyPart(relatedPart)
      relatedPart.setContent(related)

      val alternativePart = new MimeBodyPart
      val alternative: Alternative = new MimeMultipart("alternative")

      related.addBodyPart(alternativePart)
      alternativePart.setContent(alternative)

      (root, related, alternative)
    }

    implicit private def emailAddressToInternetAddress(
        emailAddress: EmailAddress
    ): InternetAddress =
      new InternetAddress(emailAddress.address, emailAddress.name)

    private def createMimeMessage(
        session: Session,
        root: Root
    ): javax.mail.internet.MimeMessage = {

      val message = new MimeMessage(session)
      message.setSubject(subject, "UTF-8")
      from.foreach(message.setFrom(_))
      replyTo.foreach(replyTo =>
        message.setReplyTo(Array(replyTo: InternetAddress))
      )
      message.setContent(root)
      message.setSentDate(new Date)
      message
    }

    private def addRecipients(message: javax.mail.internet.MimeMessage): Unit =
      recipients.foreach { recipient =>
        recipient.emailAddress.foreach(
          message.addRecipient(
            recipient.tpe,
            _
          )
        )

      }

    private def addTextPart(alternative: Alternative): Unit = {
      val messagePart = new MimeBodyPart
      messagePart.setText(text, "UTF-8")
      alternative.addBodyPart(messagePart)
    }

    private def addHtmlPart(
        alternative: Alternative
    )(htmlText: String): Unit = {
      val messagePartHtml = new MimeBodyPart
      messagePartHtml.setContent(htmlText, "text/html; charset=UTF-8")
      alternative.addBodyPart(messagePartHtml)
    }

    private def addAttachments(root: Root, related: Related): Unit =
      attachments.foreach { a =>
        a.disposition match {
          case Disposition.Inline =>
            related.addBodyPart(attachmentToMimeBodyPart(a))
          case Disposition.Attachment =>
            root.addBodyPart(attachmentToMimeBodyPart(a))
        }
      }

    private def attachmentToMimeBodyPart(
        attachment: Attachment
    ): MimeBodyPart = {
      val datasource = new javax.mail.util.ByteArrayDataSource(
        attachment.data,
        attachment.mimeType
      )
      datasource.setName(attachment.name)
      val datasourceName = datasource.getName

      val attachmentPart = new MimeBodyPart
      attachmentPart.setDataHandler(new DataHandler(datasource))
      attachmentPart.setFileName(attachment.name)
      attachmentPart.setHeader("Content-Type", datasource.getContentType)
      attachmentPart.setContentID(s"<$datasourceName>")
      attachmentPart.setDisposition(attachment.disposition.name)
      attachmentPart
    }
  }

  sealed abstract class Disposition(val name: String)

  object Disposition {
    case object Inline extends Disposition("Inline")
    case object Attachment extends Disposition("Attachment")
  }
  case class SendEmailException(email: Email, cause: Throwable)
      extends RuntimeException(cause)
}
