package mailer

import com.typesafe.scalalogging.LazyLogging
import javax.mail.event.{ConnectionEvent, ConnectionListener}

class DefaultConnectionListener extends ConnectionListener with LazyLogging {
  override def opened(e: ConnectionEvent): Unit = {
    logger.debug(s"Opened: {}", e.getType)
  }

  override def disconnected(e: ConnectionEvent): Unit = {
    logger.debug(s"disconnected: {}", e.getType)
  }

  override def closed(e: ConnectionEvent): Unit = {
    logger.debug(s"closed: {}", e.getType)
  }
}
