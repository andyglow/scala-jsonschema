// #setup
sealed trait Event

object Event {

  final case class NewUser(firstName: String, lastName: String, login: String, password: String) extends Event

  final case class UpdateUserProfile(id: String, firstName: Option[String], lastName: Option[String]) extends Event
}

case class EventEnvelope(
  id: String,
  headers: Map[String, String],
  body: Event)
// #setup

// #result
import json._
import com.github.andyglow.jsonschema._

val schema: json.Schema[EventEnvelope] = Json.schema[EventEnvelope]

eventEnvelopeSchema.draft04
// #result
