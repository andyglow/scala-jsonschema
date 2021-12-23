package json.schema

sealed trait Flag
object Flag {

  trait EnumsAsOneOf extends Flag
}
