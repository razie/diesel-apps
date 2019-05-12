package razie.diesel.utils

object DomUtils {

  val SAMPLE_STORY=
    """
      |Stories are told in terms of input messages and expected messages...
      |
      |On one beautiful summer eve, a guest arrived:
      |
      |$send home.guest_arrived(name="Jane")
      |
      |Naturally, the lights must have come on:
      |
      |$expect lights.on
      |$expect (light is "bright")
      |
      |Let's add a chimes system, in charge with greeting Jane:
      |
      |$expect (greeting is "Greetings, Jane")
      |$expect chimes.welcome(name is "Jane")
      |""".stripMargin
  val SAMPLE_SPEC =
    """
      |## Specifications
      |
      |Specifications, like [[Spec:lights-spec]] deal with the actual *implementation* of the system.
      |
      |Our system will turn the lights on when a guest arrives:
      |
      |$when home.guest_arrived(name) => lights.on
      |$when home.guest_arrived(name=="Jane") => chimes.welcome(name = "Jane")
      |
      |Then we have a sensor which we check to see if they're truly on:
      |
      |$when lights.on => lights.check
      |
      |We can also *mock* the messages that we don't have access to yet:
      |
      |$mock lights.check => (light = "bright")
      |$mock chimes.welcome => (greeting = "Greetings, "+name)
      |
      |As you can see, specifications are simply wiki topics, with special annotations for messages, conditions, mocks and such.
      |
      |""".stripMargin

}
