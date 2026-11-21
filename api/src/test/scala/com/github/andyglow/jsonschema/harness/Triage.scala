package com.github.andyglow.jsonschema.harness

import org.scalatest.Tag

/** Tags for tests that describe the decided, not the current, behavior.
  *
  * A `TriageB` test asserts the FIXED behavior of a TRIAGE group B bug (see the team's TRIAGE.md). While a bug is open, the test wraps its body in `pendingUntilFixed { ... }` (reported as
  * pending, failing with "should no longer be pending" once the fix lands); whoever fixes the bug removes the wrapper and keeps the test. All group B bugs are fixed on Scala 2.
  *
  * Run just these with `testOnly -- -n TRIAGE-B`.
  */
object TriageB extends Tag("TRIAGE-B")

/** Group A: Scala 2 behavior that is gone on Scala 3 by design (TRIAGE.md group A). Tests assert the Scala 3 behavior and pin the Scala 2 one, switching on [[Golden.scalaBinaryVersion]].
  */
object TriageA extends Tag("TRIAGE-A")

/** Not triaged yet: a suspected bug found while writing the characterization tests. The test pins today's behavior; the harness report lists these for the lead/owner.
  */
object Untriaged extends Tag("UNTRIAGED")

/** Group C: behavior the owner still has to decide on. The tests pin today's output. */
object TriageC extends Tag("TRIAGE-C")

object Scala {
  def is3: Boolean = Golden.scalaBinaryVersion == "3"
  def is2: Boolean = !is3
}
