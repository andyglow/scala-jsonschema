package com.github.andyglow.jsonschema.harness

import org.scalatest.Tag

/** Tags for tests that describe the decided, not the current, behavior.
  *
  * A `TriageB` test asserts the FIXED behavior of a TRIAGE group B bug (see the team's TRIAGE.md) and wraps its body in `pendingUntilFixed { ... }`, so it is reported as pending today and starts
  * failing ("should no longer be pending") as soon as the fix lands. Whoever fixes the bug removes the `pendingUntilFixed` wrapper and keeps the test.
  *
  * Run just these with `testOnly -- -n TRIAGE-B`.
  */
object TriageB extends Tag("TRIAGE-B")

/** Group C: behavior the owner still has to decide on. The tests pin today's output. */
object TriageC extends Tag("TRIAGE-C")
