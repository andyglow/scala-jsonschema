local Pipeline(name) = {
  kind: "pipeline",
  type: "docker",
  name: "scala-jsonschema/" + name,
  workspace: {
    path: name
  }
};

local OneStepPipeline(name, step) = Pipeline(name) + {
  kind: "pipeline",
  type: "docker",
  name: "scala-jsonschema/" + name,
  steps: [ step ]
};

local BuildStep(name) = {
  name: name,
  image: "andyglow/sbt:latest",
  when: { "branch": "master" },
  environment: {
    CODECOV_TOKEN: { from_secret: "codecov_token" }
  }
};

local SbtCleanTest(ver) = BuildStep("build_" + ver) + {
  commands: [
    "CI=Drone SCALA_VER=" + ver + " sbt clean test"
  ]
};

local Coverage(name) = BuildStep(name) + {
  commands: [
     "CI=Drone SCALA_VER=2.13 sbt clean coverage test",
     "CI=Drone SCALA_VER=2.13 sbt coverageAggregate",
     "wget -O .codecov https://codecov.io/bash",
     "chmod +x .codecov",
     "./.codecov -X gcov -X coveragepy -X xcode -X gcovout"
  ]
};

local Notify(name) = {
  name: name,
  image: "plugins/slack",
  when: { status: [ "success", "failure" ] },
  settings: {
    webhook: { from_secret: "slack_webhook_url" },
    channel: "builds",
    username: "drone",
    link_names: true,
    template: |||
    {{#success build.status}}
      {{repo.name}}: build {{build.number}} succeeded (spent {{since build.started}}). Good job. {{build.link}}
    {{else}}
      {{repo.name}}: build {{build.number}} failed. Fix please. {{build.link}}
    {{/success}}
|||
  }
};

[
  OneStepPipeline("2.13", SbtCleanTest("2.13")),
  OneStepPipeline("2.12", SbtCleanTest("2.12")),
  OneStepPipeline("2.11", SbtCleanTest("2.11")),
  Pipeline("finalize") + {
    steps: [
      Coverage("scoverage"),
      Notify("slack") + { depends_on: [ "scoverage" ] }
    ],
    depends_on: [
      "scala-jsonschema/2.13",
      "scala-jsonschema/2.12",
      "scala-jsonschema/2.11"
    ],
  },
]