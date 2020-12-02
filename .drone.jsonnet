local AbstractPipeline(name) = {
  kind: "pipeline",
  type: "docker",
  name: name
};

local Pipeline(ver) = AbstractPipeline("scala-jsonschema/" + ver) {
  kind: "pipeline",
  type: "docker",
  workspace: { path: ver }
};

local OneStepPipeline(ver, step) = Pipeline(ver) + {
  kind: "pipeline",
  type: "docker",
  name: "scala-jsonschema/" + ver,
  steps: [ step ]
};

local BuildStep(ver) = {
  name: "build: " + ver,
  image: "andyglow/sbt:latest",
  when: { "branch": "master" },
  environment: {
    SCALA_VER: ver,
    CODECOV_TOKEN: { from_secret: "codecov_token" },
    DRONE_WORKSPACE_PATH: "/drone/src/" + ver // used in ParseJsonBulkSpec
  }
};

local SbtCleanTest(ver) = BuildStep(ver) + {
  commands: [
    "sbt clean test"
  ]
};

local Coverage(name, ver) = BuildStep(ver) + {
  name: name,
  commands: [
     "sbt clean coverage test",
     "sbt coverageAggregate",
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
  AbstractPipeline("finalize") + {
    steps: [
      Coverage("scoverage", "2.13") + { when: { status: [ "success" ] } },
      Notify("slack") + { depends_on: [ "scoverage" ] }
    ],
    depends_on: [
      "scala-jsonschema/2.13",
      "scala-jsonschema/2.12",
      "scala-jsonschema/2.11"
    ],
    // makes pipeline trigger unconditionally
    trigger: [
      { status: [ "success", "failure" ] }
    ]
  },
]