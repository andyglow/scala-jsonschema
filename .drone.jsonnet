local AbstractPipeline(name) = {
  kind: "pipeline",
  type: "docker",
  name: name
};

local Workspace(name) = {
  workspace: { path: name }
};

local WsPipeline(ver) = AbstractPipeline("scala-jsonschema/" + ver) + Workspace(ver);

local Pipeline(ver, build, notify) = WsPipeline(ver) + {
  kind: "pipeline",
  type: "docker",
  name: "scala-jsonschema/" + ver,
  steps: [ build, notify ]
};

local BuildStep(ver) = {
  name: "build",
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

local NotifyMessage = |||
    {{#success build.status}}
      {{repo.name}}: build {{build.number}} for ver %(ver)s succeeded (spent {{since build.started}}). Good job. {{build.link}}
    {{else}}
      {{repo.name}}: build {{build.number}} for ver %(ver)s failed. Fix please. {{build.link}}
    {{/success}}
|||;

local Notify(name, ver) = {
  name: name,
  image: "plugins/slack",
  when: { status: [ "success", "failure" ] },
  settings: {
    webhook: { from_secret: "slack_webhook_url" },
    channel: "builds",
    username: "drone",
    link_names: true,
    template: NotifyMessage % { ver: ver }
  }
};

[
  Pipeline("2.13", SbtCleanTest("2.13"), Notify("slack", "2.13")),
  Pipeline("2.12", SbtCleanTest("2.12"), Notify("slack", "2.12")),
  Pipeline("2.11", SbtCleanTest("2.11"), Notify("slack", "2.11")),
  AbstractPipeline("finalize") + Workspace("2.13") + {
    steps: [
      Coverage("scoverage", "2.13")
    ],
    depends_on: [
      "scala-jsonschema/2.13",
      "scala-jsonschema/2.12",
      "scala-jsonschema/2.11"
    ]
  },
]