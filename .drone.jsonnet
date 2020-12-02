//local BuildTpl(name, build) = {
//  kind: "pipeline",
//  type: "docker",
//  name: "scala-jsonschema:" + name,
//  steps: [
//    build
//  ]
//};

local BuildStepTpl(name) = {
  name: name,
  image: "andyglow/sbt:latest",
  when: { "branch": "master" },
  environment: {
    CODECOV_TOKEN: { from_secret: "codecov_token" }
  }
};

local BuildStep(ver) = BuildStepTpl("build_" + ver) + {
  commands: [
    "CI=Drone SCALA_VER=" + ver + " sbt clean test"
  ]
};
local CoverageStep(name) = BuildStepTpl(name) + {
  commands: [
     "CI=Drone SCALA_VER=2.13 sbt clean coverage test",
     "CI=Drone SCALA_VER=2.13 sbt coverageAggregate",
     "wget -O .codecov https://codecov.io/bash",
     "chmod +x .codecov",
     "./.codecov -X gcov -X coveragepy -X xcode -X gcovout"
  ]
};

local NotfyStep(name) = {
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

//[
//  BuildTpl("2.13", BuildStep("2.13")),
//  BuildTpl("2.12", BuildStep("2.12")),
//  BuildTpl("2.11", BuildStep("2.11")),
//  BuildTpl("coverage", CoverageStep) + { depends_on: [ "build_2.13" ] },
//]

[
{
  kind: "pipeline",
  type: "docker",
  name: "scala-jsonschema",
  steps: [
    BuildStep("2.13"),
    BuildStep("2.12"),
    BuildStep("2.11"),
    CoverageStep("coverage") + {
      depends_on: [ "build_2.13", "build_2.12", "build_2.11" ]
    },
    NotfyStep("notify") + {
      depends_on: [ "coverage" ]
    }
  ]
}
]