function localStore() {
    try {
        var s = window.localStorage;
        s.setItem("__foo", "bar");
        s.removeItem("__foo");
        return s;
    } catch(e) { }
    return undefined;
}

document.addEventListener("DOMContentLoaded", function() {
    const ds = document.querySelector("#dependency-selector");
    if (ds) {
        const store = localStore();

        const group          = "com.github.andyglow";
        const artifactPrefix = "scala-jsonschema";
        const version        = ds.getAttribute("x-version");
        const scalaVersion   = ds.getAttribute("x-scala-version");

        const sbtPre = document.querySelector(".mdc-tab-content-container > .mdc-tab-content:nth-child(1) > pre");
        const sbtCode = document.querySelector("code.language-scala");

        const mvnPre = document.querySelector(".mdc-tab-content-container > .mdc-tab-content:nth-child(2) > pre");
        const mvnCode = document.querySelector(".language-xml");

        const grdPre = document.querySelector(".mdc-tab-content-container > .mdc-tab-content:nth-child(3) > pre");
        const grdCode = document.querySelector(".language-gradle");

        function $el(tag, attrs = {}, parent) {
            const el = document.createElement(tag);
            for (const [key, value] of Object.entries(attrs)) {
                el.setAttribute(key, value);
            }

            if (parent != null) parent.appendChild(el);

            return el;
        }

        function onDependenciesUpdated(selectedDependencies) {
            var deps = [""]; // "" is for core/api/macros
            for (const [suffix, included] of Object.entries(selectedDependencies)) {
                deps.push(`-${suffix}`);
            }
            deps.sort();
            var sbt = "libraryDependencies ++= Seq(";
            var mvn = `
<properties>
  <scala.binary.version>${scalaVersion}</scala.binary.version>
</properties>
`;
            var grd = `
versions += [
  ScalaBinary: "${scalaVersion}"
]
dependencies {`;
            for (const dep of deps) {
                sbt = `${sbt}\n  "${group}" %% "${artifactPrefix}${dep}" % ${version},`;
                mvn = `${mvn}
<dependency>
  <groupId>${group}</groupId>
  <artifactId>${artifactPrefix}${dep}_\${scala.binary.version}</artifactId>
  <version>${version}</version>
</dependency>
`;
                grd = `${grd}\n  compile group: 'com.github.andyglow', name: "scala-jsonschema${dep}_\${versions.ScalaBinary}", version: '${version}'`;
            }
            sbt += '\n)';
            grd += '\n}';

            sbtCode.innerText = sbt;
            sbtPre.setAttribute("class", "prettyprint");

            mvnCode.innerText = mvn;
            mvnPre.setAttribute("class", "prettyprint");

            grdCode.innerText = grd;
            grdPre.setAttribute("class", "prettyprint");

            window.prettyPrint && prettyPrint();

            if (store) store.setItem("dependency-selection", JSON.stringify(selectedDependencies));
        }
        var initDeps = {};
        if (store) {
            const selectionJson = store.getItem("dependency-selection");
            if (selectionJson) {
                try { initDeps = JSON.parse(selectionJson); }
                catch(e) {
                    store.removeItem("dependency-selection");
                }
            }
        }
        const selectedDependencies = new Proxy(initDeps, {
            set: function (target, key, value) {
                if (value) target[key] = value; else delete target[key];
                onDependenciesUpdated(target);
            }
        });

        function li(dep) {
            const formField = $el("div", {class: "mdc-form-field"});
            const cbDiv = $el("div", {class: "mdc-checkbox"});
            const attrs = {type: "checkbox", class: "mdc-checkbox__native-control", id: `cb-${dep.suffix}`};
            if (dep.hasOwnProperty("selected") && dep.selected) attrs.checked = true;
            if (selectedDependencies[dep.suffix]) attrs.checked = true;
            if (dep.hasOwnProperty("mandatory") && dep.mandatory) attrs.disabled = "disabled";
            const cbInput = $el("input", attrs);
            const bgDiv = $el("div", {class: "mdc-checkbox__background"});
            bgDiv.innerHtml = `
    <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
      <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
    </svg>
    <div class="mdc-checkbox__mixedmark"></div>        
        `;
            const rippleDiv = $el("div", {class: "mdc-checkbox__ripple"});

            const label = $el("label", {for: `cb-${dep.suffix}`})
            label.innerText = dep.name;

            cbDiv.appendChild(cbInput);
            cbDiv.appendChild(bgDiv);
            cbDiv.appendChild(rippleDiv);

            formField.appendChild(cbDiv);
            formField.appendChild(label);

            cbInput.addEventListener("change", function (event) {
                selectedDependencies[dep.suffix] = event.target.checked;
            });

            return formField;
        }

        const dependencies = [
            {name: "Core", children: [
                {name: "Core", suffix: "", selected: true, mandatory: true} ]},
            {name: "Integrations", children: [
                {name: "Play-Json", suffix: "play-json"},
                {name: "Spray-Json", suffix: "spray-json"},
                {name: "Circe", suffix: "circe"},
                {name: "Json4s", suffix: "json4s"},
                {name: "uJson", suffix: "ujson"} ]},
            {name: "Extensions", children: [
                {name: "Joda-time", suffix: "joda-time"},
                {name: "Enumeratum", suffix: "enumeratum"},
                {name: "Cats", suffix: "cats"},
                {name: "Refined", suffix: "refined"} ]}
        ];
        const outer = document.getElementById("dependency-selector");

        for (const category of dependencies) {
            const column = $el("div", { class: "dependency-category" }, outer);
            const header = $el("h3", {}, column); header.innerText = category.name;
            const selection = $el("div", {}, column);
            for (const dep of category.children) {
                selection.appendChild(li(dep));
            }
        }

        onDependenciesUpdated(selectedDependencies);
    }
});