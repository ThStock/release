package release

import release.PomMod.{Dep, PluginDep, PluginExec, PomRef}

object Novosales1Deps {

  def plugins(): Seq[PluginDep] = Seq(
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "org.jacoco", "jacoco-maven-plugin", "", Seq(PluginExec("default-prepare-agent", Seq("prepare-agent"), "", Map()), PluginExec("default-report", Seq("report"), "prepare-package", Map()))),
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "org.apache.maven.plugins", "maven-dependency-plugin", "2.8", Seq(PluginExec("pre-build-validate-tree", Seq("tree"), "validate", Map("outputFile" -> "target/dependency-tree")), PluginExec("pre-build-validate-list", Seq("list"), "validate", Map("outputFile" -> "dep.list", "sort" -> "true")))),
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "com.novomind.maven", "zkm-maven-plugin", "", Seq(PluginExec("", Seq(), "", Map()))),
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "org.apache.maven.plugins", "maven-checkstyle-plugin", "2.17", Seq(PluginExec("validate", Seq("check"), "validate", Map("configLocation" -> "checkstyle.xml", "includeTestSourceDirectory" -> "true", "consoleOutput" -> "true", "encoding" -> "UTF-8", "failsOnError" -> "true")))),
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "org.codehaus.mojo", "aspectj-maven-plugin", "1.8", Seq(PluginExec("", Seq("compile", "test-compile"), "", Map()))),
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "", "maven-surefire-plugin", "", Seq()),
    PluginDep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT:pom"), "", "maven-failsafe-plugin", "2.19.1", Seq(PluginExec("", Seq("integration-test", "verify"), "", Map()))),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "com.novomind.ishop.maven", "ishop-maven-plugin", "", Seq(PluginExec("", Seq("pom-to-class-path", "check-obsolete-artifacts", "check-for-changes-before", "check-for-changes-package"), "", Map()))),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "org.codehaus.mojo", "hibernate3-maven-plugin", "3.0", Seq()),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "", "maven-assembly-plugin", "2.6", Seq(PluginExec("assembly", Seq("single"), "package", Map()))),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "org.apache.maven.plugins", "maven-compiler-plugin", "", Seq()),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "org.apache.maven.plugins", "maven-antrun-plugin", "1.8", Seq(PluginExec("extractWarInfo", Seq("run"), "package", Map("target" -> "")), PluginExec("copyResources", Seq("run"), "prepare-package", Map("target" -> "")))),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "org.codehaus.mojo", "aspectj-maven-plugin", "", Seq()),
    PluginDep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT:war"), "pl.allegro", "grunt-maven-plugin", "1.5.1", Seq(PluginExec("", Seq("npm", "grunt"), "prepare-package", Map())))
  )

  def snapshots(): Seq[Dep] = {
    Seq(
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops", "ishop-shop-parent", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.core", "ishop-core-all", "27.1.2-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.core", "ishop-core-all-tests", "27.1.2-SNAPSHOT", "pom", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops-ext", "ishop-shops-ext-shop-base", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.core", "ishop-core-projects", "27.1.2-SNAPSHOT", "pom", "import", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.core", "ishop-core-projects", "27.1.2-SNAPSHOT", "pom", "import", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-db-migration", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-ipim-reviews", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.backoffice", "bo-services", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.backoffice", "bo-core", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops-ext", "shop-footprint", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.exi", "ishop-ext-productdata-ipim-generic", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.exi", "ishop-ext-objectstore-attributeimporter", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.plugin", "ishop-plugin-stable", "27.3.0-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.plugin", "ishop-plugin-unstable", "27.3.0-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.plugin", "ishop-plugin-demo", "27.3.0-SNAPSHOT", "pom", "", "")
    )
  }

  def selfMod(): Seq[Dep] = Seq(
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "", "", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "", "", "war"),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "", "test", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "", "test", "war"),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "pom", "", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "pom", "", "war"),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "pom", "test", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops", "novosales", "27.0.0-SNAPSHOT", "pom", "test", "war"),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "", "", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "", "test", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "pom", "", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "pom", "test", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "", "", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "", "", "pom"),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "", "test", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "", "test", "pom"),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "pom", "", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "pom", "", "pom"),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "pom", "test", ""),
    Dep(PomRef("X"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "pom", "test", "pom")
  )

  def selfVersion(version: String): Seq[Dep] = {
    Seq(
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:" + version), "com.novomind.ishop.shops.novosales", "novosales-projects", version, "", "", "pom"),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.shops.novosales", "novosales-erp", version, "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops", "novosales", version, "", "", "war"))
  }

  def self(): Seq[Dep] = selfVersion("27.0.0-SNAPSHOT")

  def all(): Seq[Dep] = {
    Seq(
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops", "ishop-shop-parent", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "org.apache.httpcomponents", "httpclient", "4.5.1", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "org.apache.commons", "commons-lang3", "3.4", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "io.reactivex", "rxjava", "1.0.12", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.netflix.hystrix", "hystrix-core", "1.4.12", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.github.dreamhead", "moco-core", "0.10.2", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.core", "ishop-core-all", "27.1.2-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.core", "ishop-core-all-tests", "27.1.2-SNAPSHOT", "pom", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops-ext", "ishop-shops-ext-shop-base", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "org.apache.httpcomponents", "httpclient", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "org.apache.commons", "commons-lang3", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.github.dreamhead", "moco-core", "", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.netflix.hystrix", "hystrix-core", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "io.reactivex", "rxjava", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.core", "ishop-core-all", "", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "com.novomind.ishop.core", "ishop-core-all-tests", "", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "org.aspectj", "aspectjrt", "1.8.8", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-projects:27.0.0-SNAPSHOT"), "org.aspectj", "aspectjtools", "1.8.8", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops.novosales:novosales-erp:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.core", "ishop-core-projects", "27.1.2-SNAPSHOT", "pom", "import", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "test", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.core", "ishop-core-all", "", "pom", "", ""),
      Dep(PomRef("novosales-erp"), "com.novomind.ishop.core", "ishop-core-all-tests", "", "pom", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales:27.0.0-SNAPSHOT"), "com.novomind.ishop.shops.novosales", "novosales-projects", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.core", "ishop-core-projects", "27.1.2-SNAPSHOT", "pom", "import", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-commons", "27.0.0-SNAPSHOT", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-erp", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-erp", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-db-migration", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops.novosales", "novosales-ipim-reviews", "27.0.0-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.core", "ishop-core-all", "", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.core", "ishop-core-all-tests", "", "pom", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops-ext", "ishop-shops-ext-shop-base", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.backoffice", "bo-services", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.backoffice", "bo-core", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.shops-ext", "shop-footprint", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.exi", "ishop-ext-productdata-ipim-generic", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.exi", "ishop-ext-objectstore-attributeimporter", "27.1.2-SNAPSHOT", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.icustomer", "icustomer-api", "10.1.1", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "commons-fileupload", "commons-fileupload", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "jaxen", "jaxen", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "xerces", "xercesImpl", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.google.code.gson", "gson", "2.4", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "de.jollyday", "jollyday", "0.4.6", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "net.objectlab.kit", "datecalc-common", "1.2.0", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "net.objectlab.kit", "datecalc-joda", "1.2.0", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.jcraft", "jsch", "0.1.42", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "org.kohsuke", "geoip", "1.2.8", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "net.lingala.zip4j", "zip4j", "1.3.1", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.octo.captcha", "jcaptcha", "1.0", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "javax.xml.bind", "jaxb-api", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "org.glassfish.jaxb", "jaxb-runtime", "", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "org.glassfish.jaxb", "jaxb-core", "2.2.11", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "org.jvnet.staxex", "stax-ex", "1.7.7", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.sun.xml.fastinfoset", "FastInfoset", "1.2.13", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "javax.xml.stream", "stax-api", "1.0-2", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.sun.istack", "istack-commons-runtime", "2.21", "", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.plugin", "ishop-plugin-stable", "27.3.0-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.plugin", "ishop-plugin-unstable", "27.3.0-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.novomind.ishop.plugin", "ishop-plugin-demo", "27.3.0-SNAPSHOT", "pom", "", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "javax.servlet.jsp", "jsp-api", "2.2", "", "provided", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.github.jknack", "handlebars", "1.3.1", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "com.github.javaparser", "javaparser-core", "2.3.0", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "javax.el", "javax.el-api", "3.0.0", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "org.glassfish", "javax.el", "3.0.0", "", "test", ""),
      Dep(PomRef("com.novomind.ishop.shops:novosales"), "net.logstash.logback", "logstash-logback-encoder", "4.6", "", "", "")
    )
  }
}
