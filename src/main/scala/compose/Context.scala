package compose

import ammonite.ops._
import org.yaml.snakeyaml.Yaml

import Utils._

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Created by wangzx on 16/7/4.
  *
  * project.source=http://some.com/example.git@@branch
  * project.source.1=
  * project.depends=servicea,serviceb
  */

class Context {
  var services: Map[String, Service] = Map()

  var sortedServices: List[Service] = Nil

  lazy val relatedSources: Set[String] = sortedServices.flatMap(_.relatedSources).map(_.name).toSet
  var handled: Set[String] = Set()


  val workspace = {
    val prop = System.getProperty("COMPOSE_WORKSPACE")
    if (prop != null) prop
    else {
      val env = System.getenv("COMPOSE_WORKSPACE")
      if (env != null) env
      else throw new RuntimeException("please specify COMPOSE_WORKSPACE via -D or environment.")
    }
  }

  val scomposeBranch = {
    getNonDetachGitBranch(cwd)
  }

  def loadConfiguration(ymlFiles: Seq[Path]): Context = {
    val NonOperationPattern= """(.*/(.*?)\.git)@@(.*)""".r
    val OperationPattern= """(.*/(.*?)\.git)@@(.*);(.*)""".r

    def getLabelDetail(labelValue: String) = {
      labelValue match {
        case OperationPattern(gitUrl, projectName, branch, operation) => (gitUrl, projectName, branch, operation)
        case NonOperationPattern(gitUrl, projectName, branch) => (gitUrl, projectName, branch, "")
        case _ => ("","","","")
      }
    }
    def getLabels(service: collection.mutable.Map[String, Any]): Map[String, String]  = service("labels").asInstanceOf[java.util.List[String]].asScala.map { label =>
      val pos = label.indexOf('=')
      (label.substring(0, pos) -> label.substring(pos + 1))
    }.toMap

    def buildService(name: String, service: collection.mutable.Map[String, Any], images: Map[String, (String,String)]): Service = {

      val labels: Map[String, String] = getLabels(service)

      // 是否是dockerHub上的共有镜像, 例如redis
      val publicImage = labels.getOrElse("project.extra", "").contains("public-image")

      //val Pattern(gitURL, gitName, gitBranch) = if (publicImage) "dapeng/dapeng.git@@master" else labels("project.source")
      val sourceFieldVal = if (publicImage) "dapeng/dapeng.git@@master" else labels("project.source")
      val (gitURL, gitName, gitBranch, buildOperation) = getLabelDetail(sourceFieldVal)

      val relatedSources = labels.filterKeys(_.startsWith("project.source.")).map { case (_, value) =>
        val (relateGitURL, relateProjectName, relateProjectBranch, relateProjectOp) = getLabelDetail(value)
        Service(
          name = images(name)._1,
          projectName = relateProjectName,
          gitURL = relateGitURL,
          gitBranch = relateProjectBranch,
          relatedSources = Nil,
          depends = Nil,
          buildDepends = Nil,
          image = images(name)._2,
          gitSubmoduleFolder = None,
          npmFolder = None,
          publicImage = false,
          relateProjectOp
        )
      }.toList

      val depends: Array[String] = labels.getOrElse("project.depends", "").split(",").map(_.trim).filterNot(_.isEmpty)

      val buildDepends = labels.filterKeys(_.startsWith("project.build-depends.")).toList
        .sortBy(i => (i._1.substring(i._1.lastIndexOf(".") + 1)).toInt)
        .map { case (_, value) =>
          val (dependGitURL, dependName, dependBranch, dependOp) = getLabelDetail(value)
          val imageName = if (images.contains(name)) images(name)._2 else ""
          val serviceName = if (images.contains(dependName)) images(dependName)._1 else dependName
          Service(
            name = serviceName,
            projectName = dependName,
            gitURL = dependGitURL,
            gitBranch = dependBranch,
            relatedSources = Nil,
            depends = Nil,
            buildDepends = Nil,
            image = imageName,
            gitSubmoduleFolder = None,
            npmFolder = None,
            publicImage = false,
            dependOp
          )
        }


      Service(name = name,
        projectName = gitName,
        gitURL = gitURL,
        gitBranch = gitBranch,
        relatedSources = relatedSources ::: buildDepends,
        depends = depends.toList,
        buildDepends = buildDepends,
        image = service("image").toString,
        gitSubmoduleFolder = labels.get("project.submodule-folder"),
        npmFolder = labels.get("project.npm-folder"),
        publicImage = publicImage,
        buildOperation)
    }

    def getImages(ymlFiles: Seq[Path]) = {
      ymlFiles.flatMap{ (ymlFile: Path) => {
        val content: String = read(ymlFile, "utf-8")

        val yaml = new Yaml().load(content).asInstanceOf[java.util.Map[String, Any]].asScala

        val servicesYaml = yaml("services").asInstanceOf[java.util.Map[String, Any]].asScala
        val images: mutable.Map[String, (String,String)] = servicesYaml.flatMap{case (name, serviceNode) => {
          val service =  serviceNode.asInstanceOf[java.util.Map[String, Any]].asScala
          val labels = getLabels(service)
          val realProjectName = labels.filterKeys(_.startsWith("project.source")).toList.filterNot(i => i._2 == null || i._2.isEmpty).map{case (_, value) => {
            val Pattern = """(.*/(.*?)\.git)@@(.*)""".r
            val Pattern(_, name, _) = value
            name
          }}
          realProjectName.map(i => i -> (name,service("image").toString))
        }}
        images
      }}.toMap
    }

    // goods -> (goodsService[use for replace gid], imageName)
    val images: Map[String, (String,String)] =  getImages(ymlFiles)

    ymlFiles.foreach { (ymlFile: Path) =>
      val content: String = read(ymlFile, "utf-8")

      val yaml = new Yaml().load(content).asInstanceOf[java.util.Map[String, Any]].asScala

      val servicesYaml = yaml("services").asInstanceOf[java.util.Map[String, Any]].asScala

      val services = servicesYaml.map {
        case (name, serviceNode) => {
          buildService(name, serviceNode.asInstanceOf[java.util.Map[String, Any]].asScala, images)
        }

      }.toList

      this.services ++= services.map(service => (service.name, service)).toMap
    }

    // 所有的构建依赖api包,都必须是同一分支, 否则就报错
    this.services.values.flatMap(_.buildDepends).groupBy(_.name).values.toList.map(_.toList).foreach { buildServices =>
      assert(buildServices.map(_.gitBranch).toSet.size == 1,
        s"Please check branch of project:${buildServices(0).name}. They should be the same branch")
    }

    this.sortedServices = sort(this.services)

    this
  }

  def sort(services: Map[String, Service]): List[Service] = {

    val todo: mutable.Buffer[Service] = scala.collection.mutable.Buffer[Service]() ++ services.values
    val sorted = scala.collection.mutable.Buffer[Service]()

    todo.foreach { service =>
      val invalidDepends = service.depends.filter(name => !services.contains(name))
      assert(invalidDepends.isEmpty, s"service ${service.name} has invalid depends $invalidDepends")
    }

    val items = todo.filter(service => service.depends.isEmpty)
    todo --= items
    sorted ++= items

    while (todo.nonEmpty) {
      val items = todo.filter { service =>
        service.depends.forall(dep => sorted.exists(_.name == dep))
      }
      if (items.nonEmpty) {
        todo --= items
        sorted ++= items
      }
      else {
        throw new RuntimeException(s"project.depends must having cycle depends for ${todo.map(_.name)}")
      }
    }

    sorted.toList
  }
}
