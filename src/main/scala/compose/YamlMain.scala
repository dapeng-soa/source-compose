package compose

import ammonite.ops
import org.yaml.snakeyaml.Yaml
import ammonite.ops._
import compose.env.entity.EnvEntity

import collection.JavaConverters._
import scala.collection.mutable

object YamlMain {

  def main(args: Array[String]): Unit = {
    val ymlPath = ops.Path(new java.io.File("C:\\dev").getCanonicalPath)
    val appYmlFile = ymlPath / "app-1.yml"

    val content: String = read(appYmlFile, "utf-8")
    val yaml = new Yaml().load(content).asInstanceOf[java.util.Map[String, Any]].asScala

    val services = yaml("services").asInstanceOf[java.util.Map[String, Any]].asScala

    services.map{case (key,value) => {
      println(s" service: ${key}")
      println(" serviceValue=================")
      println(value)
      println(" serviceValue=================")
      val configs: mutable.Map[String, Any] = value.asInstanceOf[java.util.Map[String, Any]].asScala

//      val tService: EnvEntity.TService = toTService(1, 1, 1,configs)
//
//      val tEnvs:
      toTService(1, 1, 1,configs)
    }}
  }


  def toTService(id: Int, tSetId: Long, tHostId: Long, configs:mutable.Map[String, Any]) = {
    println("------------------------------------------------------------------------")
    val containerName = configs("container_name").asInstanceOf[String]
    println(s" containerName: $containerName")
    println("------------------------------------------------------------------------")
    val imageName = configs("image").asInstanceOf[String]
    println(s" imageName: $imageName")
    println("------------------------------------------------------------------------")
    val envs = configs("environment").asInstanceOf[java.util.HashMap[String, Any]].asScala
    println(s" envs: ${envs}")
    println("------------------------------------------------------------------------")
    val extraHosts = configs("extra_hosts").asInstanceOf[java.util.HashMap[String, Any]].asScala
    println(s" extraHosts: $extraHosts")
    println("------------------------------------------------------------------------")
    val labels = configs("labels").asInstanceOf[java.util.HashMap[String, Any]].asScala
    println(s" labels: ${labels}")
    println("------------------------------------------------------------------------")
    val ports = configs("ports").asInstanceOf[java.util.ArrayList[Any]]
    println(s" ports: ${ports}")
    println("------------------------------------------------------------------------")
    val volumes = configs("volumes").asInstanceOf[java.util.ArrayList[Any]].asScala
    println(s" volumns: ${volumes}")
    println("------------------------------------------------------------------------")



  }
}
