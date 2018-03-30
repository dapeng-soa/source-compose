package compose

import java.io.File

import ammonite.ops._

/**
  * Created by Ever on 16/8/12.
  */
object Utils {

  def updateLastGitidIni(projectName: String, serviceName: String) = {
    val file = new File((cwd / Main.lastGitIdIni.name).toString)
    if (!file.exists()) {
      file.createNewFile()
      println(s" .local.last.gitid.ini file does not exists. created new File..")
    }

    val properties = Main.loadPropertiesByIni(Main.lastGitIdIni.name)
    println(s"updating built project gitId: ${properties}")

    if (!properties.keySet.contains(serviceName)) {
      updateGid(projectName, serviceName, Path(workspace))
    }
  }

  val workspace = {
    val prop = System.getProperty("COMPOSE_WORKSPACE")
    if (prop != null) prop
    else {
      val env = System.getenv("COMPOSE_WORKSPACE")
      if (env != null) env
      else throw new RuntimeException("please specify COMPOSE_WORKSPACE via -D or environment.")
    }
  }

  def updateGid(projectName: String, serviceName: String, path: Path): Unit = {
      val projectPath: Path = Path(projectName, path)

      val commitId = getGitCommitId(projectPath)

      write.append(cwd / Main.lastGitIdIni.name, s"${serviceName.replace('-', '_')}=$commitId\n")

      println(s"update $path properties: $serviceName=$commitId")
  }



  /**
    * get path to the head of current "non detach branch"
    *
    * @param projectPath
    * @return
    */
  def getGitHead(projectPath: Path): String = {
    val headContent = readGitHeadContent(projectPath)
    val headSegs = headContent.split(" ")
    assert(headSegs.size > 1, s"Current branch is 'Detach':$headContent, please switch to proper branch")
    headSegs.last
  }

  /**
    * get branch name for "non detach branch"
    *
    * @param projectPath
    * @return
    */
  def getNonDetachGitBranch(projectPath: Path): String = {
    val head = getGitHead(projectPath)
    head.split("/").last
  }

  /**
    * get branch name.
    *
    * @param projectPath
    * @return "DETACHED-HEAD" when at 'detached HEAD' state, branchName otherwise
    */
  def getGitBranch(projectPath: Path): String = {
    val headContent = readGitHeadContent(projectPath)
    val headSegs = headContent.split(" ")
    if (headSegs.size > 1) {
      headSegs.last.split("/").last
    } else "DETACHED-HEAD"
  }

  def getGitCommitId(projectPath: Path): String = {
    val head = getGitHead(projectPath)
    read(Path(s"${projectPath.toString}/.git/$head")).trim.substring(0, 7)
  }

  /**
    * perform some operation related to shell, which may cause a ShelloutExceptioin whose exitCode!=0.
    * such as git, mvn etc.
    *
    * @param f
    * @return
    */
  def intercept(f: => CommandResult): CommandResult = {
    try {
      val result = f
      if (Main.debugMode) println(result.out.trim)
      result
    }
    catch {
      case ex: ShelloutException =>
        println(s"CommonError:exitCode:${ex.result.exitCode}, \nmsg:${ex.result.toString()}")
        if (Main.debugMode) println(ex.result.out.trim)
        ex.result
    }
  }

  /**
    * perform some operation related to shell, exit when a ShelloutExceptioin is throw out whose exitCode!=0.
    * such as git, mvn etc.
    *
    * @param f
    * @return
    */
  def exitWhileFailed(service: String)(f: => CommandResult): Unit = {
    try {
      val result = f
      if (Main.debugMode) println(result.out.trim)
    }
    catch {
      case ex: ShelloutException =>
        println(s"$service: \nCommonError:exitCode${ex.result.exitCode}, \nmsg:${ex.result.toString()}")
        if (Main.debugMode) println(ex.result.out.trim)
        else println("please add option -X for more detail")
        System.exit(ex.result.exitCode)
    }
  }

  //implicit class CommandResultEx(x: )

  /**
    * 读取当前分支的HEAD
    *
    * @param projectPath
    * @return
    */
  def readGitHeadContent(projectPath: Path): String = {
    read(Path(s"${projectPath.toString}/.git/HEAD")).trim
  }
}
