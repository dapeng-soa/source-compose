package compose

import java.io.File

import ammonite.ops._
import Utils._

/**
  * Created by wangzx on 16/7/4.
  *
  * @param name               serviceName
  * @param projectName        projectName
  * @param gitURL             project git url
  * @param gitBranch          branch for the project
  * @param relatedSources     related project sources which will be checkout and handle as the main project
  * @param depends            the dependency services, that is, services of depends will be startup before current service
  * @param buildDepends       service of buildDepends will be build before current service
  * @param image              docker image name for current service
  * @param gitSubmoduleFolder does current project include a git submodule?
  * @param npmFolder          if the project needs npm support, npmFolder is the folder where package.json located
  * @param publicImage        is current service an extra public image? such as redis, zk
  *                           TODO add log.
  */
case class Service(name: String, projectName: String, gitURL: String,
                   gitBranch: String, relatedSources: List[Service],
                   depends: List[String], buildDepends: List[Service],
                   image: String, gitSubmoduleFolder: Option[String],
                   npmFolder: Option[String], publicImage: Boolean, buildOperation: String) {
  /**
    * 把当前分支的commitId写入.local.gitid.ini
    *
    * @param context
    */
  def updateGid(context: Context): Unit = {
    if (!publicImage) {
      val projectPath: Path = Path(projectName, Path(context.workspace))

      val commitId = getGitCommitId(projectPath)

      write.append(cwd / Main.gitIdIni.name, s"${name.replace('-', '_')}=$commitId\n")

      println(s"$projectName=$commitId")
    }
  }

  /**
    * 升级报告, git diff master
    * 一般用于灰度
    *
    * @param context
    */
  def sdiff(context: Context): Unit = {
    if (!publicImage) {
      println(s"${
        {
          getSpaces(context)
        }
      }$projectName diff begin")

      if (gitBranch != "master") {
        rm ! Main.reportPath / s"$gitBranch-$projectName.diff"
        val projectPath: Path = Path(projectName, Path(context.workspace))
        val result = intercept(%%.git("diff", "master", gitBranch)(projectPath))
        if (result.exitCode != 0) System.exit(result.exitCode)
        val diffContent = result.out.trim
        write.append(Main.reportPath / s"$gitBranch-$projectName.diff", diffContent)
      }

      println(s"${
        {
          getSpaces(context)
        }
      }$projectName diff done")
    }
  }

  def slog(context: Context, gids: Map[String, String]): Unit = {
    if (!publicImage) {
      println(s"${
        {
          getSpaces(context)
        }
      }$projectName diff-log begin")

      val gid = gids(name.replace('-', '_'))

      if (gitBranch != "master") {
        val projectPath: Path = Path(projectName, Path(context.workspace))
        val masterCommitId = getMasterCommitId(projectPath)
        rm ! Main.reportPath / s"$gitBranch-$projectName.change.log"
        val result = intercept(%%.git("log", s"$masterCommitId..$gid")(projectPath))
        if (result.exitCode != 0) System.exit(result.exitCode)
        val diffLog = result.out.trim
        write.append(Main.reportPath / s"$gitBranch-$projectName.change.log", diffLog)

        exitWhileFailed(name)(%%.git("checkout", gitBranch)(projectPath))
      }

      println(s"${
        {
          getSpaces(context)
        }
      }$projectName diff-log done")
    }
  }

  /**
    * 获得某项目master分支的提交id
    *
    * @param projectPath
    * @return
    */
  def getMasterCommitId(projectPath: Path) = {
    if (!publicImage) {
      exitWhileFailed(name)(%%.git("checkout", ".")(projectPath))
      exitWhileFailed(name)(%%.git("checkout", "master")(projectPath))
      exitWhileFailed(name)(%%.git("pull")(projectPath))

      getGitCommitId(projectPath)
    }
  }

  /**
    * usage:
    * s-merge-by-id branch
    * 需要带分支名,不针对特定service,而是针对所有的services
    *
    * 该操作包括把代码合并到master,并得出升级报告
    *
    * @param context
    */
  def smergeById(context: Context, gid: String): Unit = {
    if (!publicImage) {
      println(s"${
        {
          getSpaces(context)
        }
      }$projectName merge by id begin")
      val projectPath = Path(projectName, Path(context.workspace))


      //checkout to master and get master gid
      val masterCommitId = getMasterCommitId(projectPath)

      val diffFile: Path = Main.reportPath / s"$gitBranch-$projectName.diff"
      val changeLog: Path = Main.reportPath / s"$gitBranch-$projectName.change.log"
      rm ! diffFile
      rm ! changeLog

      val result = intercept(%%.git("diff", s"$masterCommitId..$gid")(projectPath))
      if (result.exitCode != 0) System.exit(result.exitCode)
      val diffContent = result.out.trim
      val logResult = intercept(%%.git("log", s"$masterCommitId..$gid")(projectPath))
      if (logResult.exitCode != 0) System.exit(logResult.exitCode)
      val changeLogContent = logResult.out.trim

      write.append(diffFile, diffContent)
      write.append(changeLog, changeLogContent)


      exitWhileFailed(name)(%%.git("merge", "--ff-only", gid)(projectPath))

      println(s"${
        {
          getSpaces(context)
        }
      }$projectName merge by id End")
    }
  }

  /**
    * must be done after smerge
    *
    * @param context
    */
  def spush(context: Context): Unit = {
    if (!publicImage) {
      println(s"$projectName push begin")
      val projectPath: Path = Path(projectName, Path(context.workspace))


      val branch = getNonDetachGitBranch(projectPath)
      assert(branch == "master",
        s"$projectName is not within master branch, please do smerge first")

      exitWhileFailed(name)(%%.git("push")(projectPath))

      println(s"$projectName push end")
    }
  }

  /**
    * 根据commitId来拉代码
    *
    * @param context
    * @param gids key=serviceName, value=gid
    */
  def spullByCommit(context: Context, gids: Map[String, String]): Unit = {
    if (!publicImage) {
      val gid = gids(name.replace('-', '_'))

      val workspacePath = Path(context.workspace)
      val projectPath = Path(projectName, workspacePath)

      println(s"check $projectPath")
      exitWhileFailed(name)(%%.git("checkout", gid)(projectPath))
    }
  }

  /**
    * pull source code by branch
    *
    * @param context
    */
  def spull(context: Context): Unit = {
    if (!publicImage) {
      println(s"checkout $name $gitURL@@$gitBranch...")

      val workspacePath = Path(context.workspace)
      val projectPath = Path(projectName, workspacePath)

      val isGitSubmodule = gitSubmoduleFolder.isDefined

      if (projectPath.toIO.exists() && projectPath.isDir) {

        val branch = getGitBranch(projectPath)

        if (branch != gitBranch) {
          // switch
          println(s"switch branch from ${branch} to ${gitBranch}")
          exitWhileFailed(name)(%%.git("fetch")(projectPath))
          exitWhileFailed(name)(%%.git("checkout", ".")(projectPath))
          intercept(%%.git("branch", "-d", gitBranch)(projectPath))
          exitWhileFailed(name)(%%.git("checkout", gitBranch)(projectPath))
          if (isGitSubmodule) {
            exitWhileFailed(name)(%%.git("config", "-f", ".gitmodules", "submodule.src/main/webapp/eywa-web.branch", gitBranch)(projectPath))
            exitWhileFailed(name)(%%.git("submodule", "update", "--remote")(projectPath))
          }
        }
        else {
          exitWhileFailed(name)(%%.git("checkout", ".")(projectPath))
          exitWhileFailed(name)(%%.git("pull")(projectPath))
          if (isGitSubmodule) {
            exitWhileFailed(name)(%%.git("submodule", "update", "--remote")(projectPath))
          }
        }
      }
      else {
        // clone the project
        projectPath.toIO.mkdir()
        if (isGitSubmodule) exitWhileFailed(name)(%%.git("clone", "--recursive", gitURL)(workspacePath))
        else
          exitWhileFailed(name)(%%.git("clone", gitURL)(workspacePath))
        if (isGitSubmodule) {
          exitWhileFailed(name)(%%.git("submodule", "update", "--remote")(projectPath))
          exitWhileFailed(name)(%%.git(s"config", "-f", ".gitmodules", "submodule.${gitSubmoduleFolder.get}.branch", gitBranch)(projectPath))
        }
        if (gitBranch != "master") {
          exitWhileFailed(name)(%%.git("checkout", gitBranch)(projectPath))
          exitWhileFailed(name)(%%.git("pull")(projectPath))
          if (isGitSubmodule) {
            exitWhileFailed(name)(%%.git("submodule", "update", "--remote")(projectPath))
          }
        }
      }
    }
  }


  /**
    * buildService: docker/mvn install
    * buildLibrary: publishM2/mvn install
    *
    * @param context
    * @param gids
    */
  def smake(context: Context, gids: Map[String, String], mvnProfile: String): Unit = {
    if (!publicImage) {
      println(s"$projectName make begin")

      val projectPath = Path(projectName, Path(context.workspace))

      val realImage = getRealImage(gids)
      if (isMvnCommand(projectPath) || isSbtCommand(projectPath)) {
        if (gitSubmoduleFolder.isDefined || isNeedBuildLocally(realImage)) {
          println(s" build handled services: ${context.handled}")

          npmFolder.foreach(_npmFolder => {
            val npmPath = Path(_npmFolder, projectPath)
            val buildResult = %.npm("install")(npmPath)

            if (buildResult != 0) System.exit(buildResult)

            val buildResult2 = %.npm("run", "build:happy")(npmPath)

            if (buildResult2 != 0) System.exit(buildResult2)
          })

          if (isMvnCommand(projectPath)) mvnInstall(projectPath, mvnProfile)
          else sbtDocker(projectPath)

        }
      }

      println(s"$projectName make done")
    }
  }

  def sdockerPush(context: Context, gids: Map[String, String]): Unit = {
    if (!publicImage) {
      println(s"$image push begin")
      val workspacePath = Path(context.workspace)
      val projectPath = Path(projectName, workspacePath)

      val imagePattern =
        """(.*):\$\{.*\}""".r
      val realImage = try {
        val imagePattern(_imageName) = image
        s"${_imageName}:${gids(name.replace('-', '_'))}"
      } catch {
        case ex: Throwable =>
          image
      }
      println(s"imageName:$realImage")
      exitWhileFailed(name)(%%.docker("push", realImage)(projectPath))
      println(s"$image push end")
    }
  }

  def sclean(context: Context): Unit = {
    if (!publicImage) {
      println(s"$projectName clean begin")
      val projectPath = Path(projectName, Path(context.workspace))
      sclean(projectPath)
      println(s"$projectName clean end")
    }
  }

  def sclean(projectPath: Path) = {
    if (!publicImage) {
      if (isMvnCommand(projectPath)) {
        if (Main.isWinOs) exitWhileFailed(name)(%%.`mvn.bat`("clean", "-q")(projectPath))
        else exitWhileFailed(name)(%%.mvn("clean", "-q")(projectPath))
      } else if (isSbtCommand(projectPath)) {
        if (Main.isWinOs) exitWhileFailed(name)(%%.`sbt.bat`("clean", "-no-colors")(projectPath))
        else exitWhileFailed(name)(%%.sbt("clean", "-no-colors")(projectPath))
      }
    }
  }

  def sbuild(context: Context, gids: Map[String, String], cacheGids:Map[String, String], mvnProfile: String): Unit = {
    if (!publicImage) {
      println(s"$projectName build begin")
      val beginTimeInMills = System.currentTimeMillis()
      val realImage = getRealImage(gids)
      println(s" realImage: ${realImage}")

      if (isNeedBuildLocally(realImage)) {
        println(s"need rebuild ${projectName} dependsProjects: ${buildDepends.filterNot { i => context.handled.contains(i.name) }}")
        buildDependsProjects(context, mvnProfile, cacheGids)
      }

      smake(context, gids, mvnProfile)
      println(s"$projectName build end, cost:${(System.currentTimeMillis() - beginTimeInMills) / 1000}")
    }
  }

  def srebuild(context: Context, gids: Map[String, String], cacheGids: Map[String, String] , mvnProfile: String): Map[String, String] = {
    if (!publicImage) {
      println(s"SERVICE_CALCULATE $projectName rebuild begin")

      val beginTimeInMills = System.currentTimeMillis()
      val realImage = getRealImage(gids)
      //println(s" realImage: ${realImage}")

      val dependencyProjectBuildBeginTime = System.currentTimeMillis()
      val (newCacheGids, dependencyBuildTimes): (Map[String, String], List[(String, Long)]) = if (npmFolder.isDefined || isNeedBuildLocally(realImage)) {
        println(s"SERVICE_CALCULATE Rebuild ${projectName} dependsProjects: ${buildDepends.filterNot { i => context.handled.contains(i.name) }}")

        buildDependsProjects(context, mvnProfile, cacheGids)
      } else {
        (Map[String, String](), List[(String, Long)]())
      }
      val dependencyProjectBuildEndTime = System.currentTimeMillis()

      val dependProjectPath = Path(projectName, Path(context.workspace))
      val projectCommmitId = getGitCommitId(dependProjectPath)
      val cacheGid = (cacheGids ++ newCacheGids).get(name)

      println(s"SERVICE_CALCULATE $projectName dependPath: $dependProjectPath projectCommitId: $projectCommmitId, name: $name cacheGid: $cacheGid")
      val (projectCleanBeginTime, projectCleanEndTime, makeBeginTime,makeEndTime) =
        if (gitSubmoduleFolder.isDefined || !cacheGid.isDefined || !cacheGid.get.equals(projectCommmitId)) {
        val projectCleanBeginTime = System.currentTimeMillis()
        sclean(context)
        val projectCleanEndTime = System.currentTimeMillis()

        val makeBeginTime = System.currentTimeMillis()
        smake(context, gids, mvnProfile)
        val makeEndTime = System.currentTimeMillis()

        (projectCleanBeginTime, projectCleanEndTime, makeBeginTime,makeEndTime)
      } else {
        (0L,0L,0L,0L)
      }

      //update .build.cache.ini
      val projectPath: Path = Path(projectName, Path(workspace))
      val commitId = getGitCommitId(projectPath)

      dependencyBuildTimes.foreach(i => println(s"SERVICE_CALCULATE ${i._1} build time: ${i._2} ms, toSeconds: ${i._2 / 1000} s"))

      println(s"SERVICE_CALCULATE dependency build totalTime: ${dependencyProjectBuildEndTime - dependencyProjectBuildBeginTime} ms, toSeconds: ${(dependencyProjectBuildEndTime - dependencyProjectBuildBeginTime) / 1000} s")
      println(s"SERVICE_CALCULATE $projectName rebuild cleanTime: ${projectCleanEndTime - projectCleanBeginTime} ms,  makeTime: ${makeEndTime - makeBeginTime} ms")
      println(s"SERVICE_CALCULATE $projectName rebuild end, cost:${(System.currentTimeMillis() - beginTimeInMills)} ms, toSeconds: ${(System.currentTimeMillis() - beginTimeInMills) / 1000} s")
      println(s"SERVICE_CALCULATE ")

      val updatedGids = cacheGids ++ newCacheGids ++ Map(name -> commitId)
      updatedGids
    } else {
      cacheGids
    }
  }

  private def getSpaces(context: Context) = if (context.relatedSources.contains(name)) "  " else ""


  private def mvnInstall(projectPath: Path, mvnProfile: String) = {

    println(s" mvn install projectPath: ${projectPath.last}")

    val mvnOpts: List[String] = if ("-Pproduction".equals(mvnProfile)
      && projectPath.last.toLowerCase.contains("api")) {
      List("-q", "deploy", "-Dmaven.test.skip=true", "-e", mvnProfile)
    } else {
      List("-q", "install", "-Dmaven.test.skip=true") ::: (if (mvnProfile.equals("")) List("-Pproduction") else List(mvnProfile))
    }

    println(s" Mvn install profile: ${mvnOpts}")

    val _makeResult: Int = if (Main.isWinOs) %.`mvn.bat`(mvnOpts)(projectPath)
    else %.mvn(mvnOpts)(projectPath)

    if (_makeResult != 0) System.exit(_makeResult)
  }

  private def sbtDocker(projectPath: Path) = {
    val sbtOpts = List("compile", "publishLocal", "docker", "-no-colors")
    val buildResult = if (Main.isWinOs) %.`sbt.bat`(sbtOpts)(projectPath) else %.`sbt`(sbtOpts)(projectPath)

    if (buildResult != 0) System.exit(buildResult)
  }


  /**
    * 只构建依赖的api
    *
    * @param projectPath
    */
  private def sbtPackage(projectPath: Path) = {
    val sbtOpts = List("api/compile", "api/package", "api/publish", "api/publishLocal", "api/publishM2", "-no-colors")

    val buildResult = if (Main.isWinOs) %.`sbt.bat`(sbtOpts)(projectPath) else %.`sbt`(sbtOpts)(projectPath)
    if (buildResult != 0) System.exit(buildResult)

  }

  private def isMvnCommand(projectPath: Path) = (ls ! projectPath |? (_.name == "pom.xml")).nonEmpty

  private def isSbtCommand(projectPath: Path) = (ls ! projectPath |? (_.name == "build.sbt")).nonEmpty


  private def isNeedBuildLocally(realImage: String): Boolean = {
    val response = intercept(%%.docker("images", realImage)(cwd))
    if (response.exitCode != 0) System.exit(response.exitCode)
    val needBuildLocally = if (response.out.lines.size == 1) {
      if (!Main.skipRemoteCheck) {
        println(s"${realImage} not found locally, now check remote registry")
        val pullResult = intercept(%%.docker("pull", realImage)(cwd))
        if (pullResult.exitCode != 0) {
          System.exit(pullResult.exitCode)
          false
        } else {
          true
        }
      } else true
    } else false

    needBuildLocally
  }

  private def buildDependsProjects(context: Context, mvnProfile: String, cacheGids: Map[String, String]): (Map[String, String], List[(String, Long)]) = {

    val newCacheGids = collection.mutable.HashMap[String,String]()
    newCacheGids ++= cacheGids

    val denpendencyBuildTimes = buildDepends.filterNot { buildDependService =>
      context.handled.contains(buildDependService.name)
    }.map { buildDependService =>
      println(s"${buildDependService.projectName} make begin")
      val buildBeginTime = System.currentTimeMillis()

      val dependProjectPath = Path(buildDependService.projectName, Path(context.workspace))
      val projectCommmitId = getGitCommitId(dependProjectPath)
      val cacheGid = newCacheGids.get(buildDependService.name)

      println(s"compare buildDependService.projectName: ${buildDependService.projectName}, buildDependService.name: ${buildDependService.name}  CommitIdByProjectName: ${projectCommmitId}, cacheGidByName: ${cacheGid}")
      if (!cacheGid.isDefined || !cacheGid.get.equals(projectCommmitId)) {
        println(s" SERVICE_CALCULATE need to rebuild dependency projectName: ${buildDependService.projectName}, buildDependService.name: ${buildDependService.name}")
        val _projectPath = Path(buildDependService.projectName, Path(context.workspace))

        sclean(_projectPath)
        if (isMvnCommand(_projectPath)) {
          mvnInstall(_projectPath, mvnProfile)
        } else if (isSbtCommand(_projectPath)) {
          sbtPackage(_projectPath)
          sbtDocker(_projectPath)
        }

      } else {
        println(s"SERVICE_CALCULATE no need to rebuild dependency projectName: ${buildDependService.projectName}, buildDependService.name: ${buildDependService.name}")
      }
      val buildEndTime = System.currentTimeMillis()

      newCacheGids.put(buildDependService.name, projectCommmitId)

      context.handled += buildDependService.name

      (buildDependService.name, buildEndTime - buildBeginTime)
    }

    (newCacheGids.toMap, denpendencyBuildTimes)
  }


  private def getRealImage(gids: Map[String, String]): String = {
    // check if images has been make
    // two forms of images:
    // 1. image:${git.branch}-${xx_gid}  for biz projects which will be change frequently
    // 2. image:master for basic projects which won't be change
    val imagePattern =
    """(.*):\$\{.*\}""".r
    try {
      val imagePattern(_imageName) = image
      s"${_imageName}:${gids(name.replace('-', '_'))}"
    } catch {
      case ex: Throwable =>
        image
    }
  }
}
