package release

import java.io.File
import java.nio.file.{Files, StandardCopyOption}

import org.junit.{Assert, Assume, Test}
import org.scalatest.junit.AssertionsForJUnit
import release.Sgit.{GitRemote, MissigGitDirException, Os}
import release.SgitTest.hasCommitMsg
import release.Starter.PreconditionsException

class SgitTest extends AssertionsForJUnit {

  @Test
  def testSelectGitCmd(): Unit = {
    val git = Sgit.selectedGitCmd(System.err, None)

    Sgit.getOs() match {
      case Os.Windows ⇒ {
        Assert.assertEquals(Seq("C:\\Programme\\Git\\bin\\git.exe"), git)
      }
      case Os.Linux ⇒ {
        Assert.assertEquals(Seq("git"), git)
      }
      case Os.Darwin ⇒ {
        Assert.assertEquals(Seq("git"), git)
      }
      case other ⇒ Assert.fail("unknown os: " + other + " => " + git)
    }
  }

  @Test
  def testMissingGitDir(): Unit = {

    TestHelper.assertExceptionWithCheck(message ⇒ Assert.assertEquals("no .git dir in sgit-test was found",
      message.replaceFirst("[^ ]+sgit-test-[^ ]+", "sgit-test"))
      , classOf[MissigGitDirException], () ⇒ {
        val temp = Files.createTempDirectory("sgit-test-").toFile.getAbsoluteFile
        temp.deleteOnExit()
        Sgit(file = temp, showGitCmd = false, doVerify = hasCommitMsg, out = System.out, err = System.err, gitBin = None)
      })

  }

  @Test
  def testCommitId(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val commitId = status.commitId("606411c")

    // THEN
    Assert.assertEquals("606411c1e4d62030144e9351fe0567cf3a5e5046", commitId)
  }

  @Test
  def testFindUpstream(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()
    val currentBranch = status.currentBranch
    Assume.assumeTrue("not master", currentBranch.contains("master"))

    // WHEN
    val branch = status.findUpstreamBranch().getOrElse("other")

    // THEN
    Assert.assertEquals("master", branch)
  }

  @Test
  def testCurrentBranch(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val branch = status.currentBranch

    // THEN
    Assert.assertTrue("unknown branch " + branch, Seq("master", "HEAD").contains(branch))
  }

  @Test
  def testCommitIdHEAD(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val commitId = status.commitId("origin/master")

    // THEN
    Assert.assertEquals(40, commitId.length)
  }

  @Test
  def testCommitIds(): Unit = {
    // GIVEN
    val status = SgitTest.workSgit()

    // WHEN
    val commitIds = status.commitIds("21a1a3f", "606411c").map(_.substring(0, 7))

    // THEN
    Assert.assertTrue(commitIds.isInstanceOf[List[String]])
    Assert.assertEquals(Seq("21a1a3f", "b299ac9", "29c5b35"), commitIds)
  }

  @Test
  def testSelectUpstream_master(): Unit = {

    Assert.assertEquals(Some("master"), SgitTest.workSgit().selectUpstream(Some("origin/master")))

  }

  @Test
  def testSelectUpstream_feature(): Unit = {

    Assert.assertEquals(Some("feature/any"), SgitTest.workSgit().selectUpstream(Some("origin/feature/any")))

  }

  @Test
  def testSelectUpstream_none(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(None))

  }

  @Test
  def testSelectUpstream_other_master(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(Some("other/master")))

  }

  @Test
  def testSelectUpstream_other_feature(): Unit = {

    Assert.assertEquals(None, SgitTest.workSgit().selectUpstream(Some("other/feature/any")))

  }

  @Test
  def testOutLogger(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(syserrErrors = true, Seq("fetch"),
      in ⇒ err = err :+ in, in ⇒ out = out :+ in, in ⇒ Some(in))

    // WHEN
    testee.err("any")

    // THEN
    Assert.assertEquals(Seq("any"), err)
  }

  @Test
  def testOutLoggerEmpty(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(syserrErrors = true, Seq("fetch"),
      in ⇒ err = err :+ in, in ⇒ out = out :+ in, Sgit.fetchFilter)

    // WHEN
    testee.err("Total 31 (delta 0), reused 0 (delta 0)")
    testee.err("Total 286 (delta 177), reused 278 (delta 177)")

    // THEN
    Assert.assertEquals(Nil, err)
  }

  @Test
  def testOutLoggerGerrit_push(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(syserrErrors = true, Seq("push"),
      in ⇒ err = err :+ in, in ⇒ out = out :+ in, Sgit.gerritPushFilter)

    // WHEN
    testee.err("remote: ")
    testee.err("remote: Processing changes: new: 2, refs: 3")
    testee.err("remote: Processing changes: new: 2, refs: 3")
    testee.err("remote: Processing changes: new: 2, refs: 3, done")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: New Changes:")
    testee.err("remote: New Changes:        ")
    testee.err("remote:   https://git-ishop.novomind.com:9091/72458 snap weg")
    testee.err("remote:   https://git-ishop.novomind.com:9091/72459 [ishop-release] prepare for next iteration - 29.0.6")
    testee.err("remote: ")

    // THEN
    Assert.assertEquals(List("See https://git-ishop.novomind.com:9091/72458 snap weg",
      "See https://git-ishop.novomind.com:9091/72459 [ishop-release] prepare for next iteration - 29.0.6"), err)
  }

  @Test
  def testOutLoggerGerrit_push_fail(): Unit = {
    var err: Seq[String] = Seq.empty[String]
    var out: Seq[String] = Seq.empty[String]
    val testee = Sgit.outLogger(syserrErrors = true, Seq("push"),
      in ⇒ err = err :+ in, in ⇒ out = out :+ in, Sgit.gerritPushFilter)

    // WHEN
    testee.err("remote: ")
    testee.err("remote: Processing changes: refs: 2")
    testee.err("remote: Processing changes: refs: 2, done")
    testee.err("remote: ")
    testee.err("remote: error: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\"")
    testee.err("remote: ")
    testee.err("remote: ")
    testee.err("remote: error: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\"")
    testee.err("To ssh://tstock@git-ishop.novomind.com:19418/ishop/user/tstock/sonar-demo")
    testee.err(" ! [remote rejected] master -> refs/for/master " +
      "(Commit Message Problem: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\")")
    testee.err(" ! [remote rejected] v29.0.6 -> v29.0.6 " +
      "(Commit Message Problem: Line is too long. Reduce it to 72 chars please; " +
      "\"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge\")")
    testee.err("error: failed to push some refs to 'ssh://tstock@git-ishop.novomind.com:19418/ishop/user/tstock/sonar-demo'")

    // THEN
    Assert.assertEquals(
      List(
        """[remote rejected] master -> refs/for/master
          |(Commit Message Problem: Line is too long. Reduce it to 72 chars please;
          |"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge")""".stripMargin
          .replaceAll("[\r\n]+", " "),
        """[remote rejected] v29.0.6 -> v29.0.6
          |(Commit Message Problem: Line is too long. Reduce it to 72 chars please;
          |"Eine wichtichte asfasd asdf asf sd fjrei eirgj eiogjeio jgirj ierjiogj iodfgj ioeiojgierge")""".stripMargin
          .replaceAll("[\r\n]+", " "),
        """git-err: 'error: failed to push some refs to
          |'ssh://tstock@git-ishop.novomind.com:19418/ishop/user/tstock/sonar-demo''""".stripMargin
          .replaceAll("[\r\n]+", " ")), err)
  }

  private def unw(input: String) = {
    input.trim match {
      case in: String if in.startsWith("\"") && in.endsWith("\"") ⇒ in.replaceFirst("^[\"]+", "").replaceFirst("[\"]+$", "")
      case in ⇒ in
    }
  }

  @Test
  def testUnw(): Unit = {
    Assert.assertEquals("a", unw("a"))
    Assert.assertEquals("a\na", unw("a\na"))
    Assert.assertEquals("a\na ", unw("\"a\na \""))
    Assert.assertEquals("a\na ", unw("\"a\na \" "))
    Assert.assertEquals("a\na ", unw("\"a\na \" \n"))
    Assert.assertEquals("a\na ", unw("\"a\na \"\n"))
    Assert.assertEquals("a\na ", unw("\"\"a\na \"\"\n"))
    Assert.assertEquals("a\n\"a ", unw("\"\"a\n\"a \"\"\n"))
  }

  @Test
  def testGitNative(): Unit = {
    TestHelper.assertException("Nonzero exit value: 1; git --no-pager iutghiprjhpeth; " +
      "git: 'iutghiprjhpeth' is not a git command. See 'git --help'.",
      classOf[RuntimeException], () ⇒ {
        SgitTest.workSgit().gitNative(Seq("iutghiprjhpeth"))
      })

  }

  private def testFailIllegal(expectedMsg: String, fn: () ⇒ Unit): Unit = {
    TestHelper.assertException(expectedMsg, classOf[IllegalStateException], fn)
  }

  @Test
  def testInitCloneCommitPoms(): Unit = {

    def testFile(folder: File, name: String): File = {
      val testFile = new File(folder, name)
      testFile.createNewFile()
      testFile
    }

    // GIVEN
    val testRepoA = new File(Util.localWork, "target/a")
    if (testRepoA.isDirectory) {
      Util.delete(testRepoA)
    }

    val testRepoB = new File(Util.localWork, "target/b")
    if (testRepoB.isDirectory) {
      Util.delete(testRepoB)
    }

    def copyMsgHook(to: File): Unit = {
      if (SgitTest.hasCommitMsg) {
        Files.copy(SgitTest.commitMsg, to.toPath.resolve(".git/hooks/commit-msg"), StandardCopyOption.REPLACE_EXISTING)
      }
    }

    def assertMsg(expected: Seq[String], sgit: Sgit): Unit = {
      val gitRawOut = sgit.gitNative(Seq("log", "-n1", "--pretty=\"%B\""))
      val unwrapped = unw(gitRawOut)
      val nativeLines = unwrapped.lines.toList
      val body = nativeLines match {
        case lines if lines.last.startsWith("Change-Id:") ⇒ lines.dropRight(1)
        case lines ⇒ Assert.fail("invalid lines: " + lines.mkString("|"))
      }
      Assert.assertEquals(expected, body)
      val hookLines = nativeLines.takeRight(1)
      Assert.assertEquals("Change-Id:", hookLines.head.replaceFirst(" .*", "").trim)
    }

    // WHEN
    val gitA = Sgit.init(testRepoA, showGitCmd = false, SgitTest.hasCommitMsg)

    TestHelper.assertException("Nonzero exit value: 1; git --no-pager push -q -u origin master:refs/for/master; " +
      "git-err: 'error: src refspec master does not match any.' " +
      "git-err: 'error: failed to push some refs to 'origin''",
      classOf[RuntimeException], () ⇒ {
        gitA.pushFor("master", "master")
      })

    gitA.fetchAll()
    Assert.assertEquals(Nil, gitA.remoteList())
    gitA.remoteAdd("ubglu", "failfail")
    TestHelper.assertException("Nonzero exit value: 1; " +
      "git --no-pager fetch -q --all --tags; fatal: 'failfail' does not appear to be a git repository " +
      "fatal: Could not read from remote repository. Please make sure you have the correct access rights " +
      "and the repository exists. error: Could not fetch ubglu",
      classOf[RuntimeException], () ⇒ {
        gitA.fetchAll()
      })
    TestHelper.assertException("Nonzero exit value: 1; git --no-pager push -q -u origin master:refs/heads/master; " +
      "git-err: 'error: src refspec master does not match any.' " +
      "git-err: 'error: failed to push some refs to 'origin''",
      classOf[RuntimeException], () ⇒ {
        gitA.pushHeads("master", "master")
      })

    Assert.assertEquals(Seq(GitRemote("ubglu", "failfail", "(fetch)"), GitRemote("ubglu", "failfail", "(push)")),
      gitA.remoteList())
    gitA.remoteRemove("ubglu")
    gitA.remoteAdd("ubglu", "ssh://git.example.org/ubglu")

    def failFetchAll(msg: String): Unit = {
      TestHelper.assertException(msg,
        classOf[RuntimeException], () ⇒ {
          gitA.fetchAll()
        })
    }

    Sgit.getOs() match {
      case Os.Windows ⇒ {
        failFetchAll("Nonzero exit value: 1; git --no-pager fetch -q --all --tags; " +
          "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: " +
          "Could not read from remote repository. " +
          "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu")
      }
      case Os.Linux ⇒ {
        failFetchAll("Nonzero exit value: 1; git --no-pager fetch -q --all --tags; " +
          "ssh: Could not resolve hostname git.example.org: Name or service not known fatal: " +
          "Could not read from remote repository. " +
          "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu")
      }
      case Os.Darwin ⇒ {
        failFetchAll("Nonzero exit value: 1; git --no-pager fetch -q --all --tags; " +
          "ssh: Could not resolve hostname git.example.org: nodename nor servname provided, or not known fatal: " +
          "Could not read from remote repository. " +
          "Please make sure you have the correct access rights and the repository exists. error: Could not fetch ubglu")
      }
      case other ⇒ Assert.fail("unknown os: " + other)
    }

    gitA.remoteRemove("ubglu")
    Assert.assertEquals(Nil, gitA.branchListLocal())
    Assert.assertEquals(Nil, gitA.lsFiles())
    Assert.assertEquals(Nil, gitA.branchListRemoteRefRemotes())
    Assert.assertEquals(None, gitA.findUpstreamBranch())
    copyMsgHook(testRepoA)

    Assert.assertEquals(Nil, gitA.localChanges())
    gitA.add(testFile(testRepoA, "test"))

    Assert.assertEquals(Seq("A test"), gitA.localChanges())
    gitA.commitAll("add test")
    Assert.assertEquals(Seq("Change-Id: I0000000000000000000000000000000000000000"), gitA.commitMessageBody("HEAD"))
    Assert.assertEquals(Nil, gitA.localChanges())
    Assert.assertEquals("master", gitA.currentBranch)
    Assert.assertEquals(Seq("refs/heads/master"), gitA.branchListLocal().map(_.branchName))
    Assert.assertEquals(None, gitA.findUpstreamBranch())
    val gitB = Sgit.clone(testRepoA, testRepoB, showGitCmd = false, SgitTest.hasCommitMsg)
    copyMsgHook(testRepoB)
    gitA.config("receive.denyCurrentBranch", "warn")
    gitA.add(testFile(testRepoA, "test2"))
    gitA.commitAll("add test2")
    gitB.fetchAll()
    Assert.assertEquals(Seq("origin/master"), gitB.branchListRemoteRaw().map(_.branchName))
    Assert.assertEquals(Seq("origin/master"), gitB.branchNamesRemote())
    Assert.assertEquals(Seq("refs/remotes/origin/master"), gitB.branchListRemoteRefRemotes().map(_.branchName))
    Assert.assertEquals(Seq("master"), gitB.branchNamesRemoteShort())
    Assert.assertEquals(Seq("master"), gitB.branchNamesAll())
    Assert.assertEquals("master", gitB.currentBranch)
    Assert.assertEquals("master", gitB.findUpstreamBranch().get)
    Assert.assertFalse(gitB.hasChangesToPush)

    Assert.assertEquals(Some("master"), gitB.findUpstreamBranch())
    val beforeReverts = gitB.commitId("HEAD")
    gitB.revertHead()
    Assert.assertEquals(Seq("This reverts commit ..."),
      gitB.commitMessageBody("HEAD").map(_.replaceFirst("[0-9a-f]{40}.$", "...")))
    gitB.revertHead()
    TestHelper.assertExceptionWithCheck(message ⇒
      Assert.assertEquals("The commits 000, 000 has no ChangeId lines. Please amend them manually.",
        message.replaceAll("[0-9a-f]{40}", "000"))
      , classOf[PreconditionsException], () ⇒ {
        gitB.pushFor("master", "master")
      })
    gitB.resetHard(beforeReverts)
    gitB.pushFor("master", "master")
    Assert.assertEquals(Nil, gitA.listTags())
    Assert.assertEquals(Nil, gitB.listTags())
    gitB.doTag("0.0.9")
    Assert.assertEquals(Seq("v0.0.9"), gitB.listTags())
    Assert.assertEquals(Nil, gitA.listTags())
    gitB.pushTag("0.0.9")
    Assert.assertEquals(Seq("v0.0.9"), gitA.listTags())
    val pomFile = testFile(testRepoB, "pom.xml")
    val sub = new File(testRepoB, "sub")
    sub.mkdir()
    val subPomFile = testFile(sub, "pom.xml")
    Assert.assertEquals(Seq("?? pom.xml", "?? sub/"), gitB.localChanges())
    gitB.stash()
    Assert.assertEquals(Nil, gitB.localChanges())
    gitB.stashPop()
    Assert.assertEquals(Seq("?? pom.xml", "?? sub/"), gitB.localChanges())
    gitB.add(pomFile)
    Assert.assertEquals(Seq("A pom.xml", "?? sub/"), gitB.localChanges())
    testFailIllegal("only (pom.xml) changes are allowed => A pom.xml, ?? sub/ => sub/ <= pom.xml, sub/", () ⇒ {
      gitB.localPomChanges()
    })
    val anyFile = testFile(testRepoB, "any.xml")

    Assert.assertEquals(Seq("A pom.xml", "?? any.xml", "?? sub/"), gitB.localChanges())

    testFailIllegal("only (pom.xml) changes are allowed => A pom.xml, ?? any.xml, ?? sub/ => any.xml, sub/ <= any.xml, pom.xml, sub/", () ⇒ {
      gitB.doCommitPomXmls("fail")
    })

    testFailIllegal("only (any.xml, pom.xml) changes are allowed => A pom.xml, ?? any.xml, ?? sub/ => sub/ <= any.xml, pom.xml, sub/", () ⇒ {
      gitB.doCommitWithFilter("fail", Seq("pom.xml", "any.xml"))
    })

    gitB.add(anyFile)
    val otherFile = testFile(testRepoB, "schoenes Ding") // TODO change "oe" to "ö"
    gitB.add(otherFile)
    val subject = "add " + Seq(pomFile, anyFile, otherFile).map(_.getName).mkString(", ") + "-"
    gitB.commitAll(subject + "\r\n\r\n test")
    Assert.assertEquals(Seq("test", "Change-Id: I..."),
      gitB.commitMessageBody("HEAD").map(_.replaceFirst("[0-9a-f]{40}$", "...")))
    assertMsg(Seq(subject, "", " test"), gitB)

    Assert.assertTrue(gitB.hasChangesToPush)
    Assert.assertFalse(gitB.hasLocalChanges)
    Util.write(pomFile, Seq("a"))
    Util.write(subPomFile, Seq("a"))
    Assert.assertTrue(gitB.hasLocalChanges)

    Assert.assertEquals(Seq("M pom.xml", "M sub/pom.xml"), gitB.localChanges())
    Assert.assertEquals(Seq("pom.xml", "sub/pom.xml"), gitB.localPomChanges())
    gitB.doCommitPomXmls("update pom.xml\n\nSigned-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>")
    Assert.assertEquals(Nil, gitB.localChanges())
    gitB.doTag("1.0.0")
    gitB.doTag("1.0.1")
    Assert.assertEquals(Seq("any.xml", "pom.xml", "schoenes Ding", "sub/pom.xml", "test"), gitB.lsFiles())
    Assert.assertEquals(Seq("master"), gitB.branchNamesLocal())
    assertMsg(Seq("update pom.xml", "", "Signed-off-by: Ishop-Dev-Infra <ishop-dev-infra@novomind.com>"), gitB)

    Util.write(pomFile, Seq("b"))
    Util.write(subPomFile, Seq("b"))
    gitB.doCommitWithFilter("subset\n\ntest", Seq("pom.xml", "any.xml"))
    assertMsg(Seq("subset", "", "test"), gitB)
    Assert.assertEquals(false, gitB.isDetached)
    Assert.assertEquals(Some("master"), gitB.findUpstreamBranch())
    gitB.checkout("HEAD~1")
    Assert.assertEquals(None, gitB.findUpstreamBranch())
    Assert.assertEquals(true, gitB.isDetached)
    gitB.checkout("master")
    Assert.assertEquals(false, gitB.isDetached)
    Assert.assertEquals(true, gitB.isNotDetached)
    gitB.createBranch("feature/test")
    Assert.assertEquals(Seq("feature/test", "master"), gitB.branchNamesLocal())
    gitB.deleteBranch("feature/test")
    testFailIllegal("branch 'test' not found.", () ⇒ {
      gitB.deleteBranch("test")
    })
    Assert.assertEquals(Seq("master"), gitB.branchNamesLocal())
    Util.write(anyFile, Seq("a"))
    try {
      gitB.doCommitPomXmls("update pom.xml")
      Assert.fail()
    } catch {
      case e: IllegalStateException ⇒
        Assert.assertEquals("only (pom.xml) changes are allowed => M any.xml => any.xml <= any.xml", e.getMessage)
    }
    Assert.assertTrue(gitB.hasChangesToPush)
    gitB.pushFor("master", "master")
    gitB.add(anyFile)
    Assert.assertTrue(gitB.hasLocalChanges)
    gitB.commitAll("add " + Seq(anyFile).map(_.getName).mkString(", "))
    testFailIllegal("tag v1.0.0 already exists", () ⇒ {
      gitB.doTag("1.0.0")
    })
    Assert.assertFalse(gitB.hasLocalChanges)
    Assert.assertEquals(Seq("refs/heads/master"), gitB.branchListLocal().map(_.branchName))

    gitB.createBranch("any")
    Assert.assertEquals(Seq("refs/heads/any", "refs/heads/master"), gitB.branchListLocal().map(_.branchName))
    gitB.remoteRemove("origin")
    gitB.remoteAdd("origin", "ssh://none@git-ishop.novomind.com:19418/ishop/user/tstock/sonar-demo")
    val triedUnit = gitB.tryFetchAll()
    if (triedUnit.isFailure && triedUnit.failed.get.getMessage.contains("publickey")) {

      TestHelper.assertException("Nonzero exit value: 128; git --no-pager push -q -u origin master:refs/heads/master; " +
        "git-err: 'Permission denied (publickey).' " +
        "git-err: 'fatal: Could not read from remote repository.' " +
        "git-err: 'Please make sure you have the correct access rights' " +
        "git-err: 'and the repository exists.'",
        classOf[RuntimeException], () ⇒ {
          gitB.pushHeads("master", "master")
        })
    }
  }

  @Test
  def testSplitLineOnBranchlist(): Unit = {

    val master = Sgit.splitLineOnBranchlist("* master 915c1ce40ee979c3739e640e0a86ba68adea8681 Removed ...")
    Assert.assertEquals(Seq("master", "915c1ce40ee979c3739e640e0a86ba68adea8681"), master.take(2))

    val any = Sgit.splitLineOnBranchlist("  bla  74036668cf893e4762ab4ff24ab4de8e44b70e33 Hub: SEE ..")
    Assert.assertEquals(Seq("bla", "74036668cf893e4762ab4ff24ab4de8e44b70e33"), any.take(2))

    val detached = Sgit.splitLineOnBranchlist("* (HEAD detached at 97cbb59ea) 97cbb59ea40aacb4d8acad402bf90890741b0dbe Add ...")
    Assert.assertEquals(Seq("97cbb59ea40aacb4d8acad402bf90890741b0dbe", "97cbb59ea40aacb4d8acad402bf90890741b0dbe"),
      detached.take(2))
  }

}

object SgitTest {
  private[release] val commitMsg = Sgit.findGit(Util.localWork, Util.localWork, checkExisting = true).toPath.resolve(".git/hooks/commit-msg")
  private[release] val hasCommitMsg = Files.exists(commitMsg)

  def workSgit(): Sgit = Sgit(file = Util.localWork, showGitCmd = false, doVerify = hasCommitMsg,
    out = System.out, err = System.err, gitBin = None)
}
