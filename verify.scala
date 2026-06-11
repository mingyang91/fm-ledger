// Mill single-file Scala script: ./mill verify.scala [files...]
//
// Runs Stainless formal verification in a Docker container.
// If no files are specified, verifies all of stainless-lib + every verify source
// auto-discovered under verify/src/main/scala.
//
// The Stainless VC cache is persisted to verify/.stainless-cache (gitignored) via the
// /work mount, so structurally-identical VCs are not re-solved across runs: a warm
// re-run reuses every previously-solved VC, and a one-function change re-solves only
// the VCs that actually changed. Delete verify/.stainless-cache to force a cold run.
//
// Examples:
//   ./mill verify.scala
//   ./mill verify.scala verify/stainless-lib/IO.scala

def main(args: String*): Unit =
  val image = "stainless-verify"
  val projectRoot = os.pwd

  // Build the verifier image only if it is not already present (or if STAINLESS_REBUILD is set).
  // Reusing an existing image lets CI restore it from a cache instead of re-downloading and
  // re-installing Stainless + z3 on every run, and makes local re-runs instant. To force a fresh
  // build: change the Dockerfile (CI keys its image cache on verify/stainless/), set
  // STAINLESS_REBUILD=1, or `docker rmi stainless-verify`.
  val imagePresent =
    os.proc("docker", "image", "inspect", image)
      .call(cwd = projectRoot, check = false, stdout = os.Pipe, stderr = os.Pipe)
      .exitCode == 0
  if !imagePresent || sys.env.contains("STAINLESS_REBUILD") then
    System.err.println(s"Building verifier image '$image'...")
    os.proc("docker", "build", "-t", image, "verify/stainless/").call(
      cwd = projectRoot,
      stdout = os.Inherit,
      stderr = os.Inherit
    )
  else
    System.err.println(s"Reusing existing verifier image '$image' (STAINLESS_REBUILD=1 to rebuild).")

  // Resolve files
  val files: Seq[String] =
    if args.nonEmpty then
      args.map: f =>
        val rel = if f.startsWith("verify/") then f.stripPrefix("verify/") else f
        s"/work/$rel"
    else
      val libDir = projectRoot / "verify" / "stainless-lib"
      val libs = os.list(libDir).filter(_.ext == "scala").map(p => s"/work/stainless-lib/${p.last}")
      val verifyBase = projectRoot / "verify" / "src" / "main" / "scala"
      val srcs =
        if os.exists(verifyBase) then
          os.walk(verifyBase)
            .filter(_.ext == "scala")
            .map: p =>
              s"/work/src/main/scala/${p.relativeTo(verifyBase)}"
        else Seq.empty
      libs ++ srcs

  // Persist the Stainless VC cache across runs. The cache dir lives inside the /work
  // mount (host: verify/.stainless-cache), so --cache-dir survives the `--rm` container
  // and the next run reuses it instead of re-solving every VC from scratch.
  val cacheDir = projectRoot / "verify" / ".stainless-cache"
  os.makeDir.all(cacheDir)

  println("=== Stainless Verification ===")
  println(s"Files: ${files.mkString(" ")}")
  println(s"VC cache: $cacheDir (delete to force a cold run)")
  println()

  // Run verification in container. --cache-dir points inside the mounted /work tree so
  // the VC cache persists on the host at verify/.stainless-cache.
  val cmd = Seq("docker", "run", "--rm", "-v", s"$projectRoot/verify:/work", image,
                "--cache-dir=/work/.stainless-cache") ++ files
  val result = os.proc(cmd).call(
    cwd = projectRoot,
    stdout = os.Inherit,
    stderr = os.Inherit,
    check = false
  )

  sys.exit(result.exitCode)
