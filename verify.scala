// Mill single-file Scala script: ./mill verify.scala [files...]
//
// Runs Stainless formal verification in a Docker container.
// If no files are specified, verifies all of stainless-lib + every pet-store verify
// source (auto-discovered under verify/src/main/scala). The pet store has no INVALID
// counterexample files, so the headline no-args run reports 0 invalid.
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

  // Always rebuild the verifier image so Docker/runtime changes are not hidden
  // behind a stale local tag. (The image layers are cached, so a no-change
  // rebuild is fast.)
  System.err.println(s"Building verifier image '$image'...")
  os.proc("docker", "build", "-t", image, "verify/stainless/").call(
    cwd = projectRoot,
    stdout = os.Inherit,
    stderr = os.Inherit
  )

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
