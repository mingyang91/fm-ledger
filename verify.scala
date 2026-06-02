// Mill single-file Scala script: ./mill verify.scala [files...]
//
// Runs Stainless formal verification in Docker container.
// If no files specified, verifies all stainless-lib + the JobProofs proofs.
//
// EvolutionConflict.scala is the deliberately-INVALID counterexample file (it
// asserts the broken DAG is covered, which is FALSE). It is EXCLUDED from the
// default no-args run so the headline run reports 0 invalid. To see the
// counterexample, pass it explicitly:
//   ./mill verify.scala verify/src/main/scala/io/linewise/jobfm/verify/EvolutionConflict.scala \
//                       verify/src/main/scala/io/linewise/jobfm/verify/JobProofs.scala
//
// OverflowCounterexample.scala is the Tier-1 INVALID companion: an UNGUARDED
// FMInt `+` whose no-overflow precondition cannot be discharged. Also excluded
// from the headline run; to see it fail, pass it with FMTypes:
//   ./mill verify.scala verify/stainless-lib/FMTypes.scala \
//                       verify/src/main/scala/io/linewise/jobfm/verify/OverflowCounterexample.scala
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
            // exclude the intentional INVALID counterexamples from the headline run
            .filter(_.last != "EvolutionConflict.scala")
            .filter(_.last != "OverflowCounterexample.scala")
            .filter(_.last != "ConservationConflict.scala")
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
