package io.linewise.jobfm.transpiler

import java.nio.file.{Files, Path, Paths}

/* =============================================================================
 * TRANSPILER ENTRY POINT — invoked by the Mill `transpile` task / sourceGenerator.
 *
 * args(0) = absolute path to the verified source  (verify/.../JobModel.scala)
 * args(1) = absolute path to the generated output (.../generated/JobModel.scala)
 * args(2) = a short relative label for the header  (e.g. verify/.../JobModel.scala)
 *
 * Reads the verified source, runs the scalameta transform, writes the generated
 * core, creating parent directories as needed. Idempotent: re-running produces
 * byte-identical output for an unchanged source.
 * ========================================================================== */
object Main {
  def main(args: Array[String]): Unit = {
    require(args.length >= 2, "usage: transpiler <inputFile> <outputFile> [srcLabel]")
    val in: Path  = Paths.get(args(0))
    val out: Path = Paths.get(args(1))
    val label     = if args.length >= 3 then args(2) else in.getFileName.toString
    // optional 4th arg: the production package the generated core lands in.
    val targetPkg = if args.length >= 4 then args(3) else "io.linewise.jobfm.generated"

    val source    = Files.readString(in)
    val generated = Transpiler.transpile(source, label, targetPkg)

    Option(out.getParent).foreach(p => Files.createDirectories(p))
    Files.writeString(out, generated)
    System.err.println(s"[transpiler] $label -> $out (${generated.linesIterator.size} lines)")
  }
}
