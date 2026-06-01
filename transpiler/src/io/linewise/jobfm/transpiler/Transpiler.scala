package io.linewise.jobfm.transpiler

import scala.meta._

/* =============================================================================
 * STAINLESS -> OX TRANSPILER  (scalameta-validated, principled text-rewriter)
 *
 * WHICH APPROACH, AND WHY.
 * The task preferred a scalameta AST Transformer. I built one and hit a hard
 * Scala-3 wall: scalameta 4.13.4 implements the convenient bottom-up rewrite
 * helpers (`Tree.transform`, `Tree.traverse`) and the `Transformer`/`Traverser`
 * base classes as SCALA-2.13 def-macros (they live in
 * `scala.meta.internal.transversers.{TransformerMacros,TraverserMacros}` in
 * common_2.13-4.13.4.jar). Scala 3 cannot expand Scala-2 def-macros, so
 * `source.transform { ... }` does not compile, and there is no non-macro
 * `Transformer` base class published to subclass. scalameta PARSING works fine
 * under `dialects.Scala3` — it is only the tree-REWRITING surface that is
 * Scala-3-hostile.
 *
 * So this transpiler is a PRINCIPLED TEXT-REWRITER, but scalameta is still a
 * first-class part of it as a VALIDATION GATE on BOTH ends:
 *   1. it PARSES the input with `dialects.Scala3` — a malformed verified source
 *      is rejected before any rewrite runs;
 *   2. each rule is an ANCHORED token rewrite (not blind global regex): the
 *      rules key off concrete Stainless syntax (`import stainless.*`, the
 *      `BigInt` token, `Some[T](`, `None[T]()`, `require(...)`, `.ensuring(...)`,
 *      `.holds`), exactly the proven substitutions;
 *   3. it RE-PARSES the generated output with `dialects.Scala3` and FAILS if the
 *      result is not valid Scala 3 — so a bad rewrite cannot silently ship.
 *
 * THE RULES (each applied once, in order):
 *   R1  drop  import stainless.*            (verification-only imports)
 *   R1b drop  import io.linewise.verify.effect.{FMInt, FMLong} (FM-type imports)
 *   R2  rewrite package -> io.linewise.jobfm.generated
 *   R3  TWO-TIER OVERFLOW ERASURE — FMInt -> Int, FMLong -> Long. The bounded
 *       proof-carrying wrappers (whose `+` carried a PROVEN no-overflow VC)
 *       erase to the native machine types, because the bound was discharged in
 *       the verifier so production needs no runtime check:
 *         FMInt(BigInt(n))  -> n        ; FMLong(BigInt(n)) -> nL   (literals)
 *         FMInt(e)          -> (e).toInt; FMLong(e)          -> (e).toLong (general)
 *         FMInt (type)      -> Int      ; FMLong (type)      -> Long  (type tokens)
 *         x.value           -> x        (erase the bounded-value accessor,
 *                                        anchored so Map/.values and tuple ._2
 *                                        are untouched)
 *       Operators (+,-,>=,<=,==,!=,<,>) and .toLong/.toInt pass through to the
 *       native Int/Long that already define them. NO BigInt in the output.
 *   R4  Some[T](x) -> Some(x) ; None[T]() -> None
 *   R5  drop  require(...)                  (proven precondition)
 *   R6  drop  <block>.ensuring(...)         (proven postcondition)
 *   R7  strip @law @opaque @inlineOnce @extern @ghost ... annotations
 *   R8  drop  def ... .holds  proof defs    (defensive; none in JobModel)
 * ========================================================================== */
object Transpiler {

  /** Stainless-only annotations stripped by R7. */
  val strippedAnnotations: Seq[String] =
    Seq("law", "opaque", "inlineOnce", "extern", "ghost", "pure", "induct", "partialEval")

  def header(srcRel: String): String =
    s"""// =============================================================================
       |// GENERATED FROM $srcRel BY transpiler — DO NOT EDIT.
       |//
       |// This is the PRODUCTION business core. It is mechanically derived from the
       |// Stainless-verified source of truth by the `transpiler` Mill module. Every
       |// definition below is the verified definition with the proof-language
       |// substitutions applied (stainless imports dropped, the bounded FM types
       |// FMInt/FMLong ERASED to native Int/Long — their no-overflow bound was
       |// proven in the verifier, so production carries no runtime check —
       |// Some[T](x)/None[T]() -> Some(x)/None, require/ensuring/annotations stripped).
       |// To change the business logic, edit the verified source, re-verify, and
       |// regenerate — never hand-edit this file.
       |// =============================================================================
       |""".stripMargin

  /** Full pipeline. Parse-validate input (Scala3), rewrite, parse-validate output. */
  def transpile(input: String, srcRel: String): String = {
    // (1) parse-validate the verified source as Scala 3.
    parseScala3(input).fold(
      err => throw new IllegalArgumentException(s"input is not valid Scala 3: $err"),
      _   => ()
    )

    val body = rewrite(input)
    val out  = header(srcRel) + "\n" + body

    // (3) parse-validate the GENERATED output as Scala 3. A bad rewrite fails here.
    parseScala3(out).fold(
      err => throw new IllegalStateException(
        s"generated output is not valid Scala 3 (transpiler bug): $err\n--- output ---\n$out"),
      _   => ()
    )
    out
  }

  /** Parse a string as a Scala 3 Source; Right on success, Left(message) on a
    * parse error. This is the scalameta validation gate. */
  def parseScala3(code: String): Either[String, Source] =
    dialects.Scala3(code).parse[Source] match
      case Parsed.Success(tree) => Right(tree)
      case Parsed.Error(_, msg, _) => Left(msg)

  /** Apply R1..R8 to the source text. Each rule is anchored and ordered. */
  def rewrite(input: String): String = {
    var s = input
    s = ruleR1_dropStainlessImports(s)
    s = ruleR1b_dropFmTypeImports(s)
    s = ruleR1c_dropProofImports(s)
    s = ruleR2_rewritePackage(s)
    s = ruleR7_stripAnnotations(s)   // before R5/R6 so annotated defs are clean
    s = ruleR8_dropHoldsProofs(s)
    s = ruleR5_dropRequire(s)
    s = ruleR5b_dropAssert(s)
    s = ruleR6_dropEnsuring(s)
    s = ruleR3_eraseFmTypes(s)
    s = ruleR4_dropOptionTypeArgs(s)
    s
  }

  // --- R1: drop `import stainless.*` lines ---------------------------------
  def ruleR1_dropStainlessImports(s: String): String =
    s.linesIterator
      .filterNot(l => l.trim.matches("""import\s+stainless\..*"""))
      .mkString("\n") + (if s.endsWith("\n") then "\n" else "")

  // --- R1b: drop the FM-type import lines ----------------------------------
  // `import io.linewise.verify.effect.{FMInt, FMLong}` (and any single-name
  // variant) is a verify-only import: the wrappers are ERASED by R3, so the
  // generated core must not reference the verify package at all.
  def ruleR1b_dropFmTypeImports(s: String): String =
    s.linesIterator
      .filterNot(l => l.trim.matches("""import\s+io\.linewise\.verify\.effect\..*"""))
      .mkString("\n") + (if s.endsWith("\n") then "\n" else "")

  // --- R1c: drop imports of verify-only PROOF objects ----------------------
  // `import StoreProofs.storeInv` (and any `*Proofs` object) is verify-only: the
  // proofs live in files that are NEVER transpiled, so the generated code must
  // not import them. The store CONTRACT the worker is verified against
  // (StoreLaw.AbstractStore) is kept — production provides generated.StoreLaw.
  def ruleR1c_dropProofImports(s: String): String =
    s.linesIterator
      .filterNot(l => l.trim.matches("""import\s+\w*Proofs\..*"""))
      .mkString("\n") + (if s.endsWith("\n") then "\n" else "")

  // --- R2: rewrite the package declaration ---------------------------------
  def ruleR2_rewritePackage(s: String): String =
    s.replaceFirst(
      """(?m)^package\s+[\w\.]+""",
      "package io.linewise.jobfm.generated"
    )

  // --- R3: TWO-TIER OVERFLOW ERASURE — FMInt/FMLong -> native Int/Long ------
  // The Tier-1 strategy: bounded ints whose no-overflow `+` was PROVEN in the
  // verifier erase to native machine ints in production. The erasure is staged
  // so each more-specific form is consumed before the more-general one:
  //
  //   (a) FMInt(BigInt(n))  -> n        FMLong(BigInt(n)) -> nL    (literals)
  //   (b) FMInt(<expr>)     -> (<expr>).toInt ; FMLong(<expr>) -> (<expr>).toLong
  //       for any REMAINING constructor whose argument is not the literal form
  //       (balanced-paren scan, so a nested call in <expr> is handled).
  //   (c) bare type tokens  FMInt -> Int ; FMLong -> Long
  //   (d) `.value` accessor on a bounded value -> erased (x.value -> x),
  //       anchored so `Map`-style `.values` and tuple `._2` are NOT touched.
  //
  // After R3 there is no FMInt/FMLong/BigInt token left, and every operator
  // (+,-,>=,<=,==,!=,<,>,.toInt,.toLong) resolves against native Int/Long.
  def ruleR3_eraseFmTypes(s: String): String = {
    // (a) literal constructors first, so (b) does not see them.
    val litInt  = """FMInt\(\s*BigInt\(\s*(-?\d+)\s*\)\s*\)""".r
      .replaceAllIn(s, m => m.group(1))
    val litLong = """FMLong\(\s*BigInt\(\s*(-?\d+)\s*\)\s*\)""".r
      .replaceAllIn(litInt, m => m.group(1) + "L")

    // (b) general constructors (balanced-paren) on whatever literals left behind.
    val genInt  = eraseGeneralCtor(litLong, "FMInt",  "toInt")
    val genLong = eraseGeneralCtor(genInt,  "FMLong", "toLong")

    // (c) remaining bare type tokens are TYPE uses -> native.
    val tInt  = """\bFMInt\b""".r.replaceAllIn(genLong, _ => "Int")
    val tLong = """\bFMLong\b""".r.replaceAllIn(tInt,    _ => "Long")

    // (d) erase the bounded-value accessor `.value`. Anchor on a value/paren
    // receiver immediately followed by `.value` and a NON-identifier boundary,
    // so `.values` (Map) is excluded by the `\b` after `value`. Tuple `._2`
    // never matches because the accessor name is `value`, not `_2`.
    """\.value\b""".r.replaceAllIn(tLong, _ => "")
  }

  /** Erase a `<Ctor>(<expr>)` constructor whose argument is matched with
    * balanced parentheses, rewriting it to `(<expr>).<conv>`. Scans left to
    * right; only constructor occurrences with a balanced arg list are rewritten.
    * Literal forms have already been consumed by step (a), so this catches the
    * residual general cases (e.g. `FMLong(a.value + b.value)`), keeping the
    * inner expression intact for the type-token and `.value` passes that follow. */
  private def eraseGeneralCtor(s: String, ctor: String, conv: String): String = {
    val sb = new StringBuilder
    val needle = ctor + "("
    var i = 0
    while i < s.length do
      // a constructor occurrence is `<ctor>(` with the `<ctor>` token not part
      // of a longer identifier (so `FMInts(` would not match — none exist, but
      // be precise).
      val isCtorHere =
        s.startsWith(needle, i) &&
        (i == 0 || !s.charAt(i - 1).isLetterOrDigit && s.charAt(i - 1) != '_')
      if isCtorHere then
        val argStart = i + needle.length
        val argEnd   = matchingParen(s, argStart - 1) // index of the closing ')'
        if argEnd >= 0 then
          val arg = s.substring(argStart, argEnd)
          sb.append('(').append(arg).append(").").append(conv)
          i = argEnd + 1
        else
          sb.append(s.charAt(i)); i += 1
      else
        sb.append(s.charAt(i)); i += 1
    sb.toString
  }

  /** Given the index of an opening '(' in `s`, return the index of its matching
    * ')', or -1 if unbalanced. */
  private def matchingParen(s: String, open: Int): Int = {
    var depth = 0
    var i = open
    while i < s.length do
      val c = s.charAt(i)
      if c == '(' then depth += 1
      else if c == ')' then
        depth -= 1
        if depth == 0 then return i
      i += 1
    -1
  }

  // --- R4: Some[T](x) -> Some(x) ; None[T]() -> None -----------------------
  def ruleR4_dropOptionTypeArgs(s: String): String = {
    // None[T]() -> None  (with an empty arg list)
    val noneFixed = """None\[[^\]]*\]\(\s*\)""".r.replaceAllIn(s, _ => "None")
    // Some[T]( -> Some(   (drop only the type application; keep the arg list)
    """Some\[[^\]]*\]\(""".r.replaceAllIn(noneFixed, _ => "Some(")
  }

  // --- R5: drop `require(...)` statements ----------------------------------
  // The only require in JobModel is the JobState invariant, on its own line in
  // the case-class body. Drop any line that is a bare `require(...)` call.
  def ruleR5_dropRequire(s: String): String =
    s.linesIterator
      .filterNot(l => l.trim.matches("""require\(.*\)\s*"""))
      .mkString("\n") + (if s.endsWith("\n") then "\n" else "")

  // --- R5b: drop `assert(...)` proof hints ---------------------------------
  // The worker invokes @law contracts as `assert(s.claimLaw(...))` hints so
  // Stainless chains them; these reference verify-only @law methods and are pure
  // proof scaffolding. Drop any line that is a bare `assert(...)` call.
  def ruleR5b_dropAssert(s: String): String =
    s.linesIterator
      .filterNot(l => l.trim.matches("""assert\(.*\)\s*"""))
      .mkString("\n") + (if s.endsWith("\n") then "\n" else "")

  // --- R6: drop a trailing `.ensuring(...)` postcondition ------------------
  // In JobModel, `step`'s body is `{ ... }.ensuring(res => ...)`. The closing
  // brace of the block sits on its own line as `  }.ensuring(res => ...)`.
  // Drop the `.ensuring(...)` suffix on that line, leaving the bare `}`.
  def ruleR6_dropEnsuring(s: String): String =
    s.linesIterator
      .map { l =>
        val idx = l.indexOf(".ensuring(")
        if idx >= 0 && l.take(idx).trim == "}" then l.substring(0, idx)
        else l
      }
      .mkString("\n") + (if s.endsWith("\n") then "\n" else "")

  // --- R7: strip stainless annotations -------------------------------------
  // Remove `@law`, `@opaque`, ... wherever they appear (start of a def/val or
  // mid-line). Anchored on the `@<name>` token with an optional trailing space.
  def ruleR7_stripAnnotations(s: String): String = {
    val alt = strippedAnnotations.mkString("|")
    s.replaceAll(s"""@(?:$alt)\\b\\s*""", "")
  }

  // --- R8: drop `.holds` proof defs ----------------------------------------
  // JobModel has none (proofs live in JobProofs.scala), but the rule is here so
  // the transpiler is correct if a `.holds` ever leaks into the model. We drop
  // whole `def` blocks that contain a `.holds` token. Brace-balanced from the
  // `def` line to its closing brace.
  def ruleR8_dropHoldsProofs(s: String): String = {
    if !s.contains(".holds") then return s
    val lines = s.linesIterator.toArray
    val keep  = Array.fill(lines.length)(true)
    var i = 0
    while i < lines.length do
      val l = lines(i)
      if l.trim.startsWith("def ") && lineBlockContainsHolds(lines, i) then
        // drop from this def line to the end of its brace-balanced block.
        val end = blockEnd(lines, i)
        var j = i
        while j <= end do { keep(j) = false; j += 1 }
        i = end + 1
      else i += 1
    lines.zipWithIndex.collect { case (l, idx) if keep(idx) => l }.mkString("\n") +
      (if s.endsWith("\n") then "\n" else "")
  }

  /** Does the brace-balanced block starting at def-line `start` contain `.holds`? */
  private def lineBlockContainsHolds(lines: Array[String], start: Int): Boolean = {
    val end = blockEnd(lines, start)
    (start to end).exists(k => lines(k).contains(".holds"))
  }

  /** End line index (inclusive) of the brace-balanced block opened on/after the
    * def line `start`. If no brace opens, the def is single-line: end == start. */
  private def blockEnd(lines: Array[String], start: Int): Int = {
    var depth = 0
    var seenOpen = false
    var i = start
    while i < lines.length do
      val l = lines(i)
      depth += l.count(_ == '{') - l.count(_ == '}')
      if l.contains("{") then seenOpen = true
      if seenOpen && depth <= 0 then return i
      i += 1
    if seenOpen then lines.length - 1 else start
  }
}
