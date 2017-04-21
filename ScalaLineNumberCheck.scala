package org.scalaide.core.internal.jdt.model

import scala.tools.eclipse.contribution.weaving.jdt.cfprovider.ILineNumberCheck
import scala.tools.scalap.scalax.rules.scalasig.Attribute
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.tools.scalap.scalax.rules.scalasig.ByteCodeReader
import scala.tools.scalap.scalax.rules.scalasig.ClassFile
import scala.tools.scalap.scalax.rules.scalasig.ClassFileParser

import org.eclipse.core.resources.IMarker
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.debug.core.IJavaBreakpoint
import org.scalaide.logging.HasLogger

class ScalaLineNumberCheck extends ILineNumberCheck with HasLogger {
  override def realClassFile(classFile: IType, breakpoint: IJavaBreakpoint): IType = {
    classFile match {
      case sbt: ScalaClassFile#ScalaBinaryType if sbt.getClassFile.isInstanceOf[ScalaClassFile] =>
        val cf = sbt.getClassFile.asInstanceOf[ScalaClassFile]
        val jClass = ClassFileParser.parse(ByteCode(cf.getBytes))
        val lines = LineNumbersExtractor(jClass)
        val line = breakpoint.getMarker.getAttribute(IMarker.LINE_NUMBER, -1)
        if (lines.exists(_ == line))
          sbt
        else
          sbt
      case _ => classFile
    }
  }
}

object LineNumbersExtractor extends ByteCodeReader {
  import scala.tools.scalap.scalax.rules.scalasig.ClassFileParser._
  val CODE_NAME = "Code"
  val LINE_NUMBER_TABLE = "LineNumberTable"
  def apply(classfile: ClassFile): Seq[Int] = {
    val attributes = classfile.methods.flatMap(_.attributes)
    attributes.collect {
      case attribute if classfile.constant(attribute.nameIndex) == CODE_NAME =>
        val codeInst = expect(code)(attribute.byteCode)
        codeInst
    }.flatMap { codeInst =>
      codeInst.attributes
    }.collect {
      case lineNumberTable if classfile.constant(lineNumberTable.nameIndex) == LINE_NUMBER_TABLE =>
        val lnTables = expect(lineNumberTables)(lineNumberTable.byteCode)
        lnTables
    }.flatten.map(_.lineNumber)
  }
  case class ExceptionTable(startPc: Int, endPc: Int, handlerPc: Int, catchType: Int)
  case class Code(maxStack: Int, maxLocals: Int, code: ByteCode, exceptions: Seq[ExceptionTable], attributes: Seq[Attribute])
  case class LineNumberTable(startPc: Int, lineNumber: Int)

  val exceptionTable = u2 ~ u2 ~ u2 ~ u2 ^~~~^ ExceptionTable
  val exceptionTables = u2 >> exceptionTable.times
  val code = u2 ~ u2 ~ (u4 >> bytes) ~ exceptionTables ~ attributes ^~~~~^ Code
  val lineNumberTable = u2 ~ u2 ^~^ LineNumberTable
  val lineNumberTables = u2 >> lineNumberTable.times
}
