package de.sciss

import java.io.{FileFilter, File}
import collection.immutable.{IndexedSeq => IIdxSeq}

//package object strugatzki {
//  // def file(path: String): File = new File(path)
//
//  private final class FileFilterWrapper(fun: File => Boolean) extends FileFilter {
//    def accept(f: File) = fun(f)
//  }
//
//  //  implicit final class RichFile(val f: File) extends AnyVal {
//  //    def children(filter: File => Boolean = _ => true): IIdxSeq[File] = {
//  //      val arr = f.listFiles(new FileFilterWrapper(filter))
//  //      if (arr == null) Vector.empty // Sssssuckers!
//  //      else arr.toIndexedSeq
//  //    }
//  //
//  //    def name: String = f.getName
//  //
//  //    def /(child: String): File = new File(f, child)
//  //  }
//}
