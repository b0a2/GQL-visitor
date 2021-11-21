package org.b0a2.visitors

import java.io.File

object Main {
  sealed trait FileSystem {
    def getPath: String
    def getFiles: Option[List[FileSystem]]
  }

  case class TreeFile(filepath: String) extends FileSystem {
    override def getPath: String = {
      filepath
    }

    override def getFiles: Option[List[FileSystem]] = {
      val ff = new File(this.filepath)
      if (ff.isDirectory) {
        val nList = ff.listFiles().toList.map(f => TreeFile(f.getPath))
        if (nList.isEmpty) {
          Some(List.empty[FileSystem])
        } else {
          Some(nList)
        }
      } else {
        None
      }
    }
  }

  sealed trait Reader {
    def read(): String
  }

  class Context {
    def addItem(path: String, content: String): Unit = ???
  }

  def visit(context: Context, op: (Context, FileSystem) => Context): (FileSystem) => Context = {
    def visitor(root: FileSystem): Context = {
      val files: Option[List[FileSystem]] = root.getFiles
      files match {
        case None => op(context, root)
        case Some(files) => {
          for(file <- files) {
            visitor(file)
          }
        }
      }
      context
    }
    visitor
  }

  def TreeTracker(ctx: Context, fs: FileSystem): Context  = {
    println("--->", fs.getPath)
    ctx
  }

  def main: Context = {
    val source = new TreeFile("./visitor_dir")
    val ctx = new Context()
    visit(ctx, TreeTracker)(source)
  }
}
