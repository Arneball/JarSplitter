package utils
import java.io.File
import net.lingala.zip4j.core.ZipFile
import java.io.{FileInputStream, FileOutputStream}
import net.lingala.zip4j.util.Zip4jConstants
import net.lingala.zip4j.model.ZipParameters
import com.android.dx.command.dexer.Main
import scala.annotation.tailrec

object ZipSplitter {
  private def mkTemp(name: String = "tmp-ivar") = {
    val file = File.createTempFile(name, "")
    file.delete();
    if(!file.mkdir) throw new Exception("cannot create tmp dir")
    file.deleteOnExit()
    file
  }
  
  type FileSeq = IndexedSeq[File]
  type SeqSeq = IndexedSeq[FileSeq]
  
  private implicit class ListWrapper[T](val list: IndexedSeq[T]) extends AnyVal {
    /** Like a conditional fold combined with drop and take
     * @zero start value for the fold
     * @param cond fold until condition is met
     * @transform yields the new accumulator
     */
    def takeUntil[U](zero: U)(cond: U => Boolean)(transform: (U, T) => U): (IndexedSeq[T], IndexedSeq[T]) = {
      def mkFun: T => Boolean = {
        var acc = zero
        elem: T =>   
          acc = transform(acc, elem)
          cond(acc)
      }
      val index = list.indexWhere(mkFun)
      if(index >= 0) {
        list.take(index) -> list.drop(index)
      } else { 
        list -> IndexedSeq.empty
      }
    }
  }
  
  private def cp(from: File, to: File) = { // let it crash if it does
    if(!to.exists) to.getParentFile.mkdirs
    val source = new FileInputStream(from).getChannel
    val dest = new FileOutputStream(to).getChannel
    dest.transferFrom(source, 0, source.size)
    source.close()
    dest.close()
  }

  lazy val zipParams =  new ZipParameters{
    setCompressionLevel(Zip4jConstants.COMP_DEFLATE)
    setCompressionLevel(Zip4jConstants.DEFLATE_LEVEL_MAXIMUM)
  }
  
  def helper(file: File): FileSeq = transform(file, mkTemp("blaha"))
  
  /** Transform a zip file to several smaller dexed jars*/
  private def transform(file: File, toPath: File) = {
    val tmpClassFiles = mkTemp()
    new ZipFile(file).extractAll(tmpClassFiles.getPath)

    val classFiles = getFilesWithExtension(tmpClassFiles, "class")
    
//    val totalSum = classFiles.map{ _.length }.sum
//    val subJarSize = totalSum / subJars
    val subJarSize = 1024 * 1024 * 6
    
    val bundled = bundleFiles(classFiles, subJarSize + (1024 * 256)) // safety margin
    
    val jars = bundled.zipWithIndex.map{ case (bundle, index) =>
      val innerTemp = mkTemp("out")
      println(s"Creating temporary directory for sub package: $innerTemp")
      bundle.foreach{ file =>
        // remove until first slash and the remove the slash
        val filename = file.getPath.drop(tmpClassFiles.getPath.length)
        cp(file, new File(innerTemp.getPath + filename))
      }

      // Create a target jar file for this bundle
      val tmpZip = new ZipFile(toPath.getAbsolutePath + s"/out$index.jar")
      
      // Foreach folder and file add it to the jar
      val (folders, files) = innerTemp.listFiles.partition(_.isDirectory)
      folders.foreach{ folder => tmpZip.addFolder(folder, zipParams) }
      files.foreach{ file => tmpZip.addFile(file, zipParams) }
      
      // return reference to temp file
      tmpZip.getFile()
    }
    jars
  }
  
  @tailrec private def bundleFiles(files: FileSeq, maxBytes: Long, acc: SeqSeq=IndexedSeq()): SeqSeq = files match {
    case Seq() => acc
    case files =>
      val (abundle, rest) = files.takeUntil(0l){ _ > maxBytes}{ _ + _.length }
      println(s"bundle ${abundle.length}, rest: ${rest.length}")
      bundleFiles(rest, maxBytes, abundle+:acc)
  }
  
  private def getFilesWithExtension(file: File, extension: String): FileSeq = {
    val (dirs, files) = file.listFiles.toIndexedSeq.partition{ _.isDirectory }
    dirs.map{ getFilesWithExtension(_, extension) }.flatten ++
    files.filter{ _.getName.endsWith(extension) }
  }

  def doDex(files: FileSeq): DexRes = {
    val dexedFiles = files.map{ file =>
      val out = file.getAbsoluteFile() + "-dexed.jar"
      val outFile = new File(out)
      Main.main(Array(s"--output=$out", file.getAbsolutePath))
      println("done with " + file)
      outFile
    }
    // zip the remaining stuff
    val unused = mkTemp("shit")
    unused.delete()
    val zipFile = new ZipFile(unused.getAbsolutePath)
    dexedFiles.foreach{ zipFile.addFile(_, zipParams) }
    DexRes(zipFile.getFile)
  }
}

case class DexRes(zipFile: File)