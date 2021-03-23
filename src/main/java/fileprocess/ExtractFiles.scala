package fileprocess

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, FileInputStream, PrintWriter}
import java.util.zip.{GZIPInputStream, ZipInputStream}

class ExtractFiles(inputFileName: String, outputDir: String) {
  def parseZip(zis: ZipInputStream, files: scala.collection.mutable.Map[String, String]): Unit = {
    var ze = zis.getNextEntry
    while (ze != null) {
      val fileName = ze.getName
      val gisBuffer = new ByteArrayOutputStream()
      val buffer = new Array[Byte](1024)

      var len = zis.read(buffer)
      while (len > 0) {
        gisBuffer.write(buffer, 0, len)
        len = zis.read(buffer)
      }

      val gis = new GZIPInputStream(new ByteArrayInputStream(gisBuffer.toByteArray))
      val outputBuffer = new ByteArrayOutputStream()

      len = gis.read(buffer)
      while (len > 0) {
        outputBuffer.write(buffer, 0, len)
        len = gis.read(buffer)
      }
      files += (fileName -> genXml(outputBuffer.toString, fileName))

      outputBuffer.close()
      gisBuffer.close()
      gis.close()

//      parseZip(zis, files)
      ze = zis.getNextEntry
    }
  }

  def unzip(): scala.collection.mutable.Map[String, String] = {
    val zis: ZipInputStream = new ZipInputStream(new FileInputStream(inputFileName))
    var files = scala.collection.mutable.Map[String, String]()
//    files.foreach(println)

    parseZip(zis, files)

    files
  }

//1. new directory 2. new file name 3. return new name
  def genXml(content: String, fileName: String): String = {
    val sceneContent = content.replaceAll("<INIT","\n</SCENE>\n<SCENE>\n<INIT")
    val logContent = sceneContent.replaceAll("</mjloggm>", "\n</SCENE>\n</mjloggm>")
    val rmContent = logContent.replaceFirst("</SCENE>", "")

    val newFileName = outputDir + "/" + fileName.replace("mjlog", "xml")
    val file = new File(newFileName)
    file.getParentFile.mkdirs()
    file.createNewFile()

    val writer = new PrintWriter(file)
    writer.write(rmContent)
    writer.close()

    newFileName
  }

}
