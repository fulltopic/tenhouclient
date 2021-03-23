package fileprocesstest

import fileprocess.ExtractFiles

import java.io.File
import java.nio.file.{Path, Paths}

object TestExtractFiles extends App {
  def testPath(): Unit = {
    val abPath = "/run/media/zf/Newsmy/mjres/pf4/test/xmls/"
    val entirePath = "mjlog_pf4-20_n1/2009081211gm-0061-0000-184f512b&tw=1.mjlog".replace("mjlog", "xml")
    val entireFileName = abPath + "/" + entirePath
    val file = new File (entireFileName)

    file.getParentFile.mkdirs()
    file.createNewFile()

//    val p = Paths.get(entirePath)
//    val fileName = p.getFileName
//    println(fileName)
//    println(p.getParent)
  }

  def testExtractFiles(): Unit = {
    val zipFilePath = "/run/media/zf/Newsmy/mjres/pf4/zips/mjlog_pf4-20_n6.zip"
    val outputPath = "/run/media/zf/Newsmy/mjres/pf4/xmls/"
    val tester = new ExtractFiles(zipFilePath, outputPath)
    val files = tester.unzip()
    println("-----------------> Test unzip file: " + zipFilePath)

//    files.foreach(entry => {
//      println(entry._1)
//      println(entry._2)
//    })
  }

  def dummy(): Unit = {
    println("Test compile")
  }

//  testPath()
  testExtractFiles()
//  dummy()
}
