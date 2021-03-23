package prototest

import caffe2.caffe2.{TensorProto, TensorProtos}
import com.google.protobuf.CodedOutputStream
import org.lmdbjava.{DbiFlags, Env, SeekOp}
import scalapb.descriptors.ScalaType.ByteString
import sun.nio.cs.UTF_8
import torch.torch.TensorDef

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object TestCompile extends App {
  //  val builder = TensorDef.newBuilder
  //  val tensor = TensorDef()withDims(List[Long](1, 2, 3))
  //  tensor.withDims(List[Long](1, 2, 3))
  //  println("Dims: ", tensor.dims)
  val dbPath = "/run/media/zf/Newsmy/mjres/pf4/test/testdb"
  val keyStr = "testkey0"

  def testWrite() {
    val dataProto = TensorProto().withDims(List[Long](1, 3, 3)).withDataType(TensorProto.DataType.INT32).addAllInt32Data(Array[Int](7,7,7,9, 9, 9, 8, 8, 8))
    val actionProto = TensorProto().withDataType(TensorProto.DataType.INT32).addInt32Data(9)
    val rewardProto = TensorProto().withDataType(TensorProto.DataType.FLOAT).addFloatData(-1)
    val tranProtos = TensorProtos().withProtos(List[TensorProto](dataProto, actionProto, rewardProto))

//    val stream = new ByteArrayOutputStream()
//    val output = CodedOutputStream.newInstance(stream)
//    tranProtos.writeTo(output)
//    println("proto len: " + tranProtos.toByteArray.length)

    //  println("bytes: " + output.toString)

    val dbFile = new File(dbPath)
    dbFile.deleteOnExit()
    val env: Env[ByteBuffer] = Env.create().setMapSize((1024 * 1024) * 1000L).setMaxDbs(1).open(dbFile)
    val db = env.openDbi("TEST", DbiFlags.MDB_CREATE)

    val key = ByteBuffer.allocateDirect(128)
    val value = ByteBuffer.allocateDirect(1024)
//    println("Bytearray: " + stream.toByteArray.length)
    value.put(tranProtos.toByteArray).flip()
    key.put(keyStr.getBytes(StandardCharsets.UTF_8)).flip()

    db.put(key, value)

    val parseProto = TensorProtos.parseFrom(tranProtos.toByteArray)
    val parseTensor = parseProto.protos.head
    println("Parse tensor: " + parseTensor.dims)
    println("Parse tensor: " + parseProto.protos(2).dataType)

    db.close()
  }

  def testRead(): Unit = {
    val dbFile = new File(dbPath)
    val env: Env[ByteBuffer] = Env.create().setMapSize((1024 * 1024) * 1000L).setMaxDbs(1).open(dbFile)
    val db = env.openDbi("TEST", DbiFlags.MDB_CREATE)

    val txn = env.txnRead()

    val key = ByteBuffer.allocateDirect(128)
    key.put(keyStr.getBytes(StandardCharsets.UTF_8)).flip()
    val readRc = db.get(txn, key)
    println("Readrc: " + readRc)
    val readBuffer = txn.`val`()
    println("read len: " + readBuffer.getClass + ", " + readBuffer.remaining())

    val buffer = Array.ofDim[Byte](readBuffer.remaining())
    readBuffer.get(buffer)
    println("Get buffer from outside heap")

//    val d = UTF_8.INSTANCE.decode(buffer)
//    println("Decode as string: " + d)
    val protos = TensorProtos.parseFrom(buffer)
    println("Parse buffer")

    val tensors = protos.protos
    tensors.foreach(tensor => {
      println("Tensor dims: " + tensor.dims)
      println("Tensor dataType: " + tensor.dataType)
    })
    println("Tensor data: " + tensors.head.int32Data)



    db.close()
  }

//  testWrite()
  testRead()
}