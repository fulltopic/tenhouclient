// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO2

package caffe2.caffe2_legacy

object Caffe2LegacyProto extends _root_.scalapb.GeneratedFileObject {
  lazy val dependencies: Seq[_root_.scalapb.GeneratedFileObject] = Seq.empty
  lazy val messagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]] =
    Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]](
      caffe2.caffe2_legacy.CaffeDatum
    )
  private lazy val ProtoBytes: Array[Byte] =
      scalapb.Encoding.fromBase64(scala.collection.immutable.Seq(
  """CiBjYWZmZTIvcHJvdG8vY2FmZmUyX2xlZ2FjeS5wcm90bxIGY2FmZmUyIp0CCgpDYWZmZURhdHVtEikKCGNoYW5uZWxzGAEgA
  SgFQg3iPwoSCGNoYW5uZWxzUghjaGFubmVscxIjCgZoZWlnaHQYAiABKAVCC+I/CBIGaGVpZ2h0UgZoZWlnaHQSIAoFd2lkdGgYA
  yABKAVCCuI/BxIFd2lkdGhSBXdpZHRoEh0KBGRhdGEYBCABKAxCCeI/BhIEZGF0YVIEZGF0YRIgCgVsYWJlbBgFIAEoBUIK4j8HE
  gVsYWJlbFIFbGFiZWwSLQoKZmxvYXRfZGF0YRgGIAMoAkIO4j8LEglmbG9hdERhdGFSCWZsb2F0RGF0YRItCgdlbmNvZGVkGAcgA
  SgIOgVmYWxzZUIM4j8JEgdlbmNvZGVkUgdlbmNvZGVkKkoKDUxlZ2FjeVBhZGRpbmcSCgoGTk9UU0VUEAASCQoFVkFMSUQQARIIC
  gRTQU1FEAISGAoUQ0FGRkVfTEVHQUNZX1BPT0xJTkcQAw=="""
      ).mkString)
  lazy val scalaDescriptor: _root_.scalapb.descriptors.FileDescriptor = {
    val scalaProto = com.google.protobuf.descriptor.FileDescriptorProto.parseFrom(ProtoBytes)
    _root_.scalapb.descriptors.FileDescriptor.buildFrom(scalaProto, dependencies.map(_.scalaDescriptor))
  }
  lazy val javaDescriptor: com.google.protobuf.Descriptors.FileDescriptor = {
    val javaProto = com.google.protobuf.DescriptorProtos.FileDescriptorProto.parseFrom(ProtoBytes)
    com.google.protobuf.Descriptors.FileDescriptor.buildFrom(javaProto, Array(
    ))
  }
  @deprecated("Use javaDescriptor instead. In a future version this will refer to scalaDescriptor.", "ScalaPB 0.5.47")
  def descriptor: com.google.protobuf.Descriptors.FileDescriptor = javaDescriptor
}