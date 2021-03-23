// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO2

package caffe2.metanet

object MetanetProto extends _root_.scalapb.GeneratedFileObject {
  lazy val dependencies: Seq[_root_.scalapb.GeneratedFileObject] = Seq(
    caffe2.caffe2.Caffe2Proto
  )
  lazy val messagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]] =
    Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]](
      caffe2.metanet.ModelInfo,
      caffe2.metanet.BlobsMap,
      caffe2.metanet.NetsMap,
      caffe2.metanet.PlansMap,
      caffe2.metanet.StringMap,
      caffe2.metanet.MetaNetDef
    )
  private lazy val ProtoBytes: Array[Byte] =
      scalapb.Encoding.fromBase64(scala.collection.immutable.Seq(
  """ChpjYWZmZTIvcHJvdG8vbWV0YW5ldC5wcm90bxIGY2FmZmUyGhljYWZmZTIvcHJvdG8vY2FmZmUyLnByb3RvIoACCglNb2Rlb
  EluZm8SJgoHcHJvamVjdBgBIAEoCUIM4j8JEgdwcm9qZWN0Ugdwcm9qZWN0Ei8KCm1vZGVsQ2xhc3MYAiABKAlCD+I/DBIKbW9kZ
  WxDbGFzc1IKbW9kZWxDbGFzcxImCgd2ZXJzaW9uGAMgASgJQgziPwkSB3ZlcnNpb25SB3ZlcnNpb24SSgoNcHJlZGljdG9yVHlwZ
  RgEIAEoCToQU0lOR0xFX1BSRURJQ1RPUkIS4j8PEg1wcmVkaWN0b3JUeXBlUg1wcmVkaWN0b3JUeXBlEiYKB21vZGVsSWQYBSABK
  AlCDOI/CRIHbW9kZWxJZFIHbW9kZWxJZCJICghCbG9ic01hcBIaCgNrZXkYASACKAlCCOI/BRIDa2V5UgNrZXkSIAoFdmFsdWUYA
  iADKAlCCuI/BxIFdmFsdWVSBXZhbHVlIlcKB05ldHNNYXASGgoDa2V5GAEgAigJQgjiPwUSA2tleVIDa2V5EjAKBXZhbHVlGAIgA
  igLMg4uY2FmZmUyLk5ldERlZkIK4j8HEgV2YWx1ZVIFdmFsdWUiWQoIUGxhbnNNYXASGgoDa2V5GAEgAigJQgjiPwUSA2tleVIDa
  2V5EjEKBXZhbHVlGAIgAigLMg8uY2FmZmUyLlBsYW5EZWZCCuI/BxIFdmFsdWVSBXZhbHVlIkkKCVN0cmluZ01hcBIaCgNrZXkYA
  SACKAlCCOI/BRIDa2V5UgNrZXkSIAoFdmFsdWUYAiACKAlCCuI/BxIFdmFsdWVSBXZhbHVlIqwFCgpNZXRhTmV0RGVmEjIKBWJsb
  2JzGAEgAygLMhAuY2FmZmUyLkJsb2JzTWFwQgriPwcSBWJsb2JzUgVibG9icxIuCgRuZXRzGAIgAygLMg8uY2FmZmUyLk5ldHNNY
  XBCCeI/BhIEbmV0c1IEbmV0cxI/Cgltb2RlbEluZm8YAyABKAsyES5jYWZmZTIuTW9kZWxJbmZvQg7iPwsSCW1vZGVsSW5mb1IJb
  W9kZWxJbmZvEjIKBXBsYW5zGAQgAygLMhAuY2FmZmUyLlBsYW5zTWFwQgriPwcSBXBsYW5zUgVwbGFucxJpChdhcHBsaWNhdGlvb
  lNwZWNpZmljSW5mbxgFIAMoCzIRLmNhZmZlMi5TdHJpbmdNYXBCHOI/GRIXYXBwbGljYXRpb25TcGVjaWZpY0luZm9SF2FwcGxpY
  2F0aW9uU3BlY2lmaWNJbmZvEi8KCmJsb2JzT3JkZXIYBiADKAlCD+I/DBIKYmxvYnNPcmRlclIKYmxvYnNPcmRlchI1CgxwcmVMb
  2FkQmxvYnMYByADKAlCEeI/DhIMcHJlTG9hZEJsb2JzUgxwcmVMb2FkQmxvYnMSXwoRdGVuc29yQm91bmRTaGFwZXMYCCABKAsyG
  S5jYWZmZTIuVGVuc29yQm91bmRTaGFwZXNCFuI/ExIRdGVuc29yQm91bmRTaGFwZXNSEXRlbnNvckJvdW5kU2hhcGVzElAKFXJlc
  XVlc3RPbmx5RW1iZWRkaW5ncxgJIAMoCUIa4j8XEhVyZXF1ZXN0T25seUVtYmVkZGluZ3NSFXJlcXVlc3RPbmx5RW1iZWRkaW5nc
  xI/Cglhb3RDb25maWcYCiABKAsyES5jYWZmZTIuQU9UQ29uZmlnQg7iPwsSCWFvdENvbmZpZ1IJYW90Q29uZmln"""
      ).mkString)
  lazy val scalaDescriptor: _root_.scalapb.descriptors.FileDescriptor = {
    val scalaProto = com.google.protobuf.descriptor.FileDescriptorProto.parseFrom(ProtoBytes)
    _root_.scalapb.descriptors.FileDescriptor.buildFrom(scalaProto, dependencies.map(_.scalaDescriptor))
  }
  lazy val javaDescriptor: com.google.protobuf.Descriptors.FileDescriptor = {
    val javaProto = com.google.protobuf.DescriptorProtos.FileDescriptorProto.parseFrom(ProtoBytes)
    com.google.protobuf.Descriptors.FileDescriptor.buildFrom(javaProto, Array(
      caffe2.caffe2.Caffe2Proto.javaDescriptor
    ))
  }
  @deprecated("Use javaDescriptor instead. In a future version this will refer to scalaDescriptor.", "ScalaPB 0.5.47")
  def descriptor: com.google.protobuf.Descriptors.FileDescriptor = javaDescriptor
}