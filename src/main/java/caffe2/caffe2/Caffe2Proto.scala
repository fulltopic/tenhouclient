// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO2

package caffe2.caffe2

object Caffe2Proto extends _root_.scalapb.GeneratedFileObject {
  lazy val dependencies: Seq[_root_.scalapb.GeneratedFileObject] = Seq.empty
  lazy val messagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]] =
    Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]](
      caffe2.caffe2.ExternalDataProto,
      caffe2.caffe2.TensorProto,
      caffe2.caffe2.QTensorProto,
      caffe2.caffe2.TensorProtos,
      caffe2.caffe2.TensorShape,
      caffe2.caffe2.TensorShapes,
      caffe2.caffe2.TensorBoundShape,
      caffe2.caffe2.TensorBoundShapes,
      caffe2.caffe2.AOTConfig,
      caffe2.caffe2.Argument,
      caffe2.caffe2.DeviceOption,
      caffe2.caffe2.OperatorDef,
      caffe2.caffe2.MapFieldEntry,
      caffe2.caffe2.BackendOptions,
      caffe2.caffe2.PartitionInfo,
      caffe2.caffe2.NetDef,
      caffe2.caffe2.ExecutionStep,
      caffe2.caffe2.PlanDef,
      caffe2.caffe2.BlobProto,
      caffe2.caffe2.DBReaderProto
    )
  private lazy val ProtoBytes: Array[Byte] =
      scalapb.Encoding.fromBase64(scala.collection.immutable.Seq(
  """ChljYWZmZTIvcHJvdG8vY2FmZmUyLnByb3RvEgZjYWZmZTIi4AIKEUV4dGVybmFsRGF0YVByb3RvEmgKC3NvdXJjZV90eXBlG
  AEgASgOMiQuY2FmZmUyLkV4dGVybmFsRGF0YVByb3RvLlNvdXJjZVR5cGU6EElOTElORV9DT05UQUlORVJCD+I/DBIKc291cmNlV
  HlwZVIKc291cmNlVHlwZRIqCglyZWNvcmRfaWQYAiABKAlCDeI/ChIIcmVjb3JkSWRSCHJlY29yZElkEjAKC3JlY29yZF9zaXplG
  AUgASgEQg/iPwwSCnJlY29yZFNpemVSCnJlY29yZFNpemUSJgoGb2Zmc2V0GAMgASgDOgEwQgviPwgSBm9mZnNldFIGb2Zmc2V0E
  iYKB3N0cmlkZXMYBCADKANCDOI/CRIHc3RyaWRlc1IHc3RyaWRlcyIzCgpTb3VyY2VUeXBlEhQKEElOTElORV9DT05UQUlORVIQA
  BIPCgtTSU1QTEVfRklMRRABIvIICgtUZW5zb3JQcm90bxIdCgRkaW1zGAEgAygDQgniPwYSBGRpbXNSBGRpbXMSTwoJZGF0YV90e
  XBlGAIgASgOMhwuY2FmZmUyLlRlbnNvclByb3RvLkRhdGFUeXBlOgVGTE9BVEIN4j8KEghkYXRhVHlwZVIIZGF0YVR5cGUSWwoMc
  3RvcmFnZV90eXBlGAwgASgOMh8uY2FmZmUyLlRlbnNvclByb3RvLlN0b3JhZ2VUeXBlOgVUWVBFREIQ4j8NEgtzdG9yYWdlVHlwZ
  VILc3RvcmFnZVR5cGUSLwoKZmxvYXRfZGF0YRgDIAMoAkIQEAHiPwsSCWZsb2F0RGF0YVIJZmxvYXREYXRhEi8KCmludDMyX2Rhd
  GEYBCADKAVCEBAB4j8LEglpbnQzMkRhdGFSCWludDMyRGF0YRIqCglieXRlX2RhdGEYBSABKAxCDeI/ChIIYnl0ZURhdGFSCGJ5d
  GVEYXRhEjAKC3N0cmluZ19kYXRhGAYgAygMQg/iPwwSCnN0cmluZ0RhdGFSCnN0cmluZ0RhdGESMgoLZG91YmxlX2RhdGEYCSADK
  AFCERAB4j8MEgpkb3VibGVEYXRhUgpkb3VibGVEYXRhEi8KCmludDY0X2RhdGEYCiADKANCEBAB4j8LEglpbnQ2NERhdGFSCWlud
  DY0RGF0YRInCghyYXdfZGF0YRgNIAEoDEIM4j8JEgdyYXdEYXRhUgdyYXdEYXRhElEKDWV4dGVybmFsX2RhdGEYDiABKAsyGS5jY
  WZmZTIuRXh0ZXJuYWxEYXRhUHJvdG9CEeI/DhIMZXh0ZXJuYWxEYXRhUgxleHRlcm5hbERhdGESHQoEbmFtZRgHIAEoCUIJ4j8GE
  gRuYW1lUgRuYW1lEkwKDWRldmljZV9kZXRhaWwYCCABKAsyFC5jYWZmZTIuRGV2aWNlT3B0aW9uQhHiPw4SDGRldmljZURldGFpb
  FIMZGV2aWNlRGV0YWlsEkMKB3NlZ21lbnQYCyABKAsyGy5jYWZmZTIuVGVuc29yUHJvdG8uU2VnbWVudEIM4j8JEgdzZWdtZW50U
  gdzZWdtZW50GkcKB1NlZ21lbnQSIAoFYmVnaW4YASACKANCCuI/BxIFYmVnaW5SBWJlZ2luEhoKA2VuZBgCIAIoA0II4j8FEgNlb
  mRSA2VuZCK4AQoIRGF0YVR5cGUSDQoJVU5ERUZJTkVEEAASCQoFRkxPQVQQARIJCgVJTlQzMhACEggKBEJZVEUQAxIKCgZTVFJJT
  kcQBBIICgRCT09MEAUSCQoFVUlOVDgQBhIICgRJTlQ4EAcSCgoGVUlOVDE2EAgSCQoFSU5UMTYQCRIJCgVJTlQ2NBAKEgsKB0ZMT
  0FUMTYQDBIKCgZET1VCTEUQDRIXChNaRVJPX0NPTExJU0lPTl9IQVNIEA4iPwoLU3RvcmFnZVR5cGUSCQoFVFlQRUQQARIHCgNSQ
  VcQAhIMCghFWFRFUk5BTBADEg4KCk5PX0NPTlRFTlQQBCKBBAoMUVRlbnNvclByb3RvEh0KBGRpbXMYASADKANCCeI/BhIEZGltc
  1IEZGltcxIsCglwcmVjaXNpb24YAiACKAVCDuI/CxIJcHJlY2lzaW9uUglwcmVjaXNpb24SIAoFc2NhbGUYAyACKAFCCuI/BxIFc
  2NhbGVSBXNjYWxlEh0KBGJpYXMYBCACKAFCCeI/BhIEYmlhc1IEYmlhcxIqCglpc19zaWduZWQYBSACKAhCDeI/ChIIaXNTaWduZ
  WRSCGlzU2lnbmVkEh8KBGRhdGEYBiADKAVCCxAB4j8GEgRkYXRhUgRkYXRhEh0KBG5hbWUYByABKAlCCeI/BhIEbmFtZVIEbmFtZ
  RJPCglkYXRhX3R5cGUYCCABKA4yHC5jYWZmZTIuVGVuc29yUHJvdG8uRGF0YVR5cGU6BUlOVDMyQg3iPwoSCGRhdGFUeXBlUghkY
  XRhVHlwZRIjCgZzY2FsZXMYCSADKAFCC+I/CBIGc2NhbGVzUgZzY2FsZXMSIwoGYmlhc2VzGAogAygBQgviPwgSBmJpYXNlc1IGY
  mlhc2VzEh0KBGF4aXMYCyABKAVCCeI/BhIEYXhpc1IEYXhpcxI9Cg1pc19tdWx0aXBhcmFtGAwgASgIOgVmYWxzZUIR4j8OEgxpc
  011bHRpcGFyYW1SDGlzTXVsdGlwYXJhbSJICgxUZW5zb3JQcm90b3MSOAoGcHJvdG9zGAEgAygLMhMuY2FmZmUyLlRlbnNvclByb
  3RvQgviPwgSBnByb3Rvc1IGcHJvdG9zIpACCgtUZW5zb3JTaGFwZRIdCgRkaW1zGAEgAygDQgniPwYSBGRpbXNSBGRpbXMSTwoJZ
  GF0YV90eXBlGAIgASgOMhwuY2FmZmUyLlRlbnNvclByb3RvLkRhdGFUeXBlOgVGTE9BVEIN4j8KEghkYXRhVHlwZVIIZGF0YVR5c
  GUSMwoMdW5rbm93bl9kaW1zGAMgAygFQhDiPw0SC3Vua25vd25EaW1zUgt1bmtub3duRGltcxI9Cg11bmtub3duX3NoYXBlGAQgA
  SgIOgVmYWxzZUIR4j8OEgx1bmtub3duU2hhcGVSDHVua25vd25TaGFwZRIdCgRuYW1lGAUgASgJQgniPwYSBG5hbWVSBG5hbWUiS
  AoMVGVuc29yU2hhcGVzEjgKBnNoYXBlcxgBIAMoCzITLmNhZmZlMi5UZW5zb3JTaGFwZUIL4j8IEgZzaGFwZXNSBnNoYXBlcyKEA
  woQVGVuc29yQm91bmRTaGFwZRI1CgVzaGFwZRgBIAEoCzITLmNhZmZlMi5UZW5zb3JTaGFwZUIK4j8HEgVzaGFwZVIFc2hhcGUSS
  QoIZGltX3R5cGUYAiADKA4yIC5jYWZmZTIuVGVuc29yQm91bmRTaGFwZS5EaW1UeXBlQgziPwkSB2RpbVR5cGVSB2RpbVR5cGUSH
  QoEbmFtZRgDIAEoCUIJ4j8GEgRuYW1lUgRuYW1lEjcKDnNoYXBlX2lzX2ZpbmFsGAQgASgIQhHiPw4SDHNoYXBlSXNGaW5hbFIMc
  2hhcGVJc0ZpbmFsIpUBCgdEaW1UeXBlEgsKB1VOS05PV04QABIMCghDT05TVEFOVBABEgkKBUJBVENIEAISGAoUQkFUQ0hfT0ZfR
  kVBVFVSRV9NQVgQAxIgChxCQVRDSF9PRl9GRUFUVVJFX01BWF9ERUZBVUxUEAQSDwoLRkVBVFVSRV9NQVgQBRIXChNGRUFUVVJFX
  01BWF9ERUZBVUxUEAYixwEKEVRlbnNvckJvdW5kU2hhcGVzEj0KBnNoYXBlcxgBIAMoCzIYLmNhZmZlMi5UZW5zb3JCb3VuZFNoY
  XBlQgviPwgSBnNoYXBlc1IGc2hhcGVzEjcKDm1heF9iYXRjaF9zaXplGAIgASgDQhHiPw4SDG1heEJhdGNoU2l6ZVIMbWF4QmF0Y
  2hTaXplEjoKD21heF9mZWF0dXJlX2xlbhgDIAEoA0IS4j8PEg1tYXhGZWF0dXJlTGVuUg1tYXhGZWF0dXJlTGVuIrwBCglBT1RDb
  25maWcSNwoObWF4X2JhdGNoX3NpemUYASACKANCEeI/DhIMbWF4QmF0Y2hTaXplUgxtYXhCYXRjaFNpemUSMQoMbWF4X3NlcV9za
  XplGAIgAigDQg/iPwwSCm1heFNlcVNpemVSCm1heFNlcVNpemUSQwoSaW5fYmF0Y2hfYnJvYWRjYXN0GAMgAigIQhXiPxISEGluQ
  mF0Y2hCcm9hZGNhc3RSEGluQmF0Y2hCcm9hZGNhc3Qi1QMKCEFyZ3VtZW50Eh0KBG5hbWUYASABKAlCCeI/BhIEbmFtZVIEbmFtZ
  RIUCgFmGAIgASgCQgbiPwMSAWZSAWYSFAoBaRgDIAEoA0IG4j8DEgFpUgFpEhQKAXMYBCABKAxCBuI/AxIBc1IBcxIpCgF0GAogA
  SgLMhMuY2FmZmUyLlRlbnNvclByb3RvQgbiPwMSAXRSAXQSJAoBbhgIIAEoCzIOLmNhZmZlMi5OZXREZWZCBuI/AxIBblIBbhIjC
  gZmbG9hdHMYBSADKAJCC+I/CBIGZmxvYXRzUgZmbG9hdHMSHQoEaW50cxgGIAMoA0IJ4j8GEgRpbnRzUgRpbnRzEiYKB3N0cmluZ
  3MYByADKAxCDOI/CRIHc3RyaW5nc1IHc3RyaW5ncxI7Cgd0ZW5zb3JzGAsgAygLMhMuY2FmZmUyLlRlbnNvclByb3RvQgziPwkSB
  3RlbnNvcnNSB3RlbnNvcnMSLQoEbmV0cxgJIAMoCzIOLmNhZmZlMi5OZXREZWZCCeI/BhIEbmV0c1IEbmV0cxI/CghxdGVuc29yc
  xgMIAMoCzIULmNhZmZlMi5RVGVuc29yUHJvdG9CDeI/ChIIcXRlbnNvcnNSCHF0ZW5zb3JzIq8CCgxEZXZpY2VPcHRpb24SMwoLZ
  GV2aWNlX3R5cGUYASABKAU6ATBCD+I/DBIKZGV2aWNlVHlwZVIKZGV2aWNlVHlwZRIqCglkZXZpY2VfaWQYAiABKAVCDeI/ChIIZ
  GV2aWNlSWRSCGRldmljZUlkEjAKC3JhbmRvbV9zZWVkGAMgASgNQg/iPwwSCnJhbmRvbVNlZWRSCnJhbmRvbVNlZWQSKgoJbm9kZ
  V9uYW1lGAQgASgJQg3iPwoSCG5vZGVOYW1lUghub2RlTmFtZRIxCgxudW1hX25vZGVfaWQYBSABKAVCD+I/DBIKbnVtYU5vZGVJZ
  FIKbnVtYU5vZGVJZBItCgpleHRyYV9pbmZvGAYgAygJQg7iPwsSCWV4dHJhSW5mb1IJZXh0cmFJbmZvIq4ECgtPcGVyYXRvckRlZ
  hIgCgVpbnB1dBgBIAMoCUIK4j8HEgVpbnB1dFIFaW5wdXQSIwoGb3V0cHV0GAIgAygJQgviPwgSBm91dHB1dFIGb3V0cHV0Eh0KB
  G5hbWUYAyABKAlCCeI/BhIEbmFtZVIEbmFtZRIdCgR0eXBlGAQgASgJQgniPwYSBHR5cGVSBHR5cGUSLAoDYXJnGAUgAygLMhAuY
  2FmZmUyLkFyZ3VtZW50QgjiPwUSA2FyZ1IDYXJnEkwKDWRldmljZV9vcHRpb24YBiABKAsyFC5jYWZmZTIuRGV2aWNlT3B0aW9uQ
  hHiPw4SDGRldmljZU9wdGlvblIMZGV2aWNlT3B0aW9uEiMKBmVuZ2luZRgHIAEoCUIL4j8IEgZlbmdpbmVSBmVuZ2luZRI2Cg1jb
  250cm9sX2lucHV0GAggAygJQhHiPw4SDGNvbnRyb2xJbnB1dFIMY29udHJvbElucHV0Ej4KDmlzX2dyYWRpZW50X29wGAkgASgIO
  gVmYWxzZUIR4j8OEgxpc0dyYWRpZW50T3BSDGlzR3JhZGllbnRPcBItCgpkZWJ1Z19pbmZvGAogASgJQg7iPwsSCWRlYnVnSW5mb
  1IJZGVidWdJbmZvEiMKBmRvbWFpbhgLIAEoCUIL4j8IEgZkb21haW5SBmRvbWFpbhItCgpvcF92ZXJzaW9uGAwgASgDQg7iPwsSC
  W9wVmVyc2lvblIJb3BWZXJzaW9uIkcKDU1hcEZpZWxkRW50cnkSGgoDa2V5GAEgAigJQgjiPwUSA2tleVIDa2V5EhoKA3ZhbBgCI
  AIoCUII4j8FEgN2YWxSA3ZhbCKBAQoOQmFja2VuZE9wdGlvbnMSMwoMYmFja2VuZF9uYW1lGAEgAigJQhDiPw0SC2JhY2tlbmROY
  W1lUgtiYWNrZW5kTmFtZRI6CgZvcHRpb24YAiADKAsyFS5jYWZmZTIuTWFwRmllbGRFbnRyeUIL4j8IEgZvcHRpb25SBm9wdGlvb
  iLfAQoNUGFydGl0aW9uSW5mbxIdCgRuYW1lGAEgAigJQgniPwYSBG5hbWVSBG5hbWUSKgoJZGV2aWNlX2lkGAIgAygFQg3iPwoSC
  GRldmljZUlkUghkZXZpY2VJZBItCgpleHRyYV9pbmZvGAMgASgJQg7iPwsSCWV4dHJhSW5mb1IJZXh0cmFJbmZvElQKD2JhY2tlb
  mRfb3B0aW9ucxgEIAMoCzIWLmNhZmZlMi5CYWNrZW5kT3B0aW9uc0IT4j8QEg5iYWNrZW5kT3B0aW9uc1IOYmFja2VuZE9wdGlvb
  nMi7QMKBk5ldERlZhIdCgRuYW1lGAEgASgJQgniPwYSBG5hbWVSBG5hbWUSLAoCb3AYAiADKAsyEy5jYWZmZTIuT3BlcmF0b3JEZ
  WZCB+I/BBICb3BSAm9wEh0KBHR5cGUYAyABKAlCCeI/BhIEdHlwZVIEdHlwZRIwCgtudW1fd29ya2VycxgEIAEoBUIP4j8MEgpud
  W1Xb3JrZXJzUgpudW1Xb3JrZXJzEkwKDWRldmljZV9vcHRpb24YBSABKAsyFC5jYWZmZTIuRGV2aWNlT3B0aW9uQhHiPw4SDGRld
  mljZU9wdGlvblIMZGV2aWNlT3B0aW9uEiwKA2FyZxgGIAMoCzIQLmNhZmZlMi5Bcmd1bWVudEII4j8FEgNhcmdSA2FyZxI5Cg5le
  HRlcm5hbF9pbnB1dBgHIAMoCUIS4j8PEg1leHRlcm5hbElucHV0Ug1leHRlcm5hbElucHV0EjwKD2V4dGVybmFsX291dHB1dBgII
  AMoCUIT4j8QEg5leHRlcm5hbE91dHB1dFIOZXh0ZXJuYWxPdXRwdXQSUAoOcGFydGl0aW9uX2luZm8YCSADKAsyFS5jYWZmZTIuU
  GFydGl0aW9uSW5mb0IS4j8PEg1wYXJ0aXRpb25JbmZvUg1wYXJ0aXRpb25JbmZvIu4FCg1FeGVjdXRpb25TdGVwEh0KBG5hbWUYA
  SABKAlCCeI/BhIEbmFtZVIEbmFtZRI9CgdzdWJzdGVwGAIgAygLMhUuY2FmZmUyLkV4ZWN1dGlvblN0ZXBCDOI/CRIHc3Vic3Rlc
  FIHc3Vic3RlcBImCgduZXR3b3JrGAMgAygJQgziPwkSB25ldHdvcmtSB25ldHdvcmsSJwoIbnVtX2l0ZXIYBCABKANCDOI/CRIHb
  nVtSXRlclIHbnVtSXRlchJBChBjcml0ZXJpYV9uZXR3b3JrGAUgASgJQhYYAeI/ERIPY3JpdGVyaWFOZXR3b3JrUg9jcml0ZXJpY
  U5ldHdvcmsSLQoKcmVwb3J0X25ldBgHIAEoCUIO4j8LEglyZXBvcnROZXRSCXJlcG9ydE5ldBI8Cg9yZXBvcnRfaW50ZXJ2YWwYC
  CABKAVCE+I/EBIOcmVwb3J0SW50ZXJ2YWxSDnJlcG9ydEludGVydmFsEjEKDHJ1bl9ldmVyeV9tcxgLIAEoA0IP4j8MEgpydW5Fd
  mVyeU1zUgpydW5FdmVyeU1zEkgKE2NvbmN1cnJlbnRfc3Vic3RlcHMYBiABKAhCF+I/FBISY29uY3VycmVudFN1YnN0ZXBzUhJjb
  25jdXJyZW50U3Vic3RlcHMSPQoQc2hvdWxkX3N0b3BfYmxvYhgJIAEoCUIT4j8QEg5zaG91bGRTdG9wQmxvYlIOc2hvdWxkU3Rvc
  EJsb2ISKgoJb25seV9vbmNlGAogASgIQg3iPwoSCG9ubHlPbmNlUghvbmx5T25jZRI/ChBjcmVhdGVfd29ya3NwYWNlGAwgASgIQ
  hTiPxESD2NyZWF0ZVdvcmtzcGFjZVIPY3JlYXRlV29ya3NwYWNlElUKGG51bV9jb25jdXJyZW50X2luc3RhbmNlcxgNIAEoBUIb4
  j8YEhZudW1Db25jdXJyZW50SW5zdGFuY2VzUhZudW1Db25jdXJyZW50SW5zdGFuY2VzIrIBCgdQbGFuRGVmEh0KBG5hbWUYASABK
  AlCCeI/BhIEbmFtZVIEbmFtZRI2CgduZXR3b3JrGAIgAygLMg4uY2FmZmUyLk5ldERlZkIM4j8JEgduZXR3b3JrUgduZXR3b3JrE
  lAKDmV4ZWN1dGlvbl9zdGVwGAMgAygLMhUuY2FmZmUyLkV4ZWN1dGlvblN0ZXBCEuI/DxINZXhlY3V0aW9uU3RlcFINZXhlY3V0a
  W9uU3RlcCLtAgoJQmxvYlByb3RvEh0KBG5hbWUYASABKAlCCeI/BhIEbmFtZVIEbmFtZRIdCgR0eXBlGAIgASgJQgniPwYSBHR5c
  GVSBHR5cGUSOAoGdGVuc29yGAMgASgLMhMuY2FmZmUyLlRlbnNvclByb3RvQgviPwgSBnRlbnNvclIGdGVuc29yEiYKB2NvbnRlb
  nQYBCABKAxCDOI/CRIHY29udGVudFIHY29udGVudBI8CgdxdGVuc29yGAUgASgLMhQuY2FmZmUyLlFUZW5zb3JQcm90b0IM4j8JE
  gdxdGVuc29yUgdxdGVuc29yEkMKEmNvbnRlbnRfbnVtX2NodW5rcxgGIAEoBUIV4j8SEhBjb250ZW50TnVtQ2h1bmtzUhBjb250Z
  W50TnVtQ2h1bmtzEj0KEGNvbnRlbnRfY2h1bmtfaWQYByABKAVCE+I/EBIOY29udGVudENodW5rSWRSDmNvbnRlbnRDaHVua0lkI
  pUBCg1EQlJlYWRlclByb3RvEh0KBG5hbWUYASABKAlCCeI/BhIEbmFtZVIEbmFtZRIjCgZzb3VyY2UYAiABKAlCC+I/CBIGc291c
  mNlUgZzb3VyY2USJAoHZGJfdHlwZRgDIAEoCUIL4j8IEgZkYlR5cGVSBmRiVHlwZRIaCgNrZXkYBCABKAlCCOI/BRIDa2V5UgNrZ
  Xkq3wEKD0RldmljZVR5cGVQcm90bxINCglQUk9UT19DUFUQABIOCgpQUk9UT19DVURBEAESEAoMUFJPVE9fTUtMRE5OEAISEAoMU
  FJPVE9fT1BFTkdMEAMSEAoMUFJPVE9fT1BFTkNMEAQSDwoLUFJPVE9fSURFRVAQBRINCglQUk9UT19ISVAQBhIOCgpQUk9UT19GU
  EdBEAcSDwoLUFJPVE9fTVNOUFUQCBINCglQUk9UT19YTEEQCRInCiNQUk9UT19DT01QSUxFX1RJTUVfTUFYX0RFVklDRV9UWVBFU
  xAK"""
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