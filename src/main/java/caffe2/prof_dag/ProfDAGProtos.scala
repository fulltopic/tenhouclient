// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO2

package caffe2.prof_dag

/** Operator profiling information.
  *
  * Note: The indices for elements of 'stats' and the indices of
  * 'output_profile' inside each 'stats' are assumed to match the
  * indices of 'op' elements of a corresponding NetDef and the 'output'
  * indices within each 'op'.
  */
@SerialVersionUID(0L)
final case class ProfDAGProtos(
    stats: _root_.scala.Seq[caffe2.prof_dag.ProfDAGProto] = _root_.scala.Seq.empty,
    netName: _root_.scala.Option[_root_.scala.Predef.String] = _root_.scala.None,
    opsStats: _root_.scala.Seq[caffe2.prof_dag.OpProfile] = _root_.scala.Seq.empty,
    unknownFields: _root_.scalapb.UnknownFieldSet = _root_.scalapb.UnknownFieldSet.empty
    ) extends scalapb.GeneratedMessage with scalapb.lenses.Updatable[ProfDAGProtos] {
    @transient
    private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
    private[this] def __computeSerializedValue(): _root_.scala.Int = {
      var __size = 0
      stats.foreach { __item =>
        val __value = __item
        __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
      }
      if (netName.isDefined) {
        val __value = netName.get
        __size += _root_.com.google.protobuf.CodedOutputStream.computeStringSize(2, __value)
      };
      opsStats.foreach { __item =>
        val __value = __item
        __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
      }
      __size += unknownFields.serializedSize
      __size
    }
    override def serializedSize: _root_.scala.Int = {
      var read = __serializedSizeCachedValue
      if (read == 0) {
        read = __computeSerializedValue()
        __serializedSizeCachedValue = read
      }
      read
    }
    def writeTo(`_output__`: _root_.com.google.protobuf.CodedOutputStream): _root_.scala.Unit = {
      stats.foreach { __v =>
        val __m = __v
        _output__.writeTag(1, 2)
        _output__.writeUInt32NoTag(__m.serializedSize)
        __m.writeTo(_output__)
      };
      netName.foreach { __v =>
        val __m = __v
        _output__.writeString(2, __m)
      };
      opsStats.foreach { __v =>
        val __m = __v
        _output__.writeTag(3, 2)
        _output__.writeUInt32NoTag(__m.serializedSize)
        __m.writeTo(_output__)
      };
      unknownFields.writeTo(_output__)
    }
    def clearStats = copy(stats = _root_.scala.Seq.empty)
    def addStats(__vs: caffe2.prof_dag.ProfDAGProto*): ProfDAGProtos = addAllStats(__vs)
    def addAllStats(__vs: Iterable[caffe2.prof_dag.ProfDAGProto]): ProfDAGProtos = copy(stats = stats ++ __vs)
    def withStats(__v: _root_.scala.Seq[caffe2.prof_dag.ProfDAGProto]): ProfDAGProtos = copy(stats = __v)
    def getNetName: _root_.scala.Predef.String = netName.getOrElse("")
    def clearNetName: ProfDAGProtos = copy(netName = _root_.scala.None)
    def withNetName(__v: _root_.scala.Predef.String): ProfDAGProtos = copy(netName = Option(__v))
    def clearOpsStats = copy(opsStats = _root_.scala.Seq.empty)
    def addOpsStats(__vs: caffe2.prof_dag.OpProfile*): ProfDAGProtos = addAllOpsStats(__vs)
    def addAllOpsStats(__vs: Iterable[caffe2.prof_dag.OpProfile]): ProfDAGProtos = copy(opsStats = opsStats ++ __vs)
    def withOpsStats(__v: _root_.scala.Seq[caffe2.prof_dag.OpProfile]): ProfDAGProtos = copy(opsStats = __v)
    def withUnknownFields(__v: _root_.scalapb.UnknownFieldSet) = copy(unknownFields = __v)
    def discardUnknownFields = copy(unknownFields = _root_.scalapb.UnknownFieldSet.empty)
    def getFieldByNumber(__fieldNumber: _root_.scala.Int): _root_.scala.Any = {
      (__fieldNumber: @_root_.scala.unchecked) match {
        case 1 => stats
        case 2 => netName.orNull
        case 3 => opsStats
      }
    }
    def getField(__field: _root_.scalapb.descriptors.FieldDescriptor): _root_.scalapb.descriptors.PValue = {
      _root_.scala.Predef.require(__field.containingMessage eq companion.scalaDescriptor)
      (__field.number: @_root_.scala.unchecked) match {
        case 1 => _root_.scalapb.descriptors.PRepeated(stats.iterator.map(_.toPMessage).toVector)
        case 2 => netName.map(_root_.scalapb.descriptors.PString(_)).getOrElse(_root_.scalapb.descriptors.PEmpty)
        case 3 => _root_.scalapb.descriptors.PRepeated(opsStats.iterator.map(_.toPMessage).toVector)
      }
    }
    def toProtoString: _root_.scala.Predef.String = _root_.scalapb.TextFormat.printToUnicodeString(this)
    def companion = caffe2.prof_dag.ProfDAGProtos
    // @@protoc_insertion_point(GeneratedMessage[caffe2.ProfDAGProtos])
}

object ProfDAGProtos extends scalapb.GeneratedMessageCompanion[caffe2.prof_dag.ProfDAGProtos] with scalapb.HasBuilder[caffe2.prof_dag.ProfDAGProtos] {
  implicit def messageCompanion: scalapb.GeneratedMessageCompanion[caffe2.prof_dag.ProfDAGProtos] with scalapb.HasBuilder[caffe2.prof_dag.ProfDAGProtos] = this
  def merge(`_message__`: caffe2.prof_dag.ProfDAGProtos, `_input__`: _root_.com.google.protobuf.CodedInputStream): caffe2.prof_dag.ProfDAGProtos = newBuilder(_message__).merge(_input__).result()
  implicit def messageReads: _root_.scalapb.descriptors.Reads[caffe2.prof_dag.ProfDAGProtos] = _root_.scalapb.descriptors.Reads{
    case _root_.scalapb.descriptors.PMessage(__fieldsMap) =>
      _root_.scala.Predef.require(__fieldsMap.keys.forall(_.containingMessage == scalaDescriptor), "FieldDescriptor does not match message type.")
      caffe2.prof_dag.ProfDAGProtos(
        stats = __fieldsMap.get(scalaDescriptor.findFieldByNumber(1).get).map(_.as[_root_.scala.Seq[caffe2.prof_dag.ProfDAGProto]]).getOrElse(_root_.scala.Seq.empty),
        netName = __fieldsMap.get(scalaDescriptor.findFieldByNumber(2).get).flatMap(_.as[_root_.scala.Option[_root_.scala.Predef.String]]),
        opsStats = __fieldsMap.get(scalaDescriptor.findFieldByNumber(3).get).map(_.as[_root_.scala.Seq[caffe2.prof_dag.OpProfile]]).getOrElse(_root_.scala.Seq.empty)
      )
    case _ => throw new RuntimeException("Expected PMessage")
  }
  def javaDescriptor: _root_.com.google.protobuf.Descriptors.Descriptor = ProfDagProtoCompanion.javaDescriptor.getMessageTypes().get(3)
  def scalaDescriptor: _root_.scalapb.descriptors.Descriptor = ProfDagProtoCompanion.scalaDescriptor.messages(3)
  def messageCompanionForFieldNumber(__number: _root_.scala.Int): _root_.scalapb.GeneratedMessageCompanion[_] = {
    var __out: _root_.scalapb.GeneratedMessageCompanion[_] = null
    (__number: @_root_.scala.unchecked) match {
      case 1 => __out = caffe2.prof_dag.ProfDAGProto
      case 3 => __out = caffe2.prof_dag.OpProfile
    }
    __out
  }
  lazy val nestedMessagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]] = Seq.empty
  def enumCompanionForFieldNumber(__fieldNumber: _root_.scala.Int): _root_.scalapb.GeneratedEnumCompanion[_] = throw new MatchError(__fieldNumber)
  lazy val defaultInstance = caffe2.prof_dag.ProfDAGProtos(
    stats = _root_.scala.Seq.empty,
    netName = _root_.scala.None,
    opsStats = _root_.scala.Seq.empty
  )
  final class Builder private (
    private val __stats: _root_.scala.collection.immutable.VectorBuilder[caffe2.prof_dag.ProfDAGProto],
    private var __netName: _root_.scala.Option[_root_.scala.Predef.String],
    private val __opsStats: _root_.scala.collection.immutable.VectorBuilder[caffe2.prof_dag.OpProfile],
    private var `_unknownFields__`: _root_.scalapb.UnknownFieldSet.Builder
  ) extends _root_.scalapb.MessageBuilder[caffe2.prof_dag.ProfDAGProtos] {
    def merge(`_input__`: _root_.com.google.protobuf.CodedInputStream): this.type = {
      var _done__ = false
      while (!_done__) {
        val _tag__ = _input__.readTag()
        _tag__ match {
          case 0 => _done__ = true
          case 10 =>
            __stats += _root_.scalapb.LiteParser.readMessage[caffe2.prof_dag.ProfDAGProto](_input__)
          case 18 =>
            __netName = Option(_input__.readStringRequireUtf8())
          case 26 =>
            __opsStats += _root_.scalapb.LiteParser.readMessage[caffe2.prof_dag.OpProfile](_input__)
          case tag =>
            if (_unknownFields__ == null) {
              _unknownFields__ = new _root_.scalapb.UnknownFieldSet.Builder()
            }
            _unknownFields__.parseField(tag, _input__)
        }
      }
      this
    }
    def result(): caffe2.prof_dag.ProfDAGProtos = {
      caffe2.prof_dag.ProfDAGProtos(
        stats = __stats.result(),
        netName = __netName,
        opsStats = __opsStats.result(),
        unknownFields = if (_unknownFields__ == null) _root_.scalapb.UnknownFieldSet.empty else _unknownFields__.result()
      )
    }
  }
  object Builder extends _root_.scalapb.MessageBuilderCompanion[caffe2.prof_dag.ProfDAGProtos, caffe2.prof_dag.ProfDAGProtos.Builder] {
    def apply(): Builder = new Builder(
      __stats = new _root_.scala.collection.immutable.VectorBuilder[caffe2.prof_dag.ProfDAGProto],
      __netName = _root_.scala.None,
      __opsStats = new _root_.scala.collection.immutable.VectorBuilder[caffe2.prof_dag.OpProfile],
      `_unknownFields__` = null
    )
    def apply(`_message__`: caffe2.prof_dag.ProfDAGProtos): Builder = new Builder(
        __stats = new _root_.scala.collection.immutable.VectorBuilder[caffe2.prof_dag.ProfDAGProto] ++= _message__.stats,
        __netName = _message__.netName,
        __opsStats = new _root_.scala.collection.immutable.VectorBuilder[caffe2.prof_dag.OpProfile] ++= _message__.opsStats,
        `_unknownFields__` = new _root_.scalapb.UnknownFieldSet.Builder(_message__.unknownFields)
    )
  }
  def newBuilder: Builder = caffe2.prof_dag.ProfDAGProtos.Builder()
  def newBuilder(`_message__`: caffe2.prof_dag.ProfDAGProtos): Builder = caffe2.prof_dag.ProfDAGProtos.Builder(_message__)
  implicit class ProfDAGProtosLens[UpperPB](_l: _root_.scalapb.lenses.Lens[UpperPB, caffe2.prof_dag.ProfDAGProtos]) extends _root_.scalapb.lenses.ObjectLens[UpperPB, caffe2.prof_dag.ProfDAGProtos](_l) {
    def stats: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Seq[caffe2.prof_dag.ProfDAGProto]] = field(_.stats)((c_, f_) => c_.copy(stats = f_))
    def netName: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Predef.String] = field(_.getNetName)((c_, f_) => c_.copy(netName = Option(f_)))
    def optionalNetName: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Option[_root_.scala.Predef.String]] = field(_.netName)((c_, f_) => c_.copy(netName = f_))
    def opsStats: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Seq[caffe2.prof_dag.OpProfile]] = field(_.opsStats)((c_, f_) => c_.copy(opsStats = f_))
  }
  final val STATS_FIELD_NUMBER = 1
  final val NET_NAME_FIELD_NUMBER = 2
  final val OPS_STATS_FIELD_NUMBER = 3
  def of(
    stats: _root_.scala.Seq[caffe2.prof_dag.ProfDAGProto],
    netName: _root_.scala.Option[_root_.scala.Predef.String],
    opsStats: _root_.scala.Seq[caffe2.prof_dag.OpProfile]
  ): _root_.caffe2.prof_dag.ProfDAGProtos = _root_.caffe2.prof_dag.ProfDAGProtos(
    stats,
    netName,
    opsStats
  )
  // @@protoc_insertion_point(GeneratedMessageCompanion[caffe2.ProfDAGProtos])
}
