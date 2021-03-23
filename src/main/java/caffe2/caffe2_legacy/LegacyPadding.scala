// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO2

package caffe2.caffe2_legacy

sealed abstract class LegacyPadding(val value: _root_.scala.Int) extends _root_.scalapb.GeneratedEnum {
  type EnumType = LegacyPadding
  def isNotset: _root_.scala.Boolean = false
  def isValid: _root_.scala.Boolean = false
  def isSame: _root_.scala.Boolean = false
  def isCaffeLegacyPooling: _root_.scala.Boolean = false
  def companion: _root_.scalapb.GeneratedEnumCompanion[LegacyPadding] = caffe2.caffe2_legacy.LegacyPadding
  final def asRecognized: _root_.scala.Option[caffe2.caffe2_legacy.LegacyPadding.Recognized] = if (isUnrecognized) _root_.scala.None else _root_.scala.Some(this.asInstanceOf[caffe2.caffe2_legacy.LegacyPadding.Recognized])
}

object LegacyPadding extends _root_.scalapb.GeneratedEnumCompanion[LegacyPadding] {
  sealed trait Recognized extends LegacyPadding
  implicit def enumCompanion: _root_.scalapb.GeneratedEnumCompanion[LegacyPadding] = this
  /** Do not use old-stype padding strategies.
    */
  @SerialVersionUID(0L)
  case object NOTSET extends LegacyPadding(0) with LegacyPadding.Recognized {
    val index = 0
    val name = "NOTSET"
    override def isNotset: _root_.scala.Boolean = true
  }
  
  /** VALID and SAME are two strategies adopted in Google DistBelief: it forces
    * the input shape as follows. For SAME, the output is:
    *   R_out = ceil(float(R) / float(S))
    *   C_out = ceil(float(C) / float(S))
    * where R and C are row and column, S is the stride, and K is the kernel.
    * The number of padded pixels is then computed as
    *   Pr = ((R_out - 1) * S + K - R)
    *   Pc = ((C_out - 1) * S + K - C)
    * When Pr and Pc are even numbers, both sides (left and right, or top and
    * bottom) get half each. When Pr and Pc are odd numbers, the right and the
    * bottom gets the one additional padding pixel.
    * For VALID, padding values of 0 are always used.
    */
  @SerialVersionUID(0L)
  case object VALID extends LegacyPadding(1) with LegacyPadding.Recognized {
    val index = 1
    val name = "VALID"
    override def isValid: _root_.scala.Boolean = true
  }
  
  @SerialVersionUID(0L)
  case object SAME extends LegacyPadding(2) with LegacyPadding.Recognized {
    val index = 2
    val name = "SAME"
    override def isSame: _root_.scala.Boolean = true
  }
  
  /** CAFFE_LEGACY_POOLING is a flag that notifies the code to use the old Caffe
    * padding strategy.
    * Basically, in caffe2, after padding the convolution and pooling use the
    * same computation strategy: half-windows at the right and bottom are
    * discarded. In Caffe, convolution follows this strategy but if there are
    * some pixels in the half-windows, the pooling layer will actually put one
    * additional output. If you set LegacyPadding to this, we will compute the
    * equivalent padding strategy in caffe2 so that the output size is
    * backward compatible with Caffe.
    * THIS IS NOW DEPRECATED. ANY non-conventional use has to be manually
    * converted.
    */
  @SerialVersionUID(0L)
  case object CAFFE_LEGACY_POOLING extends LegacyPadding(3) with LegacyPadding.Recognized {
    val index = 3
    val name = "CAFFE_LEGACY_POOLING"
    override def isCaffeLegacyPooling: _root_.scala.Boolean = true
  }
  
  @SerialVersionUID(0L)
  final case class Unrecognized(unrecognizedValue: _root_.scala.Int) extends LegacyPadding(unrecognizedValue) with _root_.scalapb.UnrecognizedEnum
  
  lazy val values = scala.collection.immutable.Seq(NOTSET, VALID, SAME, CAFFE_LEGACY_POOLING)
  def fromValue(__value: _root_.scala.Int): LegacyPadding = __value match {
    case 0 => NOTSET
    case 1 => VALID
    case 2 => SAME
    case 3 => CAFFE_LEGACY_POOLING
    case __other => Unrecognized(__other)
  }
  def javaDescriptor: _root_.com.google.protobuf.Descriptors.EnumDescriptor = Caffe2LegacyProto.javaDescriptor.getEnumTypes().get(0)
  def scalaDescriptor: _root_.scalapb.descriptors.EnumDescriptor = Caffe2LegacyProto.scalaDescriptor.enums(0)
}