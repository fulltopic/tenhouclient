package conn.msgs

import org.deeplearning4j.gym.StepReply
import org.nd4j.linalg.api.ndarray.INDArray

class ActionResponse(val reply: StepReply[INDArray], val action: Int, val tile: Int, val isReach: Boolean = false, val isTourEnd: Boolean = false) {
}
