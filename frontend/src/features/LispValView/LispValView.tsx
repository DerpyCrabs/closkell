import Value from '../../components/Value'
import { FocusedLispVal } from '../../types'

export default function LispValView({ val }: { val: FocusedLispVal }) {
  return <Value val={val} level={0} />
}
