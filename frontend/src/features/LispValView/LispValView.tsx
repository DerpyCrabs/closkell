import React from 'react'
import LispVal from '../../components/LispVal'
import { FocusedLispVal } from '../../types'

export default function LispValView({ val }: { val: FocusedLispVal }) {
  return <LispVal val={val} />
}
