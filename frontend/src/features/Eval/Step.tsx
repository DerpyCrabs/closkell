import React from 'react'
import { Box } from '@material-ui/core'
import {
  FocusedLispVal,
  LVZipper,
  LispError,
  LispVal,
  Result,
} from '../../types'
import LispValView from '../LispValView/LispValView'

export default function Step({
  value,
  index,
  step,
}: {
  step: Result<LispError, LVZipper>
  value: number
  index: number
}) {
  return (
    <div
      style={{ flex: 1, overflowY: 'scroll' }}
      role='tabpanel'
      hidden={value !== index}
    >
      {value === index && (
        <Box p={3}>
          {step.Right !== undefined && (
            <LispValView val={evalOutputTransform(step.Right)} />
          )}
          {step.Left !== undefined && (
            <div
              style={{
                whiteSpace: 'pre-wrap',
                fontFamily: "'Roboto Mono', monospace",
              }}
            >
              {step.Left.error}
            </div>
          )}
        </Box>
      )}
    </div>
  )
}

const evalOutputTransform = (step: LVZipper): FocusedLispVal => {
  let val = {
    ...traverseLispVal((cr: LispVal) => ({ ...cr, focused: false, env: [] }))(
      step[1]
    ),
    env: step[0],
    focused: true,
  }
  step[2].forEach((crumb) => {
    val = {
      focused: false,
      env: crumb.env,
      type: 'list',
      value: [
        ...crumb.ls.map(
          traverseLispVal((cr: LispVal) => ({ ...cr, focused: false, env: [] }))
        ),
        val,
        ...crumb.rs.map(
          traverseLispVal((cr: LispVal) => ({ ...cr, focused: false, env: [] }))
        ),
      ],
    }
  })
  return val
}

const traverseLispVal = (f: any) => (val: any) => {
  if (val.type === 'list') {
    return { ...f(val), value: val.value.map(traverseLispVal(f)).map(f) }
  } else {
    return f(val)
  }
}
