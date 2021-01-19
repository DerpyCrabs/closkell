import React from 'react'
import { Box } from '@material-ui/core'
import { Error, FocusedLispVal, Result } from '../../types'
import LispValView from '../LispValView/LispValView'

export default function Step({
  value,
  index,
  step,
}: {
  step: Result<Error, FocusedLispVal>
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
          {step.Right !== undefined && <LispValView val={step.Right} />}
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
