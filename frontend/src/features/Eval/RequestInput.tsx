import React from 'react'
import {
  Checkbox,
  Fab,
  FormControlLabel,
  FormGroup,
  Paper,
} from '@material-ui/core'
import RunIcon from '@material-ui/icons/PlayArrow'
import { EvalRequest } from '../../types'

export default function EvalRequestInput({
  request,
  setRequest,
}: {
  request: EvalRequest
  setRequest: (s: EvalRequest) => void
}) {
  const [currentCode, setCurrentCode] = React.useState(request.expression)
  const [typeCheck, setTypeCheck] = React.useState(request.typeCheck)
  const [macroExpand, setMacroExpand] = React.useState(request.macroExpand)
  return (
    <div style={{ display: 'flex', width: '40%', flexDirection: 'column' }}>
      <textarea
        value={currentCode}
        onChange={(e) => setCurrentCode(e.target.value)}
        style={{
          width: '100%',
          backgroundColor: '#303030',
          border: 0,
          color: '#eee',
          fontSize: '22px',
          padding: '16px 16px',
          flex: 1,
          resize: 'none',
        }}
      />
      <Fab
        color='secondary'
        style={{
          position: 'fixed',
          right: 'calc(60% + 10px)',
          top: '36px',
          zIndex: 999999,
        }}
        onClick={() =>
          setRequest({ typeCheck, macroExpand, expression: currentCode })
        }
      >
        <RunIcon />
      </Fab>
      <Paper elevation={3} style={{ paddingLeft: '16px' }}>
        <FormGroup row>
          <FormControlLabel
            control={
              <Checkbox
                checked={macroExpand}
                onChange={(_) => setMacroExpand((m) => !m)}
              />
            }
            label='Expand macros'
          />
          <FormControlLabel
            control={
              <Checkbox
                checked={typeCheck}
                onChange={(_) => setTypeCheck((m) => !m)}
              />
            }
            label='Check types'
          />
        </FormGroup>
      </Paper>
    </div>
  )
}
