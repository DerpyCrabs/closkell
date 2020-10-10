import React from 'react'
import { Fab } from '@material-ui/core'
import RunIcon from '@material-ui/icons/PlayArrow'

export default function CodeInput({
  code,
  setCode,
}: {
  code: string
  setCode: (s: string) => void
}) {
  const [currentCode, setCurrentCode] = React.useState(code)
  return (
    <div style={{ display: 'flex', width: '40%' }}>
      <textarea
        value={currentCode}
        onChange={(e) => setCurrentCode(e.target.value)}
        style={{
          width: '100%',
          backgroundColor: '#333',
          color: '#eee',
          fontSize: '22px',
          padding: '16px 16px',
        }}
      />
      <Fab
        color='secondary'
        style={{ position: 'fixed', right: 'calc(60% + 10px)', top: '36px' }}
        onClick={() => setCode(currentCode)}
      >
        <RunIcon />
      </Fab>
    </div>
  )
}
