import React from 'react'
import AppBar from './components/AppBar'
import Eval from './features/Eval/Eval'

export default function App() {
  return (
    <div style={{ height: '100vh', display: 'flex', flexDirection: 'column' }}>
      <AppBar />
      <Eval />
    </div>
  )
}
