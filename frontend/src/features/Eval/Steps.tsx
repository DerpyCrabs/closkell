import React from 'react'
import { Tab, Tabs } from '@material-ui/core'

export default function Steps({
  steps,
  selected,
  setSelected,
}: {
  steps: Array<any>
  selected: number
  setSelected: (i: number) => void
}) {
  return (
    <Tabs
      orientation='vertical'
      variant='scrollable'
      value={selected}
      onChange={(e, v) => setSelected(v)}
      style={{ borderRight: '2px solid #222' }}
    >
      {steps.map((step: any, i: number) => (
        <Tab label={`Step ${i + 1}`} key={i} />
      ))}
    </Tabs>
  )
}
