import React from 'react'
import { FocusedLispVal } from '../types'

function selectColor(number: number) {
  const hue = number * 137.508 // use golden angle approximation
  return `hsl(${hue},50%,75%)`
}

function LispValComponent({
  val,
  level,
}: {
  val: FocusedLispVal
  level: number
}) {
  if (val.type === 'list') {
    return (
      <span title={val.type}>
        <span style={{ color: selectColor(level) }}>(</span>
        {val.value.map((v: FocusedLispVal, i: number) => (
          <span key={`${level}-${i}`}>
            {i !== 0 && ' '}
            <FocusedLispValComponent val={v} level={level + 1} />
            {i !== val.value.length - 1 && ' '}
          </span>
        ))}
        <span style={{ color: selectColor(level) }}>)</span>
      </span>
    )
  } else if (val.type === 'dotted-list') {
    return <span>DottedList</span>
  } else if (val.type === 'func') {
    return <span title={val.type}>func</span>
  } else if (val.type === 'macro') {
    return <span>macro</span>
  } else if (val.type === 'unit') {
    return <span>unit</span>
  } else {
    return <span title={val.type}>{val.value}</span>
  }
}

export default function FocusedLispValComponent({
  val,
  level,
}: {
  val: FocusedLispVal
  level: number
}) {
  if (val.focused) {
    return (
      <span
        style={{
          backgroundColor: '#224698aa',
          fontSize: '22px',
          fontFamily: "'Roboto Mono', monospace",
        }}
      >
        <LispValComponent val={val} level={level} />
      </span>
    )
  } else {
    return (
      <span
        style={{
          fontFamily: "'Roboto Mono', monospace",
          fontSize: '22px',
        }}
      >
        <LispValComponent val={val} level={level} />
      </span>
    )
  }
}
