import React from 'react'
import { FocusedLispVal } from '../types'

function LispValComponent({ val }: { val: FocusedLispVal }) {
  if (val.type === 'list') {
    return (
      <span title={val.type}>
        (
        {val.value.map((v: FocusedLispVal) => (
          <span>
            {' '}
            <FocusedLispValComponent val={v} />{' '}
          </span>
        ))}
        )
      </span>
    )
  } else if (val.type === 'dotted-list') {
    return <span>DottedList</span>
  } else if (val.type === 'func') {
    return <span title={val.type}>func</span>
  } else if (val.type === 'macro') {
    return <span>macro</span>
  } else {
    return <span title={val.type}>{val.value}</span>
  }
}

export default function FocusedLispValComponent({
  val,
}: {
  val: FocusedLispVal
}) {
  if (val.focused) {
    return (
      <span
        style={{
          backgroundColor: '#3256a8aa',
          fontSize: '22px',
          fontFamily: "'Roboto Mono', monospace",
        }}
      >
        <LispValComponent val={val} />
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
        <LispValComponent val={val} />
      </span>
    )
  }
}
