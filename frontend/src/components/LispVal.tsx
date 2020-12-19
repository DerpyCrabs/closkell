import React from 'react'
import { FocusedLispVal } from '../types'

function selectColor(number: number) {
  const hue = number * 137.508 // use golden angle approximation
  return `hsl(${hue},50%,75%)`
}

function LetVal({
  values,
  level,
}: {
  values: Array<FocusedLispVal>
  level: number
}) {
  return (
    <>
      {values.map((v: FocusedLispVal, i: number) => (
        <span key={`${level}-${i}`}>
          {i !== 0 && (
            <>
              <br />
            </>
          )}
          <span style={{ paddingLeft: i !== 0 ? '32px' : '0px' }}>
            <FocusedLispValComponent val={v} level={level + 1} />
          </span>
        </span>
      ))}
    </>
  )
}

function LispValComponent({
  val,
  level,
}: {
  val: FocusedLispVal
  level: number
}) {
  if (val.type === 'list' || val.type === 'call') {
    return (
      <span title={val.type}>
        <span style={{ color: selectColor(level) }}>
          {val.type === 'list' ? '[' : '('}
        </span>
        {val.value.length !== 0 &&
        val.value[0].type === 'atom' &&
        val.value[0].value === 'let' ? (
          <LetVal values={val.value} level={level + 1} />
        ) : (
          val.value.map((v: FocusedLispVal, i: number) => (
            <span key={`${level}-${i}`}>
              {i !== 0 && ' '}
              <FocusedLispValComponent val={v} level={level + 1} />
              {i !== val.value.length - 1 && ' '}
            </span>
          ))
        )}
        <span style={{ color: selectColor(level) }}>
          {val.type === 'list' ? ']' : ')'}
        </span>
      </span>
    )
  } else if (val.type === 'dotted-list') {
    return (
      <span title={val.type}>
        <span style={{ color: selectColor(level) }}>[</span>
        {val.head.map((v: FocusedLispVal, i: number) => (
          <span key={`${level}-${i}`}>
            {i !== 0 && ' '}
            <FocusedLispValComponent val={v} level={level + 1} />{' '}
          </span>
        ))}
        {'. '}
        <FocusedLispValComponent val={val.tail} level={level + 1} />
        <span style={{ color: selectColor(level) }}>]</span>
      </span>
    )
  } else if (val.type === 'func') {
    return <span title={val.type}>func</span>
  } else if (val.type === 'macro') {
    return <span>macro</span>
  } else if (val.type === 'unit') {
    return <span>unit</span>
  } else if (val.type === 'bool') {
    return <span>{JSON.stringify(val.value)}</span>
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
