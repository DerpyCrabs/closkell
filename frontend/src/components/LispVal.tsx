import React from 'react'
import {
  FocusedLispVal,
  FocusedValPath,
  LispVal as LispValType,
} from '../types'

function selectColor(number: number) {
  const hue = number * 137.508 // use golden angle approximation
  return `hsl(${hue},50%,75%)`
}

function LetVal({
  values,
  focusedPath,
  level,
}: {
  values: Array<LispValType>
  focusedPath: FocusedValPath | null
  level: number
}) {
  return (
    <>
      {values.map((v: LispValType, i: number) => (
        <span key={`${level}-${i}`}>
          {i !== 0 && (
            <>
              <br />
            </>
          )}
          <span style={{ paddingLeft: i !== 0 ? '32px' : '0px' }}>
            <FocusedLispValComponent
              val={
                focusedPath !== null && focusedPath[0] === i
                  ? [v, focusedPath.slice(1)]
                  : [v, null]
              }
              level={level + 1}
            />
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
  if (
    val[0].type === 'list' ||
    val[0].type === 'map' ||
    val[0].type === 'call'
  ) {
    const value = val[0].value
    return (
      <span title={val[0].type}>
        <span style={{ color: selectColor(level) }}>
          {val[0].type === 'list' ? '[' : val[0].type === 'map' ? '{' : '('}
        </span>
        {value.length !== 0 &&
        value[0].type === 'atom' &&
        value[0].value === 'let' ? (
          <LetVal
            values={val[0].value}
            focusedPath={val[1]}
            level={level + 1}
          />
        ) : (
          value.map((v: LispValType, i: number) => (
            <span key={`${level}-${i}`}>
              {i !== 0 && ' '}
              <FocusedLispValComponent
                val={
                  val[1] !== null && val[1][0] === i
                    ? [v, val[1].slice(1)]
                    : [v, null]
                }
                level={level + 1}
              />
              {i !== value.length - 1 && ' '}
            </span>
          ))
        )}
        <span style={{ color: selectColor(level) }}>
          {val[0].type === 'list' ? ']' : val[0].type === 'map' ? '}' : ')'}
        </span>
      </span>
    )
  } else if (val[0].type === 'dotted-list') {
    return (
      <span title={val[0].type}>
        <span style={{ color: selectColor(level) }}>[</span>
        {val[0].head.map((v: LispValType, i: number) => (
          <span key={`${level}-${i}`}>
            {i !== 0 && ' '}
            <FocusedLispValComponent val={[v, null]} level={level + 1} />{' '}
          </span>
        ))}
        {'. '}
        <FocusedLispValComponent val={[val[0].tail, null]} level={level + 1} />
        <span style={{ color: selectColor(level) }}>]</span>
      </span>
    )
  } else if (val[0].type === 'func') {
    return <span title={val[0].type}>func</span>
  } else if (val[0].type === 'macro') {
    return <span>macro</span>
  } else if (val[0].type === 'unit') {
    return <span>unit</span>
  } else if (val[0].type === 'bool') {
    return <span>{JSON.stringify(val[0].value)}</span>
  } else {
    return <span title={val[0].type}>{val[0].value}</span>
  }
}

export default function FocusedLispValComponent({
  val,
  level,
}: {
  val: FocusedLispVal
  level: number
}) {
  if (val[1] !== null && val[1].length === 0) {
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
